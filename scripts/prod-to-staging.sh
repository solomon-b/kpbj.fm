#!/usr/bin/env bash
#
# Copy Production to Staging (Database + S3)
# Copies both the production database and S3 bucket to staging.
# Database: PII is sanitized for User and Host accounts.
# S3: Complete replacement of staging bucket.
#
# Usage: ./scripts/prod-to-staging.sh <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>
#
# Arguments:
#   PROD_AWS_ACCESS_KEY_ID      - Production Tigris access key ID
#   PROD_AWS_SECRET_ACCESS_KEY  - Production Tigris secret access key
#
# Required environment variables:
#   PROD_DB_PASSWORD       - Production database password
#   STAGING_DB_PASSWORD    - Staging database password
#   AWS_ACCESS_KEY_ID      - Staging Tigris access key ID
#   AWS_SECRET_ACCESS_KEY  - Staging Tigris secret access key
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Parse arguments
if [ $# -lt 2 ]; then
  echo "Usage: $0 <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>"
  exit 1
fi

PROD_AWS_ACCESS_KEY_ID="$1"
PROD_AWS_SECRET_ACCESS_KEY="$2"

echo "========================================"
echo "Production to Staging Full Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the staging environment with production data."
echo "  - Database: PII sanitized for User/Host accounts"
echo "  - S3 Bucket: Complete replacement"
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

# Validate all required credentials
if [ -z "${PROD_DB_PASSWORD:-}" ]; then
  echo "ERROR: PROD_DB_PASSWORD environment variable is not set."
  echo "Add to your .envrc.local: export PROD_DB_PASSWORD=\"<password>\""
  exit 1
fi
if [ -z "${STAGING_DB_PASSWORD:-}" ]; then
  echo "ERROR: STAGING_DB_PASSWORD environment variable is not set."
  exit 1
fi
if [ -z "${AWS_ACCESS_KEY_ID:-}" ]; then
  echo "ERROR: AWS_ACCESS_KEY_ID environment variable is not set (for staging)."
  exit 1
fi
if [ -z "${AWS_SECRET_ACCESS_KEY:-}" ]; then
  echo "ERROR: AWS_SECRET_ACCESS_KEY environment variable is not set (for staging)."
  exit 1
fi

# Store staging S3 credentials before they might be overwritten
STAGING_AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID"
STAGING_AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY"

echo ""
echo "========================================"
echo "  STEP 1/2: Copying Database"
echo "========================================"

echo ""
echo "Starting database proxies..."

# Start production proxy
fly proxy "$PROD_PROXY_PORT:5432" -a "$PROD_DB_APP" &
PROD_PROXY_PID=$!

# Start staging proxy
fly proxy "$STAGING_PROXY_PORT:5432" -a "$STAGING_DB_APP" &
STAGING_PROXY_PID=$!

# Cleanup function for proxies
cleanup_proxies() {
  echo "Cleaning up proxies..."
  kill "$PROD_PROXY_PID" 2>/dev/null || true
  kill "$STAGING_PROXY_PID" 2>/dev/null || true
}
trap cleanup_proxies EXIT

sleep 3

echo "Stopping staging app to release database connections..."
fly scale count 0 --app "$STAGING_APP" --yes

echo "Dropping staging database..."
psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/postgres" \
  -c "DROP DATABASE IF EXISTS $STAGING_DB_NAME WITH (FORCE);"

echo "Creating staging database..."
psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/postgres" \
  -c "CREATE DATABASE $STAGING_DB_NAME;"

echo "Copying production database to staging..."
pg_dump "postgres://$PROD_DB_USER:$PROD_DB_PASSWORD@localhost:$PROD_PROXY_PORT/$PROD_DB_NAME" \
  | psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME"

echo "Sanitizing PII for User and Host accounts..."
psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME" \
  -v password_hash="'$SANITIZED_PASSWORD_HASH'" \
  -f "$SCRIPT_DIR/lib/sanitize-pii.sql"

# Close database proxies before S3 copy
cleanup_proxies

echo ""
echo "========================================"
echo "  STEP 2/2: Copying S3 Bucket"
echo "========================================"

# Create temp directory for download
TMPDIR=$(mktemp -d)
trap 'rm -rf "$TMPDIR"; cleanup_proxies' EXIT

echo ""
echo "Downloading from production bucket..."
echo "  Bucket: $PROD_BUCKET"

# Download from production using prod credentials
AWS_ACCESS_KEY_ID="$PROD_AWS_ACCESS_KEY_ID" \
AWS_SECRET_ACCESS_KEY="$PROD_AWS_SECRET_ACCESS_KEY" \
aws s3 sync "s3://$PROD_BUCKET" "$TMPDIR" \
  --endpoint-url "$TIGRIS_ENDPOINT"

FILE_COUNT=$(find "$TMPDIR" -type f | wc -l)
echo "  Downloaded $FILE_COUNT files"

echo ""
echo "Uploading to staging bucket..."
echo "  Bucket: $STAGING_BUCKET"

# Upload to staging using staging credentials (with --delete to remove extra files)
AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
aws s3 sync "$TMPDIR" "s3://$STAGING_BUCKET" \
  --endpoint-url "$TIGRIS_ENDPOINT" \
  --delete

echo ""
echo "Restarting staging app..."
fly scale count 1 --app "$STAGING_APP" --yes

echo ""
echo "========================================"
echo "Production copied to staging successfully!"
echo "========================================"
echo ""
echo "Database:"
echo "  - User/Host accounts: email=userN@example.com, password=hunter2"
echo "  - Staff/Admin accounts: unchanged (use real credentials)"
echo ""
echo "S3 Bucket:"
echo "  - Files available at: https://$STAGING_BUCKET.fly.storage.tigris.dev/..."
echo ""
