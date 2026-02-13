#!/usr/bin/env bash
#
# Copy Production to Local Dev (Database + Files)
# Copies both the production database and S3 bucket to local development.
# Database: PII is sanitized for User and Host accounts.
# Files: Downloaded to /tmp/kpbj.
#
# Credentials are loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-to-local.sh
#
# Prerequisites:
#   - Local PostgreSQL running (just dev-postgres-start)
#   - flyctl installed and authenticated
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

echo "========================================"
echo "Production to Local Dev Full Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the local dev environment with production data."
echo "  - Database: PII sanitized for User/Host accounts"
echo "  - Files: Downloaded to $LOCAL_STORAGE_ROOT"
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
export PROD_DB_PASSWORD=$(load_secret prod db_password)
PROD_AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
PROD_AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)

# Check if local postgres is running
if ! pg_isready -h localhost -p "$DEV_DB_PORT" -q; then
  echo "ERROR: Local PostgreSQL is not running on port $DEV_DB_PORT."
  echo "Start it with: just dev-postgres-start"
  exit 1
fi

echo ""
echo "========================================"
echo "  STEP 1/2: Copying Database"
echo "========================================"

echo ""
echo "Starting production database proxy..."

# Start production proxy
fly proxy "$PROD_PROXY_PORT:5432" -a "$PROD_DB_APP" &
PROD_PROXY_PID=$!

# Cleanup function for proxy
cleanup_proxy() {
  echo "Cleaning up proxy..."
  kill "$PROD_PROXY_PID" 2>/dev/null || true
}
trap cleanup_proxy EXIT

sleep 3

echo "Dropping local database..."
psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d postgres \
  -c "DROP DATABASE IF EXISTS $LOCAL_DB_NAME WITH (FORCE);"

echo "Creating local database..."
psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d postgres \
  -c "CREATE DATABASE $LOCAL_DB_NAME;"

echo "Copying production database to local..."
pg_dump "postgres://$PROD_DB_USER:$PROD_DB_PASSWORD@localhost:$PROD_PROXY_PORT/$PROD_DB_NAME" \
  | psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d "$LOCAL_DB_NAME"

echo "Sanitizing PII for User and Host accounts..."
psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d "$LOCAL_DB_NAME" \
  -v password_hash="'$SANITIZED_PASSWORD_HASH'" \
  -f "$SCRIPT_DIR/lib/sanitize-pii.sql"

# Close database proxy before file copy
cleanup_proxy

echo ""
echo "========================================"
echo "  STEP 2/2: Copying Files"
echo "========================================"

# Create local storage directory
mkdir -p "$LOCAL_STORAGE_ROOT"

echo ""
echo "Downloading from production bucket..."
echo "  Bucket: $PROD_BUCKET"
echo "  Destination: $LOCAL_STORAGE_ROOT"

# Download from production (with --delete to mirror exactly)
AWS_ACCESS_KEY_ID="$PROD_AWS_ACCESS_KEY_ID" \
AWS_SECRET_ACCESS_KEY="$PROD_AWS_SECRET_ACCESS_KEY" \
aws s3 sync "s3://$PROD_BUCKET" "$LOCAL_STORAGE_ROOT" \
  --endpoint-url "$PROD_ENDPOINT" \
  --delete

FILE_COUNT=$(find "$LOCAL_STORAGE_ROOT" -type f | wc -l)
TOTAL_SIZE=$(du -sh "$LOCAL_STORAGE_ROOT" | cut -f1)

echo ""
echo "========================================"
echo "Production copied to local dev successfully!"
echo "========================================"
echo ""
echo "Database:"
echo "  - User/Host accounts: email=userN@example.com, password=hunter2"
echo "  - Staff/Admin accounts: unchanged (use real credentials)"
echo ""
echo "Files:"
echo "  - Location: $LOCAL_STORAGE_ROOT"
echo "  - Files: $FILE_COUNT"
echo "  - Size: $TOTAL_SIZE"
echo ""
