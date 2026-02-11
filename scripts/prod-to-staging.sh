#!/usr/bin/env bash
#
# Copy Production to Staging (Database + S3)
# Copies both the production database and S3 bucket to staging.
# Database: PII is sanitized for User and Host accounts.
# S3: Incremental sync — only copies new/changed files.
#
# Credentials are loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-to-staging.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "$SCRIPT_DIR/lib/common.sh"
# shellcheck source=lib/sync-s3.sh
source "$SCRIPT_DIR/lib/sync-s3.sh"

echo "========================================"
echo "Production to Staging Full Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the staging environment with production data."
echo "  - Database: PII sanitized for User/Host accounts"
echo "  - S3 Bucket: Incremental sync (new/changed files only)"
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
export PROD_DB_PASSWORD=$(load_secret prod db_password)
export STAGING_DB_PASSWORD=$(load_secret staging db_password)
export PROD_AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
export PROD_AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)
export STAGING_AWS_ACCESS_KEY_ID=$(load_secret staging aws_access_key_id)
export STAGING_AWS_SECRET_ACCESS_KEY=$(load_secret staging aws_secret_access_key)

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
echo "  STEP 2/2: Syncing S3 Bucket"
echo "========================================"

sync_s3_buckets

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
