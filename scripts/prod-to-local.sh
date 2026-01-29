#!/usr/bin/env bash
#
# Copy Production to Local Dev (Database + Files)
# Copies both the production database and S3 bucket to local development.
# Database: PII is sanitized for User and Host accounts.
# Files: Downloaded to /tmp/kpbj.
#
# Usage: ./scripts/prod-to-local.sh <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>
#
# Arguments:
#   PROD_AWS_ACCESS_KEY_ID      - Production Tigris access key ID
#   PROD_AWS_SECRET_ACCESS_KEY  - Production Tigris secret access key
#
# Required environment variables:
#   PROD_DB_PASSWORD  - Production database password
#
# Prerequisites:
#   - Local PostgreSQL running (just postgres-dev-start)
#   - flyctl installed and authenticated
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Configuration
PROD_DB_APP="kpbj-postgres"
PROD_DB_NAME="kpbj_fm"
PROD_DB_USER="kpbj_fm"
PROD_PROXY_PORT="15433"
PROD_BUCKET="production-kpbj-storage"
TIGRIS_ENDPOINT="https://fly.storage.tigris.dev"

# Local dev configuration
LOCAL_DB_PORT="5433"
LOCAL_DB_NAME="dev_db"
LOCAL_DB_USER="postgres"
LOCAL_STORAGE_ROOT="/tmp/kpbj"

# Argon2 hash for "hunter2" - used to sanitize passwords
SANITIZED_PASSWORD_HASH='$argon2id$v=19$m=65536,t=2,p=1$bndaU0ZXRzYvRHRRbU9ubmM0TGsyQT09$12Dlzkd8oZ5CBlVuUivYSGbPL1M4nfTEIXb56hS+FVo'

# Parse arguments
if [ $# -lt 2 ]; then
  echo "Usage: $0 <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>"
  exit 1
fi

PROD_AWS_ACCESS_KEY_ID="$1"
PROD_AWS_SECRET_ACCESS_KEY="$2"

echo "========================================"
echo "Production to Local Dev Full Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the local dev environment with production data."
echo "  - Database: PII sanitized for User/Host accounts"
echo "  - Files: Downloaded to $LOCAL_STORAGE_ROOT"
echo ""

read -p "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

# Validate required credentials
if [ -z "${PROD_DB_PASSWORD:-}" ]; then
  echo "ERROR: PROD_DB_PASSWORD environment variable is not set."
  echo "Add to your .envrc.local: export PROD_DB_PASSWORD=\"<password>\""
  exit 1
fi

# Check if local postgres is running
if ! pg_isready -h localhost -p "$LOCAL_DB_PORT" -q; then
  echo "ERROR: Local PostgreSQL is not running on port $LOCAL_DB_PORT."
  echo "Start it with: just postgres-dev-start"
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

sleep 3

echo "Dropping local database..."
psql -h localhost -p "$LOCAL_DB_PORT" -U "$LOCAL_DB_USER" -d postgres \
  -c "DROP DATABASE IF EXISTS $LOCAL_DB_NAME WITH (FORCE);"

echo "Creating local database..."
psql -h localhost -p "$LOCAL_DB_PORT" -U "$LOCAL_DB_USER" -d postgres \
  -c "CREATE DATABASE $LOCAL_DB_NAME;"

echo "Copying production database to local..."
pg_dump "postgres://$PROD_DB_USER:$PROD_DB_PASSWORD@localhost:$PROD_PROXY_PORT/$PROD_DB_NAME" \
  | psql -h localhost -p "$LOCAL_DB_PORT" -U "$LOCAL_DB_USER" -d "$LOCAL_DB_NAME"

echo "Sanitizing PII for User and Host accounts..."
psql -h localhost -p "$LOCAL_DB_PORT" -U "$LOCAL_DB_USER" -d "$LOCAL_DB_NAME" <<SANITIZE_SQL
  UPDATE users u
  SET
    email = 'user' || u.id || '@example.com',
    password = '$SANITIZED_PASSWORD_HASH'
  FROM user_metadata um
  WHERE um.user_id = u.id
    AND um.user_role IN ('User', 'Host');

  UPDATE user_metadata
  SET
    display_name = 'user' || user_id,
    full_name = 'Test User ' || user_id
  WHERE user_role IN ('User', 'Host');

  TRUNCATE server_sessions;
SANITIZE_SQL

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
  --endpoint-url "$TIGRIS_ENDPOINT" \
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
