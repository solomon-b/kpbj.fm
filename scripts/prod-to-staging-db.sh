#!/usr/bin/env bash
#
# Copy Production Database to Staging
# Copies the production PostgreSQL database to staging with PII sanitization.
# User and Host accounts have their email, password, and names anonymized.
# Staff and Admin accounts retain their real credentials.
#
# Credentials are loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-to-staging-db.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

echo "========================================"
echo "Production to Staging Database Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the staging database with production data."
echo "  - User and Host accounts: PII will be sanitized"
echo "  - Staff and Admin accounts: credentials unchanged"
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
export PROD_DB_PASSWORD=$(load_secret prod db_password)
export STAGING_DB_PASSWORD=$(load_secret staging db_password)

echo ""
echo "Starting database proxies..."

# Start production proxy
fly proxy "$PROD_PROXY_PORT:5432" -a "$PROD_DB_APP" &
PROD_PROXY_PID=$!

# Start staging proxy
fly proxy "$STAGING_PROXY_PORT:5432" -a "$STAGING_DB_APP" &
STAGING_PROXY_PID=$!

# Cleanup function
cleanup() {
  echo "Cleaning up proxies..."
  kill "$PROD_PROXY_PID" 2>/dev/null || true
  kill "$STAGING_PROXY_PID" 2>/dev/null || true
}
trap cleanup EXIT

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

echo "Restarting staging app..."
fly scale count 1 --app "$STAGING_APP" --yes

echo ""
echo "========================================"
echo "Database copy complete!"
echo "========================================"
echo ""
echo "  User/Host accounts: email=userN@example.com, password=hunter2"
echo "  Staff/Admin accounts: unchanged (use real credentials)"
echo ""
