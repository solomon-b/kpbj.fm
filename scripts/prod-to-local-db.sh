#!/usr/bin/env bash
#
# Copy Production Database to Local Dev
# Copies the production PostgreSQL database to local development with PII sanitization.
# User and Host accounts have their email, password, and names anonymized.
# Staff and Admin accounts retain their real credentials.
#
# Credentials are loaded from SOPS-encrypted secrets.
#
# Usage: ./scripts/prod-to-local-db.sh
#
# Prerequisites:
#   - Local PostgreSQL running (just dev-postgres-start)
#   - SSH access to production VPS
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

echo "========================================"
echo "Production to Local Dev Database Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the local dev database with production data."
echo "  - User and Host accounts: PII will be sanitized"
echo "  - Staff and Admin accounts: credentials unchanged"
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
PROD_READONLY_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/prod-web.yaml)

# Check if local postgres is running
if ! pg_isready -h localhost -p "$DEV_DB_PORT" -q; then
  echo "ERROR: Local PostgreSQL is not running on port $DEV_DB_PORT."
  echo "Start it with: just dev-postgres-start"
  exit 1
fi

echo ""
open_ssh_tunnel "$PROD_PROXY_PORT" "$PROD_VPS_TARGET"

# Cleanup function
cleanup() {
  echo "Cleaning up tunnel..."
  kill "$TUNNEL_PID" 2>/dev/null || true
}
trap cleanup EXIT

echo "Dropping local database..."
psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d postgres \
  -c "DROP DATABASE IF EXISTS $LOCAL_DB_NAME WITH (FORCE);"

echo "Creating local database..."
psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d postgres \
  -c "CREATE DATABASE $LOCAL_DB_NAME;"

echo "Copying production database to local..."
pg_dump "postgres://$PROD_READONLY_USER:$PROD_READONLY_PASSWORD@localhost:$PROD_PROXY_PORT/$PROD_DB_NAME" \
  | psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d "$LOCAL_DB_NAME"

echo "Sanitizing PII for User and Host accounts..."
psql -h localhost -p "$DEV_DB_PORT" -U "$LOCAL_DB_USER" -d "$LOCAL_DB_NAME" \
  -v password_hash="'$SANITIZED_PASSWORD_HASH'" \
  -f "$SCRIPT_DIR/lib/sanitize-pii.sql"

echo ""
echo "========================================"
echo "Database copy complete!"
echo "========================================"
echo ""
echo "  User/Host accounts: email=userN@example.com, password=hunter2"
echo "  Staff/Admin accounts: unchanged (use real credentials)"
echo ""
