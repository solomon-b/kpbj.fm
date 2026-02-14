#!/usr/bin/env bash
#
# Copy Production Database to Staging
# Copies the production PostgreSQL database to staging with PII sanitization.
# User and Host accounts have their email, password, and names anonymized.
# Staff and Admin accounts retain their real credentials.
#
# Both databases live on VPS hosts, accessed via SSH tunnel.
# Credentials are loaded from SOPS-encrypted secrets.
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
PROD_READONLY_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/prod-web.yaml)
STAGING_DB_PASSWORD=$(sops -d --extract '["db_password"]' secrets/staging-web.yaml)

# Cleanup function
cleanup() {
  echo "Cleaning up..."
  kill "$PROD_TUNNEL_PID" 2>/dev/null || true
  kill "$STAGING_TUNNEL_PID" 2>/dev/null || true
  ssh "$STAGING_VPS_TARGET" systemctl start kpbj-web 2>/dev/null || true
}
trap cleanup EXIT

echo ""
open_ssh_tunnel "$PROD_PROXY_PORT" "$PROD_VPS_TARGET"
PROD_TUNNEL_PID=$TUNNEL_PID

open_ssh_tunnel "$STAGING_PROXY_PORT" "$STAGING_VPS_TARGET"
STAGING_TUNNEL_PID=$TUNNEL_PID

echo "Stopping staging web service to release database connections..."
ssh "$STAGING_VPS_TARGET" systemctl stop kpbj-web

echo "Dropping and recreating staging database..."
ssh "$STAGING_VPS_TARGET" "sudo -u postgres psql -c \"DROP DATABASE IF EXISTS $STAGING_DB_NAME WITH (FORCE);\" && sudo -u postgres psql -c \"CREATE DATABASE $STAGING_DB_NAME OWNER $STAGING_DB_USER;\""

echo "Copying production database to staging..."
pg_dump "postgres://$PROD_READONLY_USER:$PROD_READONLY_PASSWORD@localhost:$PROD_PROXY_PORT/$PROD_DB_NAME" \
  | psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME"

echo "Sanitizing PII for User and Host accounts..."
psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME" \
  -v password_hash="'$SANITIZED_PASSWORD_HASH'" \
  -f "$SCRIPT_DIR/lib/sanitize-pii.sql"

echo "Starting staging web service..."
ssh "$STAGING_VPS_TARGET" systemctl start kpbj-web

echo ""
echo "========================================"
echo "Database copy complete!"
echo "========================================"
echo ""
echo "  User/Host accounts: email=userN@example.com, password=hunter2"
echo "  Staff/Admin accounts: unchanged (use real credentials)"
echo ""
