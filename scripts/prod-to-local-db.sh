#!/usr/bin/env bash
#
# Copy Production Database to Local Dev
# Copies the production PostgreSQL database to local development with PII sanitization.
# User and Host accounts have their email, password, and names anonymized.
# Staff and Admin accounts retain their real credentials.
#
# Usage: ./scripts/prod-to-local-db.sh
#
# Required environment variables:
#   PROD_DB_PASSWORD  - Production database password
#
# Prerequisites:
#   - Local PostgreSQL running (just postgres-dev-start)
#   - flyctl installed and authenticated
#

set -euo pipefail

# Configuration
PROD_DB_APP="kpbj-postgres"
PROD_DB_NAME="kpbj_fm"
PROD_DB_USER="kpbj_fm"
PROD_PROXY_PORT="15433"

# Local dev configuration (matches Justfile)
LOCAL_DB_PORT="5433"
LOCAL_DB_NAME="dev_db"
LOCAL_DB_USER="postgres"

# Argon2 hash for "hunter2" - used to sanitize passwords
SANITIZED_PASSWORD_HASH='$argon2id$v=19$m=65536,t=2,p=1$bndaU0ZXRzYvRHRRbU9ubmM0TGsyQT09$12Dlzkd8oZ5CBlVuUivYSGbPL1M4nfTEIXb56hS+FVo'

echo "========================================"
echo "Production to Local Dev Database Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the local dev database with production data."
echo "  - User and Host accounts: PII will be sanitized"
echo "  - Staff and Admin accounts: credentials unchanged"
echo ""

read -p "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

# Validate environment variables
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
echo "Starting production database proxy..."

# Start production proxy
fly proxy "$PROD_PROXY_PORT:5432" -a "$PROD_DB_APP" &
PROD_PROXY_PID=$!

# Cleanup function
cleanup() {
  echo "Cleaning up proxy..."
  kill "$PROD_PROXY_PID" 2>/dev/null || true
}
trap cleanup EXIT

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

echo ""
echo "========================================"
echo "Database copy complete!"
echo "========================================"
echo ""
echo "  User/Host accounts: email=userN@example.com, password=hunter2"
echo "  Staff/Admin accounts: unchanged (use real credentials)"
echo ""
