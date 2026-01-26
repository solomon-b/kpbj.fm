#!/usr/bin/env bash
#
# Copy Production Database to Staging
# Copies the production PostgreSQL database to staging with PII sanitization.
# User and Host accounts have their email, password, and names anonymized.
# Staff and Admin accounts retain their real credentials.
#
# Usage: ./scripts/prod-to-staging-db.sh
#
# Required environment variables:
#   PROD_DB_PASSWORD     - Production database password
#   STAGING_DB_PASSWORD  - Staging database password
#

set -euo pipefail

# Configuration
PROD_DB_APP="kpbj-postgres"
PROD_DB_NAME="kpbj_fm"
PROD_DB_USER="kpbj_fm"
STAGING_APP="kpbj-fm-staging"
STAGING_DB_APP="kpbj-postgres-staging"
STAGING_DB_NAME="kpbj_fm_staging"
STAGING_DB_USER="postgres"
PROD_PROXY_PORT="15433"
STAGING_PROXY_PORT="15432"

# Argon2 hash for "hunter2" - used to sanitize passwords
SANITIZED_PASSWORD_HASH='$argon2id$v=19$m=65536,t=2,p=1$bndaU0ZXRzYvRHRRbU9ubmM0TGsyQT09$12Dlzkd8oZ5CBlVuUivYSGbPL1M4nfTEIXb56hS+FVo'

echo "========================================"
echo "Production to Staging Database Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the staging database with production data."
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
if [ -z "${STAGING_DB_PASSWORD:-}" ]; then
  echo "ERROR: STAGING_DB_PASSWORD environment variable is not set."
  exit 1
fi

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
psql "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME" <<SANITIZE_SQL
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
