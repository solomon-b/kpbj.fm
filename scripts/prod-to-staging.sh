#!/usr/bin/env bash
#
# Copy Production to Staging (Database + Images + Placeholder Audio)
# Copies the production database and the images/ prefix of the production
# bucket to staging, then generates placeholder audio from the staging
# database. Production audio is never copied.
# Database: PII is sanitized for User and Host accounts.
#
# Both databases live on VPS hosts, accessed via SSH tunnel.
# Credentials are loaded from SOPS-encrypted secrets.
#
# Usage: ./scripts/prod-to-staging.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "$SCRIPT_DIR/lib/common.sh"
# shellcheck source=lib/sync-s3.sh
source "$SCRIPT_DIR/lib/sync-s3.sh"
# shellcheck source=lib/gen-audio.sh
source "$SCRIPT_DIR/lib/gen-audio.sh"

echo "========================================"
echo "Production to Staging Full Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the staging environment with production data."
echo "  - Database: PII sanitized for User/Host accounts"
echo "  - Images:   incremental sync (new/changed files only)"
echo "  - Audio:    placeholder tones generated from the staging database"
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
PROD_READONLY_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/prod-web.yaml)
STAGING_DB_PASSWORD=$(sops -d --extract '["db_password"]' secrets/staging-web.yaml)
export PROD_AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
export PROD_AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)
export STAGING_AWS_ACCESS_KEY_ID=$(load_secret staging aws_access_key_id)
export STAGING_AWS_SECRET_ACCESS_KEY=$(load_secret staging aws_secret_access_key)

echo ""
echo "========================================"
echo "  STEP 1/3: Copying Database"
echo "========================================"

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

# Close tunnels before S3 copy
kill "$PROD_TUNNEL_PID" 2>/dev/null || true
kill "$STAGING_TUNNEL_PID" 2>/dev/null || true

echo ""
echo "========================================"
echo "  STEP 2/3: Syncing Images"
echo "========================================"

sync_s3_buckets "images/"

echo ""
echo "========================================"
echo "  STEP 3/3: Generating Placeholder Audio"
echo "========================================"

open_ssh_tunnel "$STAGING_PROXY_PORT" "$STAGING_VPS_TARGET"
STAGING_TUNNEL_PID=$TUNNEL_PID

AUDIO_WORK_FILE=$(mktemp)
AUDIO_EXISTING_FILE=$(mktemp)

echo "Reading the work list from the staging database..."
gen_audio_work_list \
  "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME" \
  > "$AUDIO_WORK_FILE"

echo "Listing staging audio keys..."
gen_audio_staging_keys > "$AUDIO_EXISTING_FILE"

# A full refresh reloads the database, so any row that existed only on staging
# is gone and its object is an orphan. Prune them.
gen_audio_generate "$AUDIO_WORK_FILE" "$AUDIO_EXISTING_FILE" "" "1"

rm -f "$AUDIO_WORK_FILE" "$AUDIO_EXISTING_FILE"
kill "$STAGING_TUNNEL_PID" 2>/dev/null || true

echo ""
echo "Starting staging web service..."
ssh "$STAGING_VPS_TARGET" systemctl start kpbj-web

echo ""
echo "========================================"
echo "Production copied to staging successfully!"
echo "========================================"
echo ""
echo "Database:"
echo "  - User/Host accounts: email=userN@example.com, password=hunter2"
echo "  - Staff/Admin accounts: unchanged (use real credentials)"
echo ""
echo "Images:"
echo "  - Files available at: https://$STAGING_BUCKET.sfo3.digitaloceanspaces.com/images/..."
echo ""
echo "Audio:"
echo "  - Placeholder tones, 8 kbps mono, matching each row's duration"
echo "  - Run 'just prod-to-staging-audio --replace' to rewrite every key"
echo ""
