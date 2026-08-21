#!/usr/bin/env bash
#
# Generate Staging Placeholder Audio
# Reads every audio path from the staging database and writes a duration-matched
# placeholder tone to that key. Production audio is never copied or read.
#
# Credentials are loaded from SOPS-encrypted secrets.
#
# Usage: ./scripts/prod-to-staging-audio.sh [--replace] [--prune] [--dry-run]
#   --replace  rewrite keys that already exist. Needed once, for the initial
#              migration away from the real MP3s.
#   --prune    delete staging audio keys that no database row references
#   --dry-run  print what would happen and write nothing
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "$SCRIPT_DIR/lib/common.sh"
# shellcheck source=lib/gen-audio.sh
source "$SCRIPT_DIR/lib/gen-audio.sh"

REPLACE=""
PRUNE=""

for arg in "$@"; do
  case "$arg" in
    --replace) REPLACE="1" ;;
    --prune) PRUNE="1" ;;
    --dry-run) export GEN_AUDIO_DRY_RUN="1" ;;
    *) echo "Unknown option: $arg" >&2; exit 1 ;;
  esac
done

for tool in ffmpeg aws psql; do
  command -v "$tool" >/dev/null 2>&1 || {
    echo "ERROR: '$tool' is required but not installed."
    exit 1
  }
done

echo "========================================"
echo "Staging Placeholder Audio"
echo "========================================"
echo ""
if [ -n "$REPLACE" ]; then
  echo "  --replace: every audio key will be rewritten."
fi
if [ -n "$PRUNE" ]; then
  echo "  --prune: staging audio keys with no database row will be deleted."
fi
if [ -n "${GEN_AUDIO_DRY_RUN:-}" ]; then
  echo "  --dry-run: nothing will be written."
fi
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
STAGING_DB_PASSWORD=$(sops -d --extract '["db_password"]' secrets/staging-web.yaml)
export STAGING_AWS_ACCESS_KEY_ID
export STAGING_AWS_SECRET_ACCESS_KEY
STAGING_AWS_ACCESS_KEY_ID=$(load_secret staging aws_access_key_id)
STAGING_AWS_SECRET_ACCESS_KEY=$(load_secret staging aws_secret_access_key)

WORK_FILE=$(mktemp)
EXISTING_FILE=$(mktemp)
STAGING_TUNNEL_PID=""

cleanup() {
  rm -f "$WORK_FILE" "$EXISTING_FILE"
  if [ -n "$STAGING_TUNNEL_PID" ]; then
    kill "$STAGING_TUNNEL_PID" 2>/dev/null || true
  fi
}
trap cleanup EXIT

open_ssh_tunnel "$STAGING_PROXY_PORT" "$STAGING_VPS_TARGET"
STAGING_TUNNEL_PID=$TUNNEL_PID

echo "Reading the work list from the staging database..."
gen_audio_work_list \
  "postgres://$STAGING_DB_USER:$STAGING_DB_PASSWORD@localhost:$STAGING_PROXY_PORT/$STAGING_DB_NAME" \
  > "$WORK_FILE"

echo "Listing staging audio keys..."
gen_audio_staging_keys > "$EXISTING_FILE"

gen_audio_generate "$WORK_FILE" "$EXISTING_FILE" "$REPLACE" "$PRUNE"

echo ""
echo "========================================"
echo "Placeholder audio complete!"
echo "========================================"
