#!/usr/bin/env bash
#
# Sync Production S3 Bucket to Staging (Incremental)
# Compares production (Tigris) and staging (DO Spaces) buckets by key
# and file size, then copies only new/changed files and removes stale ones.
#
# Credentials are loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-to-staging-s3.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "$SCRIPT_DIR/lib/common.sh"
# shellcheck source=lib/sync-s3.sh
source "$SCRIPT_DIR/lib/sync-s3.sh"

echo "========================================"
echo "Production to Staging S3 Sync"
echo "========================================"
echo ""
echo "This will incrementally sync the staging S3 bucket with production."
echo "Only new/changed files will be copied; removed files will be deleted."
echo ""

read -rp "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
export PROD_AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
export PROD_AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)
export STAGING_AWS_ACCESS_KEY_ID=$(load_secret staging aws_access_key_id)
export STAGING_AWS_SECRET_ACCESS_KEY=$(load_secret staging aws_secret_access_key)

sync_s3_buckets

echo ""
echo "========================================"
echo "S3 sync complete!"
echo "========================================"
echo ""
echo "  Files available at: https://$STAGING_BUCKET.sfo3.digitaloceanspaces.com/..."
echo ""
