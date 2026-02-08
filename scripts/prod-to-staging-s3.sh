#!/usr/bin/env bash
#
# Sync Production S3 Bucket to Staging (Incremental)
# Compares production and staging Tigris buckets by key and file size,
# then copies only new/changed files and removes stale ones.
#
# Usage: ./scripts/prod-to-staging-s3.sh <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>
#
# Arguments:
#   PROD_AWS_ACCESS_KEY_ID      - Production Tigris access key ID
#   PROD_AWS_SECRET_ACCESS_KEY  - Production Tigris secret access key
#
# Required environment variables:
#   AWS_ACCESS_KEY_ID      - Staging Tigris access key ID
#   AWS_SECRET_ACCESS_KEY  - Staging Tigris secret access key
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "$SCRIPT_DIR/lib/common.sh"
# shellcheck source=lib/sync-s3.sh
source "$SCRIPT_DIR/lib/sync-s3.sh"

# Parse arguments
if [ $# -lt 2 ]; then
  echo "Usage: $0 <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>"
  exit 1
fi

export PROD_AWS_ACCESS_KEY_ID="$1"
export PROD_AWS_SECRET_ACCESS_KEY="$2"

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

# Validate staging AWS credentials in environment
if [ -z "${AWS_ACCESS_KEY_ID:-}" ]; then
  echo "ERROR: AWS_ACCESS_KEY_ID environment variable is not set (for staging)."
  exit 1
fi
if [ -z "${AWS_SECRET_ACCESS_KEY:-}" ]; then
  echo "ERROR: AWS_SECRET_ACCESS_KEY environment variable is not set (for staging)."
  exit 1
fi

export STAGING_AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID"
export STAGING_AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY"

sync_s3_buckets

echo ""
echo "========================================"
echo "S3 sync complete!"
echo "========================================"
echo ""
echo "  Files available at: https://$STAGING_BUCKET.fly.storage.tigris.dev/..."
echo ""
