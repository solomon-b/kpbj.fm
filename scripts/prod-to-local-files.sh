#!/usr/bin/env bash
#
# Copy Production S3 Files to Local Dev
# Downloads the production Tigris S3 bucket to local filesystem at /tmp/kpbj.
#
# Usage: ./scripts/prod-to-local-files.sh <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>
#
# Arguments:
#   PROD_AWS_ACCESS_KEY_ID      - Production Tigris access key ID
#   PROD_AWS_SECRET_ACCESS_KEY  - Production Tigris secret access key
#

set -euo pipefail

# Configuration
PROD_BUCKET="production-kpbj-storage"
TIGRIS_ENDPOINT="https://fly.storage.tigris.dev"
LOCAL_STORAGE_ROOT="/tmp/kpbj"

# Parse arguments
if [ $# -lt 2 ]; then
  echo "Usage: $0 <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>"
  exit 1
fi

PROD_AWS_ACCESS_KEY_ID="$1"
PROD_AWS_SECRET_ACCESS_KEY="$2"

echo "========================================"
echo "Production to Local Dev Files Copy"
echo "========================================"
echo ""
echo "This will download production S3 files to: $LOCAL_STORAGE_ROOT"
echo "Existing files will be replaced (using --delete)."
echo ""

read -p "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

# Create local storage directory if it doesn't exist
mkdir -p "$LOCAL_STORAGE_ROOT"

echo ""
echo "Downloading from production bucket..."
echo "  Bucket: $PROD_BUCKET"
echo "  Destination: $LOCAL_STORAGE_ROOT"
echo "  Using key: ${PROD_AWS_ACCESS_KEY_ID:0:12}..."

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
echo "Files copy complete!"
echo "========================================"
echo ""
echo "  Location: $LOCAL_STORAGE_ROOT"
echo "  Files: $FILE_COUNT"
echo "  Size: $TOTAL_SIZE"
echo ""
