#!/usr/bin/env bash
#
# Copy Production S3 Bucket to Staging
# Copies the production Tigris S3 bucket to staging, completely replacing staging data.
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

# Configuration
PROD_BUCKET="production-kpbj-storage"
STAGING_BUCKET="staging-kpbj-storage"
TIGRIS_ENDPOINT="https://fly.storage.tigris.dev"

# Parse arguments
if [ $# -lt 2 ]; then
  echo "Usage: $0 <PROD_AWS_ACCESS_KEY_ID> <PROD_AWS_SECRET_ACCESS_KEY>"
  exit 1
fi

PROD_AWS_ACCESS_KEY_ID="$1"
PROD_AWS_SECRET_ACCESS_KEY="$2"

echo "========================================"
echo "Production to Staging S3 Copy"
echo "========================================"
echo ""
echo "This will COMPLETELY REPLACE the staging S3 bucket with production data."
echo ""

read -p "Type 'yes' to confirm: " CONFIRM
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

# Store staging credentials
STAGING_AWS_ACCESS_KEY_ID="$AWS_ACCESS_KEY_ID"
STAGING_AWS_SECRET_ACCESS_KEY="$AWS_SECRET_ACCESS_KEY"

# Create temp directory for download
TMPDIR=$(mktemp -d)
trap "rm -rf $TMPDIR" EXIT

echo ""
echo "Downloading from production bucket..."
echo "  Bucket: $PROD_BUCKET"
echo "  Using key: ${PROD_AWS_ACCESS_KEY_ID:0:12}..."

# Download from production using prod credentials
AWS_ACCESS_KEY_ID="$PROD_AWS_ACCESS_KEY_ID" \
AWS_SECRET_ACCESS_KEY="$PROD_AWS_SECRET_ACCESS_KEY" \
aws s3 sync "s3://$PROD_BUCKET" "$TMPDIR" \
  --endpoint-url "$TIGRIS_ENDPOINT"

FILE_COUNT=$(find "$TMPDIR" -type f | wc -l)
echo "  Downloaded $FILE_COUNT files"

echo ""
echo "Uploading to staging bucket..."
echo "  Bucket: $STAGING_BUCKET"
echo "  Using key: ${STAGING_AWS_ACCESS_KEY_ID:0:12}..."

# Upload to staging using staging credentials (with --delete to remove extra files)
AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
aws s3 sync "$TMPDIR" "s3://$STAGING_BUCKET" \
  --endpoint-url "$TIGRIS_ENDPOINT" \
  --delete

echo ""
echo "========================================"
echo "S3 copy complete!"
echo "========================================"
echo ""
echo "  Files available at: https://$STAGING_BUCKET.fly.storage.tigris.dev/..."
echo ""
