#!/usr/bin/env bash
#
# Copy Production S3 Files to Local Dev
# Downloads images/ from the production S3 bucket to /tmp/kpbj, as the staging
# sync does. Each audio path in the local database gets a placeholder tone of the
# same length, as on staging. Load the database first with prod-to-local-db.
#
# Credentials are loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-to-local-files.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=lib/common.sh
source "$SCRIPT_DIR/lib/common.sh"
# shellcheck source=lib/gen-audio.sh
source "$SCRIPT_DIR/lib/gen-audio.sh"

echo "========================================"
echo "Production to Local Dev Files Copy"
echo "========================================"
echo ""
echo "This will download production images to: $LOCAL_STORAGE_ROOT/images"
echo "Audio files become placeholder tones, made from the local database."
echo "Existing files will be replaced (using --delete)."
echo ""

read -p "Type 'yes' to confirm: " CONFIRM
if [ "$CONFIRM" != "yes" ]; then
  echo "Aborted."
  exit 1
fi

echo "Loading credentials from SOPS..."
PROD_AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
PROD_AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)

# Create local storage directory if it doesn't exist
mkdir -p "$LOCAL_STORAGE_ROOT"

echo ""
echo "Downloading images from production bucket..."
echo "  Bucket: $PROD_BUCKET"
echo "  Destination: $LOCAL_STORAGE_ROOT"
echo "  Using key: ${PROD_AWS_ACCESS_KEY_ID:0:12}..."

# Download images/ from production (with --delete to mirror exactly), as the
# staging sync does. The rest of the bucket is audio, under audio/ and the
# staged uploads in temp/, so the placeholders below replace it.
AWS_ACCESS_KEY_ID="$PROD_AWS_ACCESS_KEY_ID" \
AWS_SECRET_ACCESS_KEY="$PROD_AWS_SECRET_ACCESS_KEY" \
aws s3 sync "s3://$PROD_BUCKET/images" "$LOCAL_STORAGE_ROOT/images" \
  --endpoint-url "$PROD_ENDPOINT" \
  --delete

generate_local_audio

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
