#!/usr/bin/env bash
#
# Upload a local file to the production DO Spaces bucket.
#
# The object is public, as the audio that Liquidsoap downloads must be. An
# object that is already at the key is replaced.
#
# Usage: ./scripts/prod-upload-s3-object.sh <local-file> <dest-key>
#
# Example:
#   ./scripts/prod-upload-s3-object.sh \
#     ./kpbj-filler.mp3 \
#     audio/filler/kpbj-filler.mp3

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

if [ $# -ne 2 ]; then
  echo "Usage: $0 <local-file> <dest-key>" >&2
  exit 1
fi

LOCAL_FILE="$1"
DEST_KEY="$2"

[ -f "$LOCAL_FILE" ] || { echo "Error: $LOCAL_FILE is not a file" >&2; exit 1; }

echo "[$(date -Iseconds)] Loading credentials from SOPS..."
AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)

command -v rclone >/dev/null 2>&1 || { echo "Error: rclone is not installed" >&2; exit 1; }

RCLONE_CONFIG=$(mktemp)
chmod 600 "$RCLONE_CONFIG"
cleanup() { rm -f "$RCLONE_CONFIG"; }
trap cleanup EXIT

cat > "$RCLONE_CONFIG" <<EOF
[spaces]
type = s3
provider = Other
access_key_id = ${AWS_ACCESS_KEY_ID}
secret_access_key = ${AWS_SECRET_ACCESS_KEY}
endpoint = ${PROD_ENDPOINT}
acl = public-read
no_check_bucket = true
EOF

echo "[$(date -Iseconds)] Uploading: ${LOCAL_FILE} -> ${DEST_KEY}"
rclone copyto \
    --config "$RCLONE_CONFIG" \
    --progress \
    "$LOCAL_FILE" \
    "spaces:${PROD_BUCKET}/${DEST_KEY}"

echo "[$(date -Iseconds)] Verifying destination exists..."
rclone ls \
    --config "$RCLONE_CONFIG" \
    "spaces:${PROD_BUCKET}/${DEST_KEY}"

echo "[$(date -Iseconds)] Done. Public URL: https://${PROD_BUCKET}.${PROD_ENDPOINT#https://}/${DEST_KEY}"
