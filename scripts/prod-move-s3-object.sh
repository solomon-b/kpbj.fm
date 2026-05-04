#!/usr/bin/env bash
#
# Move an S3 object in the production DO Spaces bucket.
#
# Usage: ./scripts/prod-move-s3-object.sh <source-key> <dest-key>
#
# Example:
#   ./scripts/prod-move-s3-object.sh \
#     audio/episodes/2026/05/14/catbluz-radio-hour_2026-05-14_7bd727ff.mp3 \
#     audio/episodes/2026/04/09/catbluz-radio-hour_2026-04-09_7bd727ff.mp3

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

if [ $# -ne 2 ]; then
  echo "Usage: $0 <source-key> <dest-key>" >&2
  exit 1
fi

SOURCE_KEY="$1"
DEST_KEY="$2"

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

echo "[$(date -Iseconds)] Copying: ${SOURCE_KEY} -> ${DEST_KEY}"
rclone copyto \
    --config "$RCLONE_CONFIG" \
    --progress \
    "spaces:${PROD_BUCKET}/${SOURCE_KEY}" \
    "spaces:${PROD_BUCKET}/${DEST_KEY}"

echo "[$(date -Iseconds)] Verifying destination exists..."
rclone ls \
    --config "$RCLONE_CONFIG" \
    "spaces:${PROD_BUCKET}/${DEST_KEY}"

echo "[$(date -Iseconds)] Deleting old object: ${SOURCE_KEY}"
rclone deletefile \
    --config "$RCLONE_CONFIG" \
    "spaces:${PROD_BUCKET}/${SOURCE_KEY}"

echo "[$(date -Iseconds)] Done. Object moved successfully."
