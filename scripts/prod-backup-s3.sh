#!/usr/bin/env bash
#
# Production S3 Backup
#
# Mirrors the production DO Spaces bucket to a local directory using rclone.
# Only downloads new or changed files (compared by size and checksum).
# Credentials loaded from SOPS-encrypted secrets/backup.yaml.
#
# Optional environment variables:
#   BACKUP_DIR  - Where to store the mirror (default: ./backups/s3)
#
# Usage: ./scripts/prod-backup-s3.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Config
BACKUP_DIR="${BACKUP_DIR:-./backups/s3}"

echo "[$(date -Iseconds)] Loading credentials from SOPS..."
AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)

# Check for required tools
command -v rclone >/dev/null 2>&1 || { echo "Error: rclone is not installed" >&2; exit 1; }

# Create temporary rclone config
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
acl = private
EOF

mkdir -p "$BACKUP_DIR"

echo "[$(date -Iseconds)] Starting S3 backup..."
echo "[$(date -Iseconds)] Bucket: $PROD_BUCKET"
echo "[$(date -Iseconds)] Destination: $BACKUP_DIR"

rclone sync \
    --config "$RCLONE_CONFIG" \
    --progress \
    --stats-one-line \
    --stats 30s \
    --transfers 8 \
    --checkers 16 \
    --checksum \
    "spaces:${PROD_BUCKET}" \
    "$BACKUP_DIR"

# Report stats
FILE_COUNT=$(find "$BACKUP_DIR" -type f | wc -l)
TOTAL_SIZE=$(du -sh "$BACKUP_DIR" | cut -f1)
echo "[$(date -Iseconds)] S3 backup complete: $FILE_COUNT files, $TOTAL_SIZE"
