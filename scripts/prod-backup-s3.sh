#!/bin/bash
# Remote production S3 backup script using rclone with snapshots.
#
# Creates date-based snapshots of the production Tigris S3 bucket.
# Uses hardlinks between snapshots to deduplicate unchanged files (rsync-style).
#
# Required environment variables:
#   AWS_ACCESS_KEY_ID      - Production Tigris access key ID
#   AWS_SECRET_ACCESS_KEY  - Production Tigris secret access key
#
# Optional environment variables:
#   BACKUP_DIR         - Where to store backups (default: ./backups/s3)
#   RETENTION_DAYS     - Days to keep snapshots (default: 14)
#   BUCKET_NAME        - S3 bucket name (default: production-kpbj-storage)
#
# Usage:
#   export AWS_ACCESS_KEY_ID="..."
#   export AWS_SECRET_ACCESS_KEY="..."
#   ./scripts/prod-backup-s3.sh
#
# TrueNAS cron example (4 AM daily, after DB backup):
#   0 4 * * * /path/to/prod-backup-s3.sh >> /var/log/kpbj-s3-backup.log 2>&1
#
# Rclone config:
#   This script creates a temporary rclone config. Alternatively, create a
#   permanent config with: rclone config create tigris s3 provider=Other ...

set -euo pipefail

# Config
BACKUP_DIR="${BACKUP_DIR:-./backups/s3}"
RETENTION_DAYS="${RETENTION_DAYS:-14}"
BUCKET_NAME="${BUCKET_NAME:-production-kpbj-storage}"
TIGRIS_ENDPOINT="https://fly.storage.tigris.dev"

# Validate required env vars
if [[ -z "${AWS_ACCESS_KEY_ID:-}" ]]; then
    echo "Error: AWS_ACCESS_KEY_ID environment variable is required" >&2
    exit 1
fi

if [[ -z "${AWS_SECRET_ACCESS_KEY:-}" ]]; then
    echo "Error: AWS_SECRET_ACCESS_KEY environment variable is required" >&2
    exit 1
fi

# Check for required tools
command -v rclone >/dev/null 2>&1 || { echo "Error: rclone is not installed" >&2; exit 1; }

# Setup
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
DATE_DIR=$(date +%Y-%m-%d)
SNAPSHOT_DIR="${BACKUP_DIR}/${DATE_DIR}"
LATEST_LINK="${BACKUP_DIR}/latest"
RCLONE_CONFIG=""

cleanup() {
    if [[ -n "$RCLONE_CONFIG" && -f "$RCLONE_CONFIG" ]]; then
        rm -f "$RCLONE_CONFIG"
    fi
}
trap cleanup EXIT

mkdir -p "$BACKUP_DIR"

echo "[$(date -Iseconds)] Starting S3 backup..."
echo "[$(date -Iseconds)] Bucket: $BUCKET_NAME"
echo "[$(date -Iseconds)] Destination: $SNAPSHOT_DIR"

# Create temporary rclone config
RCLONE_CONFIG=$(mktemp)
cat > "$RCLONE_CONFIG" <<EOF
[tigris]
type = s3
provider = Other
access_key_id = ${AWS_ACCESS_KEY_ID}
secret_access_key = ${AWS_SECRET_ACCESS_KEY}
endpoint = ${TIGRIS_ENDPOINT}
acl = private
EOF

# Find the most recent backup for hardlink reference
LINK_DEST_OPTS=""
if [[ -L "$LATEST_LINK" && -d "$LATEST_LINK" ]]; then
    PREVIOUS_BACKUP=$(readlink -f "$LATEST_LINK")
    if [[ -d "$PREVIOUS_BACKUP" ]]; then
        echo "[$(date -Iseconds)] Using previous backup for deduplication: $PREVIOUS_BACKUP"
        LINK_DEST_OPTS="--copy-dest=$PREVIOUS_BACKUP"
    fi
fi

# Create snapshot directory
mkdir -p "$SNAPSHOT_DIR"

# Run rclone sync
echo "[$(date -Iseconds)] Starting rclone sync..."
rclone sync \
    --config "$RCLONE_CONFIG" \
    --progress \
    --stats-one-line \
    --stats 30s \
    --transfers 8 \
    --checkers 16 \
    $LINK_DEST_OPTS \
    "tigris:${BUCKET_NAME}" \
    "$SNAPSHOT_DIR"

# Update latest symlink
rm -f "$LATEST_LINK"
ln -s "$SNAPSHOT_DIR" "$LATEST_LINK"

# Calculate stats
FILE_COUNT=$(find "$SNAPSHOT_DIR" -type f | wc -l)
TOTAL_SIZE=$(du -sh "$SNAPSHOT_DIR" | cut -f1)
echo "[$(date -Iseconds)] Snapshot complete: $FILE_COUNT files, $TOTAL_SIZE"

# Cleanup old snapshots
echo "[$(date -Iseconds)] Cleaning up snapshots older than $RETENTION_DAYS days..."
DELETED=0
while IFS= read -r -d '' dir; do
    if [[ -d "$dir" ]]; then
        rm -rf "$dir"
        echo "[$(date -Iseconds)] Deleted: $dir"
        ((DELETED++)) || true
    fi
done < <(find "$BACKUP_DIR" -maxdepth 1 -type d -name "20*" -mtime +"$RETENTION_DAYS" -print0)

if [[ "$DELETED" -gt 0 ]]; then
    echo "[$(date -Iseconds)] Deleted $DELETED old snapshot(s)"
fi

# Report total backup size
TOTAL_BACKUP_SIZE=$(du -sh "$BACKUP_DIR" | cut -f1)
SNAPSHOT_COUNT=$(find "$BACKUP_DIR" -maxdepth 1 -type d -name "20*" | wc -l)
echo "[$(date -Iseconds)] Total backups: $SNAPSHOT_COUNT snapshots, $TOTAL_BACKUP_SIZE"

echo "[$(date -Iseconds)] S3 backup complete"
