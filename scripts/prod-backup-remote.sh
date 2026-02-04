#!/bin/bash
# Remote production database backup script for TrueNAS or other external servers.
#
# Required environment variables:
#   FLY_API_TOKEN      - Fly.io API token (create with: fly tokens create deploy -a kpbj-postgres)
#   PROD_DB_PASSWORD   - Production database password
#
# Optional environment variables:
#   BACKUP_DIR         - Where to store backups (default: ./backups)
#   RETENTION_DAYS     - Days to keep backups (default: 14)
#
# Usage:
#   export FLY_API_TOKEN="..."
#   export PROD_DB_PASSWORD="..."
#   ./scripts/prod-backup-remote.sh
#
# TrueNAS cron example (3 AM daily):
#   0 3 * * * /path/to/prod-backup-remote.sh >> /var/log/kpbj-backup.log 2>&1

set -euo pipefail

# Config
BACKUP_DIR="${BACKUP_DIR:-./backups/postgres}"
RETENTION_DAYS="${RETENTION_DAYS:-14}"
DB_APP="kpbj-postgres"
DB_NAME="kpbj_fm"
DB_USER="kpbj_fm"
LOCAL_PORT="15432"

# Validate required env vars
if [[ -z "${FLY_API_TOKEN:-}" ]]; then
    echo "Error: FLY_API_TOKEN environment variable is required" >&2
    exit 1
fi

if [[ -z "${PROD_DB_PASSWORD:-}" ]]; then
    echo "Error: PROD_DB_PASSWORD environment variable is required" >&2
    exit 1
fi

# Check for required tools
command -v fly >/dev/null 2>&1 || { echo "Error: flyctl is not installed" >&2; exit 1; }
command -v pg_dump >/dev/null 2>&1 || { echo "Error: pg_dump is not installed" >&2; exit 1; }

# Setup
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/kpbj_fm_${TIMESTAMP}.dump"
PROXY_PID=""

cleanup() {
    if [[ -n "$PROXY_PID" ]]; then
        kill "$PROXY_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

mkdir -p "$BACKUP_DIR"

echo "[$(date -Iseconds)] Starting backup..."

# Start fly proxy
echo "[$(date -Iseconds)] Starting fly proxy..."
fly proxy "${LOCAL_PORT}:5432" -a "$DB_APP" &
PROXY_PID=$!
sleep 3

# Verify proxy is running
if ! kill -0 "$PROXY_PID" 2>/dev/null; then
    echo "Error: fly proxy failed to start" >&2
    exit 1
fi

# Run backup
echo "[$(date -Iseconds)] Running pg_dump..."
PGPASSWORD="$PROD_DB_PASSWORD" pg_dump \
    -h localhost \
    -p "$LOCAL_PORT" \
    -U "$DB_USER" \
    -Fc \
    "$DB_NAME" > "$BACKUP_FILE"

BACKUP_SIZE=$(du -h "$BACKUP_FILE" | cut -f1)
echo "[$(date -Iseconds)] Backup saved: $BACKUP_FILE ($BACKUP_SIZE)"

# Cleanup old backups
DELETED=$(find "$BACKUP_DIR" -name "kpbj_fm_*.dump" -mtime +"$RETENTION_DAYS" -print -delete | wc -l)
if [[ "$DELETED" -gt 0 ]]; then
    echo "[$(date -Iseconds)] Deleted $DELETED old backup(s)"
fi

echo "[$(date -Iseconds)] Backup complete"
