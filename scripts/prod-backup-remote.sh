#!/bin/bash
# Remote production database backup script for TrueNAS or other external servers.
#
# Connects to the production DigitalOcean VPS via SSH tunnel to run pg_dump.
#
# Required environment variables:
#   PROD_SSH_TARGET    - SSH target for production VPS (e.g., root@stream.kpbj.fm)
#   PROD_DB_PASSWORD   - Production database password
#
# Optional environment variables:
#   BACKUP_DIR         - Where to store backups (default: ./backups/postgres)
#   RETENTION_DAYS     - Days to keep backups (default: 14)
#   LOCAL_PORT         - Local port for SSH tunnel (default: 15433)
#
# Usage:
#   export PROD_SSH_TARGET="root@stream.kpbj.fm"
#   export PROD_DB_PASSWORD="..."
#   ./scripts/prod-backup-remote.sh
#
# TrueNAS cron example (3 AM daily):
#   0 3 * * * /path/to/prod-backup-remote.sh >> /var/log/kpbj-backup.log 2>&1

set -euo pipefail

# Config
BACKUP_DIR="${BACKUP_DIR:-./backups/postgres}"
RETENTION_DAYS="${RETENTION_DAYS:-14}"
DB_NAME="kpbj_fm"
DB_USER="kpbj_fm"
LOCAL_PORT="${LOCAL_PORT:-15433}"

# Validate required env vars
if [[ -z "${PROD_SSH_TARGET:-}" ]]; then
    echo "Error: PROD_SSH_TARGET environment variable is required (e.g., root@stream.kpbj.fm)" >&2
    exit 1
fi

if [[ -z "${PROD_DB_PASSWORD:-}" ]]; then
    echo "Error: PROD_DB_PASSWORD environment variable is required" >&2
    exit 1
fi

# Check for required tools
command -v ssh >/dev/null 2>&1 || { echo "Error: ssh is not installed" >&2; exit 1; }
command -v pg_dump >/dev/null 2>&1 || { echo "Error: pg_dump is not installed" >&2; exit 1; }

# Setup
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/kpbj_fm_${TIMESTAMP}.dump"
TUNNEL_PID=""

cleanup() {
    if [[ -n "$TUNNEL_PID" ]]; then
        kill "$TUNNEL_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

mkdir -p "$BACKUP_DIR"

echo "[$(date -Iseconds)] Starting backup..."

# Kill any stale process on the tunnel port
STALE_PID=$(lsof -ti:"$LOCAL_PORT" 2>/dev/null || true)
if [[ -n "$STALE_PID" ]]; then
    echo "[$(date -Iseconds)] Killing stale process on port $LOCAL_PORT (PID $STALE_PID)..."
    kill "$STALE_PID" 2>/dev/null || true
    sleep 1
fi

# Start SSH tunnel to production PostgreSQL
echo "[$(date -Iseconds)] Opening SSH tunnel to $PROD_SSH_TARGET..."
ssh -N -L "$LOCAL_PORT:127.0.0.1:5432" "$PROD_SSH_TARGET" &
TUNNEL_PID=$!
sleep 3

# Verify tunnel is running
if ! kill -0 "$TUNNEL_PID" 2>/dev/null; then
    echo "Error: SSH tunnel failed to start" >&2
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
