#!/usr/bin/env bash
#
# Remote Production Database Backup
#
# Connects to the production VPS via SSH tunnel and runs pg_dump.
# Credentials loaded from SOPS-encrypted secrets/backup.yaml.
#
# Optional environment variables:
#   BACKUP_DIR         - Where to store backups (default: ./backups/postgres)
#   RETENTION_DAYS     - Days to keep backups (default: 14)
#
# Usage: ./scripts/prod-backup-remote.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

# Config
BACKUP_DIR="${BACKUP_DIR:-./backups/postgres}"
RETENTION_DAYS="${RETENTION_DAYS:-14}"

echo "[$(date -Iseconds)] Loading credentials from SOPS..."
PROD_READONLY_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/prod-web.yaml)

# Check for required tools
command -v pg_dump >/dev/null 2>&1 || { echo "Error: pg_dump is not installed" >&2; exit 1; }

# Setup
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="${BACKUP_DIR}/kpbj_fm_${TIMESTAMP}.dump"

mkdir -p "$BACKUP_DIR"

cleanup() {
    if [[ -n "${TUNNEL_PID:-}" ]]; then
        kill "$TUNNEL_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

echo "[$(date -Iseconds)] Starting backup..."
open_ssh_tunnel "$PROD_PROXY_PORT" "$PROD_VPS_TARGET"

# Verify tunnel is running
if ! kill -0 "$TUNNEL_PID" 2>/dev/null; then
    echo "Error: SSH tunnel failed to start" >&2
    exit 1
fi

# Run backup
echo "[$(date -Iseconds)] Running pg_dump..."
PGPASSWORD="$PROD_READONLY_PASSWORD" pg_dump \
    -h localhost \
    -p "$PROD_PROXY_PORT" \
    -U "$PROD_READONLY_USER" \
    -Fc \
    "$PROD_DB_NAME" > "$BACKUP_FILE"

BACKUP_SIZE=$(du -h "$BACKUP_FILE" | cut -f1)
echo "[$(date -Iseconds)] Backup saved: $BACKUP_FILE ($BACKUP_SIZE)"

# Cleanup old backups
DELETED=$(find "$BACKUP_DIR" -name "kpbj_fm_*.dump" -mtime +"$RETENTION_DAYS" -print -delete | wc -l)
if [[ "$DELETED" -gt 0 ]]; then
    echo "[$(date -Iseconds)] Deleted $DELETED old backup(s)"
fi

echo "[$(date -Iseconds)] Backup complete"
