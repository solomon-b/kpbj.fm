#!/usr/bin/env bash
#
# Backup Production Database to Local File
# Creates a pg_dump of the production database via fly proxy.
#
# Credentials are loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-backup-db.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

echo "Loading credentials from SOPS..."
export PROD_DB_PASSWORD=$(load_secret prod db_password)

mkdir -p backups/postgres
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="backups/postgres/kpbj_fm_${TIMESTAMP}.dump"

echo "🗄️  Backing up production database..."
echo "   Starting proxy..."
fly proxy "$PROD_PROXY_PORT:5432" -a "$PROD_DB_APP" &
PROXY_PID=$!

cleanup() {
  kill "$PROXY_PID" 2>/dev/null || true
}
trap cleanup EXIT

sleep 2

echo "   Running pg_dump..."
PGPASSWORD="$PROD_DB_PASSWORD" pg_dump -h localhost -p "$PROD_PROXY_PORT" -U "$PROD_DB_USER" -Fc "$PROD_DB_NAME" > "$BACKUP_FILE"

echo "✨ Backup saved to: $BACKUP_FILE ($(du -h "$BACKUP_FILE" | cut -f1))"
