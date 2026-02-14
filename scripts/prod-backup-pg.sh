#!/usr/bin/env bash
#
# Backup Production Database to Local File
# Creates a pg_dump of the production database via SSH tunnel.
#
# Credentials are loaded from SOPS-encrypted secrets.
#
# Usage: ./scripts/prod-backup-db.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

echo "Loading credentials from SOPS..."
PROD_READONLY_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/prod-web.yaml)

mkdir -p backups/postgres
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
BACKUP_FILE="backups/postgres/kpbj_fm_${TIMESTAMP}.dump"

echo "🗄️  Backing up production database..."
open_ssh_tunnel "$PROD_PROXY_PORT" "$PROD_VPS_TARGET"

cleanup() {
  kill "$TUNNEL_PID" 2>/dev/null || true
}
trap cleanup EXIT

echo "   Running pg_dump..."
PGPASSWORD="$PROD_READONLY_PASSWORD" pg_dump -h localhost -p "$PROD_PROXY_PORT" -U "$PROD_READONLY_USER" -Fc "$PROD_DB_NAME" > "$BACKUP_FILE"

echo "✨ Backup saved to: $BACKUP_FILE ($(du -h "$BACKUP_FILE" | cut -f1))"
