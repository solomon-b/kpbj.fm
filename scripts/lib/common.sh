#!/usr/bin/env bash
#
# Common Shell Library
# Shared constants and functions for KPBJ scripts.
#
# Usage: source "$(dirname "${BASH_SOURCE[0]}")/lib/common.sh"
#

SECRETS_FILE="secrets/backup.yaml"

# Load a secret from the SOPS-encrypted backup secrets file.
# Usage: load_secret "prod" "db_password"
load_secret() {
  local section="$1"
  local key="$2"
  sops -d --extract "[\"${section}\"][\"${key}\"]" "$SECRETS_FILE"
}

# Open an SSH tunnel to a remote PostgreSQL instance.
# Kills any stale process on the port first.
# Usage: open_ssh_tunnel PORT TARGET
#   Sets TUNNEL_PID for the caller to clean up via trap.
open_ssh_tunnel() {
  local port="$1" target="$2"
  local stale_pid
  stale_pid=$(lsof -ti:"$port" 2>/dev/null || true)
  if [ -n "$stale_pid" ]; then
    echo "  Killing stale process on port $port (PID $stale_pid)..."
    kill "$stale_pid" 2>/dev/null || true
    sleep 1
  fi
  echo "Opening SSH tunnel on port $port..."
  ssh -N -L "$port:127.0.0.1:5432" "$target" &
  TUNNEL_PID=$!
  sleep 2
}

# Argon2 hash for "hunter2" - used to sanitize passwords in non-production databases
# Password: hunter2
# shellcheck disable=SC2016  # Literal $ signs in Argon2 hash, not variables
export SANITIZED_PASSWORD_HASH='$argon2id$v=19$m=65536,t=2,p=1$bndaU0ZXRzYvRHRRbU9ubmM0TGsyQT09$12Dlzkd8oZ5CBlVuUivYSGbPL1M4nfTEIXb56hS+FVo'

# Database proxy ports
export STAGING_PROXY_PORT="15432"
export PROD_PROXY_PORT="15433"

# Local development
export DEV_DB_PORT="5433"
export LOCAL_STORAGE_ROOT="/tmp/kpbj"

# Production database (VPS)
export PROD_VPS_TARGET="root@stream.kpbj.fm"
export PROD_DB_NAME="kpbj_fm"
export PROD_DB_USER="kpbj_fm"
export PROD_READONLY_USER="kpbj_readonly"

# Staging database (VPS)
export STAGING_VPS_TARGET="root@staging.kpbj.fm"
export STAGING_DB_NAME="kpbj_fm"
export STAGING_DB_USER="kpbj_fm"

# Local database
export LOCAL_DB_NAME="dev_db"
export LOCAL_DB_USER="postgres"

# S3 Endpoints (DigitalOcean Spaces)
export PROD_ENDPOINT="https://sfo3.digitaloceanspaces.com"
export STAGING_ENDPOINT="https://sfo3.digitaloceanspaces.com"

# Buckets
export PROD_BUCKET="production-kpbj-storage"
export STAGING_BUCKET="staging-kpbj-storage"
