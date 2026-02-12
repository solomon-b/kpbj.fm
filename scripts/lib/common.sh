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

# Production database
export PROD_DB_APP="kpbj-postgres"
export PROD_DB_NAME="kpbj_fm"
export PROD_DB_USER="kpbj_fm"

# Staging database (VPS)
export STAGING_VPS_TARGET="root@staging.kpbj.fm"
export STAGING_DB_NAME="kpbj_fm"
export STAGING_DB_USER="kpbj_fm"

# Local database
export LOCAL_DB_NAME="dev_db"
export LOCAL_DB_USER="postgres"

# S3/Tigris
export PROD_BUCKET="production-kpbj-storage"
export STAGING_BUCKET="staging-kpbj-storage"
export TIGRIS_ENDPOINT="https://fly.storage.tigris.dev"
