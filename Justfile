# =============================================================================
# Configuration
# =============================================================================
# Centralized constants used across recipes. Change these values here rather
# than hunting through the file.

DEV_DB_PORT := "5433"
LOCAL_STORAGE_ROOT := "/tmp/kpbj"
STAGING_PROXY_PORT := "15432"
PROD_PROXY_PORT := "15433"

HOME := "$HOME"
HS_FILES := "$(git ls-files 'services/web/*.hs' 'services/web/*.hs-boot')"
CHANGED_HS_FILES := '$(git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep "services/web/.*\.hs$")'
NIX_FILES := "$(git ls-files '*.nix' 'nix/*.nix')"
SHELL_FILES := "$(git ls-files '*.sh')"
CHANGED_SHELL_FILES := '$(git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep ".*\.sh$$")'

NIX_FMT := "nixpkgs-fmt"
ORMOLU := "ormolu"
ORMOLU_VERSION := "$(" + ORMOLU + " --version | awk 'NR==1 { print $2 }')"
ORMOLU_CHECK_VERSION := "0.7.2.0"

# Run Shellcheck with access to any file that's sourced, relative to the script's own directory
SHELLCHECK := "$(shellcheck --external-sources --source-path=SCRIPTDIR)"

# =============================================================================
# Private Helpers
# =============================================================================
# Internal recipes for prerequisite validation. Not shown in `just --list`.

[private]
_require CMD:
  @command -v {{CMD}} >/dev/null 2>&1 || { echo "ERROR: '{{CMD}}' is required but not installed."; exit 1; }

[private]
_require-docker:
  @command -v docker >/dev/null 2>&1 || { echo "ERROR: 'docker' is required but not installed."; exit 1; }
  @docker info >/dev/null 2>&1 || { echo "ERROR: Docker daemon is not running."; exit 1; }

[private]
_require-gh:
  @command -v gh >/dev/null 2>&1 || { echo "ERROR: 'gh' (GitHub CLI) is required but not installed."; exit 1; }
  @gh auth status >/dev/null 2>&1 || { echo "ERROR: Not logged in to GitHub. Run 'gh auth login' first."; exit 1; }

[private]
_require-aws:
  @command -v aws >/dev/null 2>&1 || { echo "ERROR: 'aws' (AWS CLI) is required but not installed."; exit 1; }

[private]
_require-sops:
  @command -v sops >/dev/null 2>&1 || { echo "ERROR: 'sops' is required but not installed. Run 'nix develop' to enter the dev shell."; exit 1; }
  @command -v age >/dev/null 2>&1 || { echo "ERROR: 'age' is required but not installed. Run 'nix develop' to enter the dev shell."; exit 1; }

# =============================================================================
# Build & Run
# =============================================================================
# Build the Haskell web service, run tests, and generate documentation.
# Prerequisites: GHC, Cabal (provided via Nix flake)

# Run the backend service
run:
  cabal run exe:kpbj-api

# Build all haskell packages.
build:
  cabal build all --enable-tests --enable-benchmarks

# Delete all build artifacts.
clean:
  cabal clean

# Run all test suites.
test:
  cabal test all

# Build docs
haddock:
  cabal haddock

# =============================================================================
# Formatting & Linting
# =============================================================================
# Code quality tools for Haskell, Nix, and shell scripts.
# Prerequisites: ormolu (0.7.2.0), nixpkgs-fmt, shellcheck, hlint, weeder

check-ormolu-version:
  @if ! [ "{{ORMOLU_VERSION}}" = "{{ORMOLU_CHECK_VERSION}}" ]; then \
    echo "WARNING: ormolu version mismatch, expected {{ORMOLU_CHECK_VERSION}} but got {{ORMOLU_VERSION}}"; \
  fi

# Auto-format Haskell source code using ormolu.
format-hs: check-ormolu-version
  @echo running {{ORMOLU}} --mode inplace
  @{{ORMOLU}} --mode inplace {{HS_FILES}}

# Auto-format Haskell source code using ormolu (changed files only).
format-hs-changed:
  @echo running {{ORMOLU}} --mode inplace
  @if [ -n "{{CHANGED_HS_FILES}}" ]; then \
  	{{ORMOLU}} --mode inplace {{CHANGED_HS_FILES}}; \
  fi

# Check Haskell source code formatting using ormolu.
check-format-hs: check-ormolu-version
  @echo running {{ORMOLU}} --mode check
  @{{ORMOLU}} --mode check {{HS_FILES}}

# Check Haskell source code formatting using ormolu (changed-files-only).
check-format-hs-changed: check-ormolu-version
  @echo running {{ORMOLU}} --mode check
  @if [ -n "{{CHANGED_HS_FILES}}" ]; then \
  	{{ORMOLU}} --mode check {{CHANGED_HS_FILES}}; \
  fi

# Auto-format Nix source code using `nixpkgs-fmt`.
format-nix:
  @if command -v {{NIX_FMT}} > /dev/null; then \
    echo "running {{NIX_FMT}}"; \
    {{NIX_FMT}} {{NIX_FILES}}; \
  else \
    echo "{{NIX_FMT}} is not installed; skipping"; \
  fi

# Check Nix source code using `nixpkgs-fmt`.
check-format-nix:
  @if command -v {{NIX_FMT}} > /dev/null; then \
  	echo "running {{NIX_FMT}} --check"; \
  	{{NIX_FMT}} --check {{NIX_FILES}}; \
  else \
  	echo "{{NIX_FMT}} is not installed; skipping"; \
  fi


# Run all formatters.
format: format-hs format-nix

# Run all formatters on changed files.
format-changed: format-hs-changed format-nix

# Check formatting on all files.
check-format: check-format-hs check-format-nix

# Check formatting on all changed files.
check-format-changed: check-format-hs-changed check-format-nix

# Lint shell scripts using `shellcheck`.
lint-shell:
  @echo running shellcheck
  @{{SHELLCHECK}} {{SHELL_FILES}}

# Lint shell scripts using `shellcheck` (changed files only).
lint-shell-changed:
  @echo running shellcheck
  @if [ -n "{{CHANGED_SHELL_FILES}}" ]; then \
  	{{SHELLCHECK}} {{CHANGED_SHELL_FILES}}; \
  fi

# Lint Haskell source code using hlint.
hlint:
  @echo running hlint
  @hlint {{HS_FILES}}

# Lint Haskell source code using hlint (changed files only).
hlint-changed:
  @echo running hlint
  @if [ -n "{{CHANGED_HS_FILES}}" ]; then \
  	hlint {{CHANGED_HS_FILES}}; \
  fi

# Detect dead code using weeder (rebuilds HIE files first).
weeder: build
  @echo running weeder
  @weeder

# =============================================================================
# Key Generation
# =============================================================================
# Generate RSA keys for JWT authentication.
# Reference: https://ruleoftech.com/2020/generating-jwt-and-jwk-for-information-exchange-between-services
# Prerequisites: openssl

# Generate RSA key for JWK
gen-keys:
  mkdir -p keys
  openssl genrsa -out keys/private.pem 4096
  openssl rsa -in keys/private.pem -out keys/public.pem -pubout
  openssl req -key keys/private.pem -new -x509 -days 3650 -subj "/C=FI/ST=Helsinki/O=Rule of Tech/OU=Information unit/CN=ruleoftech.com" -out keys/cert.pem
  openssl pkcs12 -export -inkey keys/private.pem -in keys/cert.pem -out keys/keys.pfx -name "kpbj-backend"

# =============================================================================
# Database (Local Development)
# =============================================================================
# Start/stop PostgreSQL containers, run migrations, and manage mock data.
# Prerequisites: Docker, psql, sqlx-cli

# Create a new SQL migration.
dev-migrations-add MIGRATION:
  sqlx migrate add {{MIGRATION}} --source services/web/migrations

# Run SQL migrations.
dev-migrations-run:
  sqlx migrate run --source services/web/migrations

# Reset PG Database.
dev-migrations-reset:
  sqlx database reset --source services/web/migrations

# List all SQL migrations.
dev-migrations-list:
  sqlx migrate info --source services/web/migrations

# Build and run a development docker container
dev-postgres-start: _require-docker
  echo "🟢 Starting the Development Postgres service.."
  docker run \
    --rm \
    --name kpbj-dev-postgres \
    -d -p {{DEV_DB_PORT}}:5432 \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    -e POSTGRES_DB=dev_db \
    -v {{HOME}}/.kpbj-dev-pg-data:/var/lib/postgresql/data \
    -d postgres
  echo "✨ Success!"

# Halt the development docker container
dev-postgres-stop:
  echo "🔴 Stopping the Development Postgres service.."
  docker container stop kpbj-dev-postgres
  echo ✨ "Success!"

# Connect to the development postgres db with psql.
dev-postgres-psql:
  echo "🌐 Connecting to the Development Postgres service.."
  psql -h localhost -p {{DEV_DB_PORT}} -U postgres -d dev_db

# Generate mock images for shows, episodes, events, etc.
dev-mock-images:
  echo "🖼️  Generating mock images..."
  ./scripts/generate-mock-images.sh
  echo "✨ Mock images generated!"

# Load mock data into the development database
dev-mock-data:
  echo "🖼️  Loading mock images to {{LOCAL_STORAGE_ROOT}}..."
  ./scripts/load-mock-images.sh
  echo "📊 Loading mock data into dev_db..."
  cd services/web/mock-data && PGPASSWORD=postgres psql -h localhost -p {{DEV_DB_PORT}} -U postgres -d dev_db -f load-all.sql
  echo "✨ Mock data loaded successfully!"


# =============================================================================
# Deployment (General)
# =============================================================================
# Build Docker images and create release PRs.
# Prerequisites: Nix, gh (GitHub CLI)

# Build and publish Docker image to container registry (for local testing)
# In CI, use the GitHub Actions workflows instead
dev-docker-publish:
  nix run .#publish

# Create a release PR that bumps the version and updates CHANGELOG.md
# Usage: just release-pr 0.3.3
release-pr VERSION: _require-gh
  ./scripts/release-pr.sh "{{VERSION}}"

# Preview the release notes that would be included in the next release
release-notes-preview:
  #!/usr/bin/env bash
  set -euo pipefail

  if [ ! -f "CHANGELOG.md" ]; then
    echo "ERROR: CHANGELOG.md not found."
    exit 1
  fi

  echo "=== Release Notes Preview ==="
  echo ""
  awk '/^## \[Unreleased\]/{flag=1; next} /^## \[|^---/{flag=0} flag' CHANGELOG.md | sed '/^$/N;/^\n$/d'
  echo ""
  echo "=== End Preview ==="

# =============================================================================
# Deployment (Staging)
# =============================================================================
# Deploy and manage the staging environment on the DigitalOcean VPS.
# The staging web service runs as a native NixOS systemd service.
# Prerequisites: SSH access to staging VPS, sops (for DB password)

STAGING_VPS_TARGET := "root@staging.kpbj.fm"
STAGING_DB_NAME := "kpbj_fm"
STAGING_DB_USER := "kpbj_fm"

# View staging web service logs
staging-logs:
  ssh {{STAGING_VPS_TARGET}} journalctl -u kpbj-web -f

# View staging service status
staging-status:
  ssh {{STAGING_VPS_TARGET}} systemctl status kpbj-web kpbj-postgres-setup postgresql

# Open staging in browser
staging-open:
  xdg-open https://staging.kpbj.fm 2>/dev/null || open https://staging.kpbj.fm

# Connect to staging database with psql (via SSH tunnel, read-only by default, pass --write to allow writes)
staging-psql *args: _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  if [ "{{args}}" = "--write" ]; then
    DB_USER="kpbj_readwrite"
    DB_PASSWORD=$(sops -d --extract '["db_readwrite_password"]' secrets/staging-web.yaml)
  else
    DB_USER="kpbj_readonly"
    DB_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/staging-web.yaml)
  fi
  kill "$(lsof -ti:{{STAGING_PROXY_PORT}})" 2>/dev/null || true
  echo "Opening SSH tunnel..."
  ssh -N -L {{STAGING_PROXY_PORT}}:127.0.0.1:5432 {{STAGING_VPS_TARGET}} &
  TUNNEL_PID=$!
  trap "kill $TUNNEL_PID 2>/dev/null || true" EXIT
  sleep 2
  PGPASSWORD="$DB_PASSWORD" psql -h localhost -p {{STAGING_PROXY_PORT}} -U "$DB_USER" -d {{STAGING_DB_NAME}}

# Run migrations on staging (via SSH tunnel)
staging-migrations-run: _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  STAGING_DB_PASSWORD=$(sops -d --extract '["db_password"]' secrets/staging-web.yaml)
  kill "$(lsof -ti:{{STAGING_PROXY_PORT}})" 2>/dev/null || true
  echo "Opening SSH tunnel..."
  ssh -N -L {{STAGING_PROXY_PORT}}:127.0.0.1:5432 {{STAGING_VPS_TARGET}} &
  TUNNEL_PID=$!
  trap "kill $TUNNEL_PID 2>/dev/null || true" EXIT
  sleep 2
  DATABASE_URL="postgres://{{STAGING_DB_USER}}:${STAGING_DB_PASSWORD}@localhost:{{STAGING_PROXY_PORT}}/{{STAGING_DB_NAME}}" \
    sqlx migrate run --source services/web/migrations

# Reset staging database (drops all tables, re-runs migrations)
# NOTE: Uses root SSH because DROP DATABASE requires superuser privileges.
staging-migrations-reset: _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  echo "⚠️  Resetting staging database (this will delete all data)..."
  read -rp "Type 'yes' to confirm: " CONFIRM
  if [ "$CONFIRM" != "yes" ]; then echo "Aborted."; exit 1; fi
  STAGING_DB_PASSWORD=$(sops -d --extract '["db_password"]' secrets/staging-web.yaml)
  echo "Stopping staging web service..."
  ssh {{STAGING_VPS_TARGET}} systemctl stop kpbj-web
  kill "$(lsof -ti:{{STAGING_PROXY_PORT}})" 2>/dev/null || true
  echo "Opening SSH tunnel..."
  ssh -N -L {{STAGING_PROXY_PORT}}:127.0.0.1:5432 {{STAGING_VPS_TARGET}} &
  TUNNEL_PID=$!
  trap "kill $TUNNEL_PID 2>/dev/null || true; ssh {{STAGING_VPS_TARGET}} systemctl start kpbj-web" EXIT
  sleep 2
  echo "Dropping and recreating database..."
  ssh {{STAGING_VPS_TARGET}} "sudo -u postgres psql -c \"DROP DATABASE IF EXISTS {{STAGING_DB_NAME}} WITH (FORCE);\" && sudo -u postgres psql -c \"CREATE DATABASE {{STAGING_DB_NAME}} OWNER {{STAGING_DB_USER}};\""
  echo "Running migrations..."
  DATABASE_URL="postgres://{{STAGING_DB_USER}}:${STAGING_DB_PASSWORD}@localhost:{{STAGING_PROXY_PORT}}/{{STAGING_DB_NAME}}" \
    sqlx migrate run --source services/web/migrations
  echo "✨ Staging database reset complete!"

# Open Grafana dashboard (SSH tunnel)
staging-grafana:
  ssh -N -L 3000:127.0.0.1:3000 {{STAGING_VPS_TARGET}}

# SSH into staging VPS
staging-ssh:
  ssh {{STAGING_VPS_TARGET}}

# Restart staging web service
staging-restart:
  ssh {{STAGING_VPS_TARGET}} systemctl restart kpbj-web

# Edit staging web secrets
staging-secrets: _require-sops
  sops secrets/staging-web.yaml

# =============================================================================
# Deployment (Production)
# =============================================================================
# Deploy and manage the production environment on the DigitalOcean VPS.
# The production web service runs as a native NixOS systemd service.
# Prerequisites: SSH access to production VPS, sops (for DB password)

PROD_VPS_TARGET := "root@stream.kpbj.fm"
PROD_DB_NAME := "kpbj_fm"
PROD_DB_USER := "kpbj_fm"

# View production web service logs
prod-logs:
  ssh {{PROD_VPS_TARGET}} journalctl -u kpbj-web -f

# View production service status
prod-status:
  ssh {{PROD_VPS_TARGET}} systemctl status kpbj-web kpbj-postgres-setup postgresql

# Open production in browser
prod-open:
  xdg-open https://www.kpbj.fm 2>/dev/null || open https://www.kpbj.fm

# Connect to production database with psql (via SSH tunnel, read-only by default, pass --write to allow writes)
prod-psql *args: _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  if [ "{{args}}" = "--write" ]; then
    DB_USER="kpbj_readwrite"
    DB_PASSWORD=$(sops -d --extract '["db_readwrite_password"]' secrets/prod-web.yaml)
  else
    DB_USER="kpbj_readonly"
    DB_PASSWORD=$(sops -d --extract '["db_readonly_password"]' secrets/prod-web.yaml)
  fi
  kill "$(lsof -ti:{{PROD_PROXY_PORT}})" 2>/dev/null || true
  echo "Opening SSH tunnel..."
  ssh -N -L {{PROD_PROXY_PORT}}:127.0.0.1:5432 {{PROD_VPS_TARGET}} &
  TUNNEL_PID=$!
  trap "kill $TUNNEL_PID 2>/dev/null || true" EXIT
  sleep 2
  PGPASSWORD="$DB_PASSWORD" psql -h localhost -p {{PROD_PROXY_PORT}} -U "$DB_USER" -d {{PROD_DB_NAME}}

# Run migrations on production (via SSH tunnel)
prod-migrations-run: _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  PROD_DB_PASSWORD=$(sops -d --extract '["db_password"]' secrets/prod-web.yaml)
  kill "$(lsof -ti:{{PROD_PROXY_PORT}})" 2>/dev/null || true
  echo "Opening SSH tunnel..."
  ssh -N -L {{PROD_PROXY_PORT}}:127.0.0.1:5432 {{PROD_VPS_TARGET}} &
  TUNNEL_PID=$!
  trap "kill $TUNNEL_PID 2>/dev/null || true" EXIT
  sleep 2
  DATABASE_URL="postgres://{{PROD_DB_USER}}:${PROD_DB_PASSWORD}@localhost:{{PROD_PROXY_PORT}}/{{PROD_DB_NAME}}" \
    sqlx migrate run --source services/web/migrations

# Open Grafana dashboard (SSH tunnel)
prod-grafana:
  ssh -N -L 3000:127.0.0.1:3000 {{PROD_VPS_TARGET}}

# SSH into production VPS
prod-ssh:
  ssh {{PROD_VPS_TARGET}}

# Restart production web service
prod-restart:
  ssh {{PROD_VPS_TARGET}} systemctl restart kpbj-web

# Edit production web secrets
prod-secrets: _require-sops
  sops secrets/prod-web.yaml

# Backup production database to local file
# Credentials loaded from SOPS-encrypted secrets/backup.yaml
prod-backup-pg: _require-sops
  ./scripts/prod-backup-pg.sh

# Backup production database from remote server (e.g., TrueNAS)
# Optional: BACKUP_DIR (default: ./backups/postgres), RETENTION_DAYS (default: 14)
prod-backup-remote:
  ./scripts/prod-backup-remote.sh

# Backup production S3 bucket using rclone with date-based snapshots
# Credentials loaded from SOPS-encrypted secrets/backup.yaml
# Optional: BACKUP_DIR (default: ./backups/s3), RETENTION_DAYS (default: 14)
prod-backup-s3: _require-sops _require-aws
  ./scripts/prod-backup-s3.sh

# =============================================================================
# Data Sync (Production to Staging)
# =============================================================================
# Copy production data to staging with PII sanitization.
# User/Host accounts get anonymized; Staff/Admin accounts keep real credentials.
# Credentials loaded from SOPS-encrypted secrets/backup.yaml.

# Copy production database to staging with PII sanitization
prod-to-staging-db: _require-sops
  ./scripts/prod-to-staging-db.sh

# Copy production S3 bucket to staging
prod-to-staging-s3: _require-sops _require-aws
  ./scripts/prod-to-staging-s3.sh

# Copy both production database and S3 bucket to staging
prod-to-staging: _require-sops _require-aws
  ./scripts/prod-to-staging.sh

# =============================================================================
# Data Sync (Production to Local)
# =============================================================================
# Copy production data to local development with PII sanitization.
# User/Host accounts get anonymized; Staff/Admin accounts keep real credentials.
# Credentials loaded from SOPS-encrypted secrets/backup.yaml.

# Copy production database to local dev with PII sanitization
# Requires local postgres running (just dev-postgres-start)
prod-to-local-db: _require-sops
  ./scripts/prod-to-local-db.sh

# Copy production S3 files to local dev (/tmp/kpbj)
prod-to-local-files: _require-sops _require-aws
  ./scripts/prod-to-local-files.sh

# Copy both production database and files to local dev
prod-to-local: _require-sops _require-aws
  ./scripts/prod-to-local.sh

# =============================================================================
# Terraform
# =============================================================================
# Infrastructure as code for DigitalOcean and Cloudflare.
# Credentials loaded from SOPS-encrypted secrets/terraform.yaml

[private]
_require-terraform:
  @command -v terraform >/dev/null 2>&1 || { echo "ERROR: 'terraform' is required but not installed."; exit 1; }

TF_DIR := "terraform"

# Initialize Terraform (with remote state backend)
tf-init: _require-terraform _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  ACCESS_KEY=$(sops -d --extract '["state_backend_access_key_id"]' secrets/terraform.yaml)
  SECRET_KEY=$(sops -d --extract '["state_backend_secret_access_key"]' secrets/terraform.yaml)
  cd {{TF_DIR}} && terraform init -backend-config="access_key=$ACCESS_KEY" -backend-config="secret_key=$SECRET_KEY"

# Preview infrastructure changes
tf-plan: _require-terraform
  cd {{TF_DIR}} && terraform plan

# Apply infrastructure changes
tf-apply: _require-terraform
  cd {{TF_DIR}} && terraform apply

# Show current Terraform state
tf-show: _require-terraform
  cd {{TF_DIR}} && terraform show

# Format Terraform files
tf-fmt: _require-terraform
  cd {{TF_DIR}} && terraform fmt

# Validate Terraform configuration
tf-validate: _require-terraform
  cd {{TF_DIR}} && terraform validate

# =============================================================================
# SOPS Secrets
# =============================================================================
# Manage SOPS-encrypted secrets.
# Prerequisites: sops, age, ssh-to-age (provided via Nix flake)

# Edit Terraform secrets (provider tokens, state backend keys)
sops-edit-terraform: _require-sops
  sops secrets/terraform.yaml

# Edit backup/sync credentials (prod + staging AWS keys, DB passwords)
sops-edit-backup: _require-sops
  sops secrets/backup.yaml

# Edit dev streaming secrets
sops-edit-dev-streaming: _require-sops
  sops secrets/dev-streaming.yaml

# Edit production streaming secrets
sops-edit-prod-streaming: _require-sops
  sops secrets/prod-streaming.yaml

# Edit staging streaming secrets
sops-edit-staging-streaming: _require-sops
  sops secrets/staging-streaming.yaml

# Edit Google Groups / GCP secrets (service account, delegated user, group email)
sops-edit-google: _require-sops
  sops secrets/google.yaml

# Edit production web secrets (DB password, SMTP, AWS keys)
sops-edit-prod-web: _require-sops
  sops secrets/prod-web.yaml

# Edit staging web secrets (DB password, SMTP, AWS keys)
sops-edit-staging-web: _require-sops
  sops secrets/staging-web.yaml

# Get a VPS host's age public key (for adding to .sops.yaml)
sops-host-key HOST:
  ssh-keyscan -t ed25519 {{HOST}} 2>/dev/null | ssh-to-age

# =============================================================================
# Streaming Services (Local Dev)
# =============================================================================
# Run the streaming stack locally in a NixOS QEMU VM.
# Uses the exact same streaming.nix config as staging/prod.
# Prerequisites: Nix, QEMU (via nixpkgs virtualisation module)

STREAM_VM_PIDFILE := "/tmp/kpbj-stream-dev.pid"
SOPS_KEY_DIR := "/tmp/kpbj-dev-sops-key"

# Build the dev streaming VM
stream-dev-build:
  nix build .#nixosConfigurations.kpbj-stream-dev.config.system.build.vm

# Start the dev streaming VM (backgrounded)
stream-dev-start: stream-dev-build _require-sops
  #!/usr/bin/env bash
  set -euo pipefail
  if [ -f "{{STREAM_VM_PIDFILE}}" ] && kill -0 "$(cat "{{STREAM_VM_PIDFILE}}")" 2>/dev/null; then
    echo "VM already running (PID $(cat "{{STREAM_VM_PIDFILE}}"))."
    exit 0
  fi
  mkdir -p "{{SOPS_KEY_DIR}}"
  cp "$HOME/.config/sops/age/keys.txt" "{{SOPS_KEY_DIR}}/keys.txt"
  echo "Starting dev streaming VM..."
  ./result/bin/run-kpbj-stream-dev-vm &
  echo $! > "{{STREAM_VM_PIDFILE}}"
  echo "VM started (PID $(cat "{{STREAM_VM_PIDFILE}}"))."
  echo "Icecast: http://localhost:8000  Webhook: http://localhost:9000"

# SSH into the dev streaming VM
stream-dev-ssh:
  ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -p 2222 root@localhost

# View dev streaming VM logs (all three services)
stream-dev-logs:
  ssh -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null -p 2222 root@localhost journalctl -u 'kpbj-*' -f

# Stop the dev streaming VM
stream-dev-stop:
  #!/usr/bin/env bash
  set -euo pipefail
  if [ ! -f "{{STREAM_VM_PIDFILE}}" ]; then
    echo "No VM pidfile found."
    exit 0
  fi
  PID=$(cat "{{STREAM_VM_PIDFILE}}")
  if kill -0 "$PID" 2>/dev/null; then
    echo "Stopping VM (PID $PID)..."
    kill "$PID"
    rm -f "{{STREAM_VM_PIDFILE}}"
    echo "VM stopped."
  else
    echo "VM not running (stale pidfile). Cleaning up."
    rm -f "{{STREAM_VM_PIDFILE}}"
  fi

# =============================================================================
# NixOS Deployment (Streaming VPS)
# =============================================================================
# Declarative NixOS configuration for streaming droplets. Builds locally
# and copies via SSH (1GB droplets can't build NixOS closures).
# Prerequisites: NixOS flake builds, SSH access to target host

PROD_STREAM_TARGET := "root@stream.kpbj.fm"
STAGING_STREAM_TARGET := "root@stream.staging.kpbj.fm"

# Complete NixOS setup on a freshly-provisioned droplet
nixos-setup HOST ENV:
  ./scripts/nixos-setup.sh {{HOST}} {{ENV}}

# Deploy NixOS config to production streaming VPS
# Uses 'boot' instead of 'switch' to avoid hangs on first deploy,
# then reboots. Safe for ongoing deploys too (activates on next boot).
nixos-deploy-prod:
  nixos-rebuild switch --flake .#kpbj-stream-prod --target-host {{PROD_STREAM_TARGET}}

# Deploy NixOS config to staging streaming VPS
nixos-deploy-staging:
  nixos-rebuild switch --flake .#kpbj-stream-staging --target-host {{STAGING_STREAM_TARGET}}

# Preview NixOS changes for production (dry-activate)
nixos-deploy-prod-dry:
  nixos-rebuild dry-activate --flake .#kpbj-stream-prod --target-host {{PROD_STREAM_TARGET}}

# Preview NixOS changes for staging (dry-activate)
nixos-deploy-staging-dry:
  nixos-rebuild dry-activate --flake .#kpbj-stream-staging --target-host {{STAGING_STREAM_TARGET}}

# Build production NixOS config locally (verify it evaluates)
nixos-build-prod:
  nix build .#nixosConfigurations.kpbj-stream-prod.config.system.build.toplevel

# Build staging NixOS config locally (verify it evaluates)
nixos-build-staging:
  nix build .#nixosConfigurations.kpbj-stream-staging.config.system.build.toplevel

# =============================================================================
# E2E Testing (Playwright)
# =============================================================================
# Browser-based end-to-end tests. Requires the web server running on port 4000.
# Run `just dev-mock-data` before running E2E tests to load deterministic test data.
# Prerequisites: playwright-test (provided via Nix flake)

# Run E2E tests (headless).
e2e:
  cd e2e && playwright test

# Run E2E tests with interactive UI mode.
e2e-ui:
  cd e2e && playwright test --ui

# Run E2E tests in a visible browser.
e2e-headed:
  cd e2e && playwright test --headed

# View the last E2E test report.
e2e-report:
  cd e2e && playwright show-report
