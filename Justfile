HOME := "$HOME"
HS_FILES := "$(git ls-files '*.hs' '*.hs-boot')"
CHANGED_HS_FILES := '$(git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep ".*\.hs$")'
NIX_FILES := "$(git ls-files '*.nix' 'nix/*.nix')"
SHELL_FILES := "$(git ls-files '*.sh')"
CHANGED_SHELL_FILES := '$(git diff --diff-filter=d --name-only `git merge-base HEAD origin/main` | grep ".*\.sh$$")'

NIX_FMT := "nixpkgs-fmt"
ORMOLU := "ormolu"
ORMOLU_VERSION := "$(" + ORMOLU + " --version | awk 'NR==1 { print $2 }')"
ORMOLU_CHECK_VERSION := "0.7.2.0"

# Run Shellcheck with access to any file that's sourced, relative to the script's own directory
SHELLCHECK := "$(shellcheck --external-sources --source-path=SCRIPTDIR)"

#-------------------------------------------------------------------------------
## Cabal

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

#-------------------------------------------------------------------------------
## Formatting

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

# Detect dead code using weeder.
weeder:
  @echo running weeder
  @weeder

#-------------------------------------------------------------------------------
## Key Gen
# https://ruleoftech.com/2020/generating-jwt-and-jwk-for-information-exchange-between-services

# Generate RSA key for JWK
gen-keys:
  mkdir -p keys
  openssl genrsa -out keys/private.pem 4096
  openssl rsa -in keys/private.pem -out keys/public.pem -pubout
  openssl req -key keys/private.pem -new -x509 -days 3650 -subj "/C=FI/ST=Helsinki/O=Rule of Tech/OU=Information unit/CN=ruleoftech.com" -out keys/cert.pem
  openssl pkcs12 -export -inkey keys/private.pem -in keys/cert.pem -out keys/keys.pfx -name "kpbj-backend"

#-------------------------------------------------------------------------------
## Database

# Connect to local dev db with psql.
psql-dev:
  @psql $DATABASE_URL

# Create a new SQL migration.
migrations-add MIGRATION:
  sqlx migrate add {{MIGRATION}} --source migrations

# Run SQL migrations.
migrations-run:
  sqlx migrate run --source migrations

# Reset PG Database.
migrations-reset:
  sqlx database reset --source migrations

# List all SQL migrations.
migrations-list:
  sqlx migrate info --source migrations

# Build and run a development docker container
postgres-dev-start:
  echo "🟢 Starting the Development Postgres service.."
  docker run \
    --rm \
    --name kpbj-dev-postgres \
    -d -p 5433:5432 \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    -e POSTGRES_DB=dev_db \
    -v {{HOME}}/.kpbj-dev-pg-data:/var/lib/postgresql/data \
    -d postgres
  echo "✨ Success!"

# Halt the development docker container
postgres-dev-stop:
  echo "🔴 Stopping the Development Postgres service.."
  docker container stop kpbj-dev-postgres
  echo ✨ "Success!"

# Connect to the development postgres db with psql.
postgres-dev-psql:
  echo "🌐 Connecting to the Development Postgres service.."
  psql -h localhost -p 5433 -U postgres -d dev_db

# Build and run a test docker container
postgres-test-start:
  echo "🟢 Starting the Test Postgres service.."
  docker run --rm --name kpbj-test-postgres -d -p 5434:5432 -e POSTGRES_HOST_AUTH_METHOD=trust -e POSTGRES_DB=test_db -d postgres
  echo "✨ Success!"

# Halt the test docker container
postgres-test-stop:
  echo "🔴 Stopping the Test Postgres service.."
  docker container stop kpbj-test-postgres
  echo ✨ "Success!"

# Connect to the test postgres db with psql.
postgres-test-psql:
  echo "🌐 Connecting to the Test Postgres service.."
  psql -h localhost -p 5434 -U postgres -d test_db

# Generate mock images for shows, episodes, events, etc.
mock-images:
  echo "🖼️  Generating mock images..."
  ./scripts/generate-mock-images.sh
  echo "✨ Mock images generated!"

# Load mock data into the development database
mock-data:
  echo "🖼️  Loading mock images to /tmp/kpbj..."
  ./scripts/load-mock-images.sh
  echo "📊 Loading mock data into dev_db..."
  cd mock-data && PGPASSWORD=postgres psql -h localhost -p 5433 -U postgres -d dev_db -f load-all.sql
  echo "✨ Mock data loaded successfully!"


#-------------------------------------------------------------------------------
## Deployment

# Build and publish Docker image to container registry
publish:
  nix run .#publish

#-------------------------------------------------------------------------------
## Fly.io Staging

STAGING_APP := "kpbj-fm-staging"
STAGING_DB_APP := "kpbj-postgres-staging"
STAGING_DB_NAME := "kpbj_fm_staging"
STAGING_CONFIG := "fly.staging.toml"

# Deploy to staging
staging-deploy:
  fly deploy -c {{STAGING_CONFIG}}

# View staging logs
staging-logs:
  fly logs --app {{STAGING_APP}}

# View staging app status
staging-status:
  fly status --app {{STAGING_APP}}

# Open staging in browser
staging-open:
  fly open --app {{STAGING_APP}}

# Connect to staging database with psql
staging-psql:
  fly postgres connect --app {{STAGING_DB_APP}} -d {{STAGING_DB_NAME}}

# Load mock data into staging database
staging-mock-data:
  @echo "📊 Loading mock data into staging..."
  @(cat mock-data/00_init.sql mock-data/01_admin_user.sql mock-data/02_shows.sql mock-data/03_show_tags.sql mock-data/04_show_tag_assignments.sql mock-data/05_host_users.sql mock-data/06_host_user_metadata.sql mock-data/07_show_hosts.sql mock-data/08_host_details.sql mock-data/09_schedule_templates.sql mock-data/10_episodes.sql mock-data/11_staff_and_users.sql mock-data/12_events.sql mock-data/13_blog_tags.sql mock-data/14_blog_posts.sql mock-data/15_episode_tracks.sql mock-data/16_summary.sql; echo "\\q") | fly postgres connect --app {{STAGING_DB_APP}} -d {{STAGING_DB_NAME}}
  @echo "✨ Mock data loaded successfully!"

# Upload mock images to staging volume
staging-mock-images:
  #!/usr/bin/env bash
  set -euo pipefail
  echo "🖼️  Uploading mock images to staging..."
  echo ""

  # Create a temporary directory with the correct structure
  TMPDIR=$(mktemp -d)
  trap "rm -rf $TMPDIR" EXIT

  # Create target directory structure (matches load-mock-images.sh)
  mkdir -p "$TMPDIR/images/2025/01/01/avatars"
  mkdir -p "$TMPDIR/images/2025/01/01/logos"
  mkdir -p "$TMPDIR/images/2025/01/01/banners"
  mkdir -p "$TMPDIR/images/2025/01/01/artwork"
  mkdir -p "$TMPDIR/images/2025/01/01/event-posters"
  mkdir -p "$TMPDIR/images/2025/01/01/blog-heroes"

  # Copy files to temp directory with correct structure
  cp mock-data/media/avatars/* "$TMPDIR/images/2025/01/01/avatars/" 2>/dev/null || true
  cp mock-data/media/shows/logos/* "$TMPDIR/images/2025/01/01/logos/" 2>/dev/null || true
  cp mock-data/media/shows/banners/* "$TMPDIR/images/2025/01/01/banners/" 2>/dev/null || true
  cp mock-data/media/episodes/artwork/* "$TMPDIR/images/2025/01/01/artwork/" 2>/dev/null || true
  cp mock-data/media/events/posters/* "$TMPDIR/images/2025/01/01/event-posters/" 2>/dev/null || true
  cp mock-data/media/blog/heroes/* "$TMPDIR/images/2025/01/01/blog-heroes/" 2>/dev/null || true

  FILE_COUNT=$(find $TMPDIR -type f | wc -l)
  echo "  Prepared $FILE_COUNT files for upload"

  # Create tarball
  TARBALL="$TMPDIR/mock-images.tar.gz"
  tar -C "$TMPDIR" -czf "$TARBALL" images
  echo "  Created tarball: $(du -h $TARBALL | cut -f1)"

  # Upload and extract via SSH
  echo "  Uploading to staging (run 'just staging-ssh-setup' first if this fails)..."
  tar -C "$TMPDIR" -czf - images | fly ssh console --app {{STAGING_APP}} -C "/bin/tar -C /tmp/kpbj -xzf -"

  echo "✨ Mock images uploaded successfully!"

# Load all mock data (database + images) into staging
staging-mock-all: staging-mock-images staging-mock-data
  @echo "🎉 All staging mock data loaded!"

# Reset staging database (drops all tables, re-runs migrations)
staging-migrations-reset:
  @echo "⚠️  Resetting staging database (this will delete all data)..."
  @echo "Starting proxy in background..."
  fly proxy 15432:5432 -a {{STAGING_DB_APP}} &
  @sleep 3
  @echo "Stopping staging app to release database connections..."
  fly scale count 0 --app {{STAGING_APP}} --yes
  @echo "Dropping database..."
  psql "postgres://postgres:{{env_var("STAGING_DB_PASSWORD")}}@localhost:15432/postgres" -c "DROP DATABASE IF EXISTS {{STAGING_DB_NAME}} WITH (FORCE);"
  @echo "Creating database..."
  psql "postgres://postgres:{{env_var("STAGING_DB_PASSWORD")}}@localhost:15432/postgres" -c "CREATE DATABASE {{STAGING_DB_NAME}};"
  @echo "Running migrations..."
  DATABASE_URL=postgres://postgres:{{env_var("STAGING_DB_PASSWORD")}}@localhost:15432/{{STAGING_DB_NAME}} sqlx migrate run --source migrations
  @echo "Restarting staging app..."
  fly scale count 1 --app {{STAGING_APP}} --yes
  @just staging-proxy-close
  @echo "✨ Staging database reset complete!"

# Run migrations on staging (via proxy)
staging-migrations-run:
  @echo "Starting proxy in background..."
  fly proxy 15432:5432 -a {{STAGING_DB_APP}} &
  @sleep 2
  DATABASE_URL=postgres://postgres:{{env_var("STAGING_DB_PASSWORD")}}@localhost:15432/{{STAGING_DB_NAME}} sqlx migrate run --source migrations
  @pkill -f "fly proxy 15432"

# Open proxy to staging database (runs in foreground)
staging-proxy-open:
  fly proxy 15432:5432 -a {{STAGING_DB_APP}}

# Close staging database proxy
staging-proxy-close:
  @kill $(lsof -ti:15432) 2>/dev/null && echo "Proxy closed" || echo "No proxy running"

# SSH into staging app
staging-ssh:
  fly ssh console --app {{STAGING_APP}}

# Restart staging app
staging-restart:
  fly apps restart {{STAGING_APP}}

# View staging secrets
staging-secrets:
  fly secrets list --app {{STAGING_APP}}

# View staging machines
staging-machines:
  fly machines list --app {{STAGING_APP}}

#-------------------------------------------------------------------------------
## Fly.io Production

PROD_APP := "kpbj-fm"
PROD_DB_APP := "kpbj-postgres"
PROD_DB_NAME := "kpbj_fm"
PROD_CONFIG := "fly.toml"

# Deploy to production
prod-deploy:
  fly deploy -c {{PROD_CONFIG}}

# View production logs
prod-logs:
  fly logs --app {{PROD_APP}}

# View production app status
status:
  fly status --app {{PROD_APP}}

# Open production in browser
prod-open:
  fly open --app {{PROD_APP}}

# Connect to production database with psql
prod-psql:
  fly postgres connect --app {{PROD_DB_APP}} -d {{PROD_DB_NAME}}

# Run migrations on production (via proxy)
prod-migrations-run:
  @echo "Starting proxy in background..."
  fly proxy 15432:5432 -a {{PROD_DB_APP}} &
  @sleep 2
  DATABASE_URL=postgres://postgres:{{env_var("PROD_DB_PASSWORD")}}@localhost:15432/{{PROD_DB_NAME}} sqlx migrate run --source migrations
  @pkill -f "fly proxy 15432"

# SSH into production app
prod-ssh:
  fly ssh console --app {{PROD_APP}}

# Restart production app
prod-restart:
  fly apps restart {{PROD_APP}}

# View production secrets
prod-secrets:
  fly secrets list --app {{PROD_APP}}

# View production machines
prod-machines:
  fly machines list --app {{PROD_APP}}
