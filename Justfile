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

# Detect dead code using weeder (rebuilds HIE files first).
weeder: build
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
  sqlx migrate add {{MIGRATION}} --source services/web/migrations

# Run SQL migrations.
migrations-run:
  sqlx migrate run --source services/web/migrations

# Reset PG Database.
migrations-reset:
  sqlx database reset --source services/web/migrations

# List all SQL migrations.
migrations-list:
  sqlx migrate info --source services/web/migrations

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
  cd services/web/mock-data && PGPASSWORD=postgres psql -h localhost -p 5433 -U postgres -d dev_db -f load-all.sql
  echo "✨ Mock data loaded successfully!"


#-------------------------------------------------------------------------------
## Deployment

# Build and publish Docker image to container registry (for local testing)
# In CI, use the GitHub Actions workflows instead
publish:
  nix run .#publish

# Create a release PR that bumps the version and updates CHANGELOG.md
# Usage: just release-pr 0.3.3
release-pr VERSION:
  #!/usr/bin/env bash
  set -euo pipefail

  VERSION="{{VERSION}}"
  TODAY=$(date +%Y-%m-%d)

  # Validate version format (X.Y.Z)
  if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
    echo "ERROR: Invalid version format. Expected X.Y.Z (e.g., 0.3.3)"
    exit 1
  fi

  # Check that CHANGELOG.md exists
  if [ ! -f "CHANGELOG.md" ]; then
    echo "ERROR: CHANGELOG.md not found. Please create it first."
    exit 1
  fi

  # Check that there's an [Unreleased] section with content
  if ! grep -q "^\## \[Unreleased\]" CHANGELOG.md; then
    echo "ERROR: No [Unreleased] section found in CHANGELOG.md"
    exit 1
  fi

  # Check for uncommitted changes (ignores untracked files)
  if [ -n "$(git status --porcelain | grep -v '^??')" ]; then
    echo "ERROR: You have uncommitted changes. Please commit or stash them first."
    exit 1
  fi

  # Ensure we're on main and up to date
  git checkout main
  git pull origin main

  # Create release branch
  BRANCH="release/$VERSION"
  echo "Creating branch: $BRANCH"
  git checkout -b "$BRANCH"

  # Extract release notes from [Unreleased] section
  # Gets everything between [Unreleased] and the next ## heading (or ---)
  echo "Extracting release notes from CHANGELOG.md..."
  RELEASE_NOTES=$(awk '/^## \[Unreleased\]/{flag=1; next} /^## \[|^---/{flag=0} flag' CHANGELOG.md | sed '/^$/N;/^\n$/d')

  if [ -z "$RELEASE_NOTES" ]; then
    echo "ERROR: [Unreleased] section is empty. Please add release notes first."
    git checkout main
    git branch -D "$BRANCH"
    exit 1
  fi

  # Update CHANGELOG.md: rename [Unreleased] to [VERSION] and add new [Unreleased]
  echo "Updating CHANGELOG.md..."
  sed -i "s/^## \[Unreleased\]/## [Unreleased]\n\n_No changes yet._\n\n---\n\n## [$VERSION] - $TODAY/" CHANGELOG.md

  # Update cabal version
  echo "Updating kpbj-api.cabal version to $VERSION"
  sed -i "s/^version:.*$/version:            $VERSION/" services/web/kpbj-api.cabal

  # Verify the cabal change
  CABAL_VERSION=$(grep -oP '^version:\s*\K[0-9]+\.[0-9]+\.[0-9]+' services/web/kpbj-api.cabal)
  if [ "$CABAL_VERSION" != "$VERSION" ]; then
    echo "ERROR: Failed to update cabal version"
    exit 1
  fi

  # Commit and push
  git add services/web/kpbj-api.cabal CHANGELOG.md
  git commit -m "chore: bump version to $VERSION"
  git push -u origin "$BRANCH"

  # Create PR with release notes using a temp file (avoids Just parsing issues)
  echo "Creating pull request..."
  TMPFILE=$(mktemp)
  {
    echo "## Release v${VERSION}"
    echo ""
    echo "This PR releases version ${VERSION}"
    echo ""
    echo "### What's Changed"
    echo ""
    echo "$RELEASE_NOTES"
    echo ""
    echo "---"
    echo ""
    echo "When merged, a git tag v${VERSION} will be automatically created, which triggers the production deployment."
  } > "$TMPFILE"

  gh pr create \
    --title "Release v$VERSION" \
    --body-file "$TMPFILE" \
    --base main

  rm -f "$TMPFILE"

  echo ""
  echo "Release PR created with release notes from CHANGELOG.md!"
  echo "Once merged, v$VERSION will be automatically tagged and deployed to production."

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

#-------------------------------------------------------------------------------
## Fly.io Staging

STAGING_APP := "kpbj-fm-staging"
STAGING_DB_APP := "kpbj-postgres-staging"
STAGING_DB_NAME := "kpbj_fm_staging"
STAGING_CONFIG := "services/web/fly.staging.toml"

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
  @(cat services/web/mock-data/00_init.sql services/web/mock-data/01_admin_user.sql services/web/mock-data/02_shows.sql services/web/mock-data/03_show_tags.sql services/web/mock-data/04_show_tag_assignments.sql services/web/mock-data/05_host_users.sql services/web/mock-data/06_host_user_metadata.sql services/web/mock-data/07_show_hosts.sql services/web/mock-data/08_host_details.sql services/web/mock-data/09_schedule_templates.sql services/web/mock-data/10_episodes.sql services/web/mock-data/11_staff_and_users.sql services/web/mock-data/12_events.sql services/web/mock-data/13_blog_tags.sql services/web/mock-data/14_blog_posts.sql services/web/mock-data/15_episode_tracks.sql services/web/mock-data/16_summary.sql; echo "\\q") | fly postgres connect --app {{STAGING_DB_APP}} -d {{STAGING_DB_NAME}}
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
  cp services/web/mock-data/media/avatars/* "$TMPDIR/images/2025/01/01/avatars/" 2>/dev/null || true
  cp services/web/mock-data/media/shows/logos/* "$TMPDIR/images/2025/01/01/logos/" 2>/dev/null || true
  cp services/web/mock-data/media/shows/banners/* "$TMPDIR/images/2025/01/01/banners/" 2>/dev/null || true
  cp services/web/mock-data/media/episodes/artwork/* "$TMPDIR/images/2025/01/01/artwork/" 2>/dev/null || true
  cp services/web/mock-data/media/events/posters/* "$TMPDIR/images/2025/01/01/event-posters/" 2>/dev/null || true
  cp services/web/mock-data/media/blog/heroes/* "$TMPDIR/images/2025/01/01/blog-heroes/" 2>/dev/null || true

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

# Upload mock images to Tigris bucket (S3-compatible)
# Requires AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY to be set
staging-mock-images-tigris AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY:
  #!/usr/bin/env bash
  set -euo pipefail
  echo "🖼️  Uploading mock images to Tigris..."
  echo ""

  BUCKET="staging-kpbj-storage"
  ENDPOINT="https://fly.storage.tigris.dev"

  # Export credentials for AWS CLI
  export AWS_ACCESS_KEY_ID="{{AWS_ACCESS_KEY_ID}}"
  export AWS_SECRET_ACCESS_KEY="{{AWS_SECRET_ACCESS_KEY}}"

  echo "  Using AWS_ACCESS_KEY_ID: ${AWS_ACCESS_KEY_ID:0:8}..."

  # Create a temporary directory with the correct structure
  TMPDIR=$(mktemp -d)
  trap "rm -rf $TMPDIR" EXIT

  # Copy files to temp directory with correct structure
  mkdir -p "$TMPDIR/images/2025/01/01"
  cp -r services/web/mock-data/media/avatars "$TMPDIR/images/2025/01/01/" 2>/dev/null || true
  cp -r services/web/mock-data/media/shows/logos "$TMPDIR/images/2025/01/01/" 2>/dev/null || true
  cp -r services/web/mock-data/media/shows/banners "$TMPDIR/images/2025/01/01/" 2>/dev/null || true
  cp -r services/web/mock-data/media/episodes/artwork "$TMPDIR/images/2025/01/01/" 2>/dev/null || true
  cp -r services/web/mock-data/media/events/posters "$TMPDIR/images/2025/01/01/event-posters" 2>/dev/null || true
  cp -r services/web/mock-data/media/blog/heroes "$TMPDIR/images/2025/01/01/blog-heroes" 2>/dev/null || true

  FILE_COUNT=$(find "$TMPDIR" -type f | wc -l)
  echo "  Prepared $FILE_COUNT files for upload"

  # Sync to Tigris using AWS CLI
  echo "  Uploading to Tigris bucket: $BUCKET..."
  aws s3 sync "$TMPDIR/images" "s3://$BUCKET/images" \
    --endpoint-url "$ENDPOINT"

  echo ""
  echo "✨ Mock images uploaded to Tigris successfully!"
  echo "   Files available at: https://$BUCKET.fly.storage.tigris.dev/images/..."

# Load all mock data (database + images) into staging
staging-mock-all: staging-mock-images staging-mock-data
  @echo "🎉 All staging mock data loaded!"

# Load all mock data (database + Tigris images) into staging
# Usage: just staging-mock-all-tigris $AWS_ACCESS_KEY_ID $AWS_SECRET_ACCESS_KEY
staging-mock-all-tigris AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY: (staging-mock-images-tigris AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY) staging-mock-data
  @echo "🎉 All staging mock data loaded (using Tigris)!"

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
  DATABASE_URL='postgres://postgres:{{env_var("STAGING_DB_PASSWORD")}}@localhost:15432/{{STAGING_DB_NAME}}' sqlx migrate run --source services/web/migrations
  @echo "Restarting staging app..."
  fly scale count 1 --app {{STAGING_APP}} --yes
  @just staging-proxy-close
  @echo "✨ Staging database reset complete!"

# Run migrations on staging (via proxy)
staging-migrations-run:
  @echo "Starting proxy in background..."
  fly proxy 15432:5432 -a {{STAGING_DB_APP}} &
  @sleep 2
  DATABASE_URL='postgres://postgres:{{env_var("STAGING_DB_PASSWORD")}}@localhost:15432/{{STAGING_DB_NAME}}' sqlx migrate run --source services/web/migrations
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
PROD_CONFIG := "services/web/fly.toml"

PROD_TIGRIS_BUCKET := "production-kpbj-storage"
STAGING_TIGRIS_BUCKET := "staging-kpbj-storage"
TIGRIS_ENDPOINT := "https://fly.storage.tigris.dev"

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
  DATABASE_URL='postgres://postgres:{{env_var("PROD_DB_PASSWORD")}}@localhost:15432/{{PROD_DB_NAME}}' sqlx migrate run --source services/web/migrations
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

# Backup production database to local file
# Requires PROD_DB_PASSWORD env var
prod-backup:
  #!/usr/bin/env bash
  set -euo pipefail
  mkdir -p backups
  TIMESTAMP=$(date +%Y%m%d_%H%M%S)
  BACKUP_FILE="backups/kpbj_fm_${TIMESTAMP}.dump"
  echo "🗄️  Backing up production database..."
  echo "   Starting proxy..."
  fly proxy 15432:5432 -a {{PROD_DB_APP}} &
  PROXY_PID=$!
  sleep 2
  echo "   Running pg_dump..."
  PGPASSWORD="${PROD_DB_PASSWORD}" pg_dump -h localhost -p 15432 -U kpbj_fm -Fc {{PROD_DB_NAME}} > "$BACKUP_FILE"
  kill $PROXY_PID 2>/dev/null || true
  echo "✨ Backup saved to: $BACKUP_FILE ($(du -h "$BACKUP_FILE" | cut -f1))"

#-------------------------------------------------------------------------------
## Production to Staging Copy

# Copy production database to staging with PII sanitization
# Requires PROD_DB_PASSWORD and STAGING_DB_PASSWORD env vars
prod-to-staging-db:
  ./scripts/prod-to-staging-db.sh

# Copy production Tigris S3 bucket to staging
# Usage: just prod-to-staging-s3 <PROD_AWS_KEY_ID> <PROD_AWS_SECRET_KEY>
prod-to-staging-s3 PROD_AWS_ACCESS_KEY_ID PROD_AWS_SECRET_ACCESS_KEY:
  ./scripts/prod-to-staging-s3.sh "{{PROD_AWS_ACCESS_KEY_ID}}" "{{PROD_AWS_SECRET_ACCESS_KEY}}"

# Copy both production database and S3 bucket to staging
# Usage: just prod-to-staging <PROD_AWS_KEY_ID> <PROD_AWS_SECRET_KEY>
prod-to-staging PROD_AWS_ACCESS_KEY_ID PROD_AWS_SECRET_ACCESS_KEY:
  ./scripts/prod-to-staging.sh "{{PROD_AWS_ACCESS_KEY_ID}}" "{{PROD_AWS_SECRET_ACCESS_KEY}}"

#-------------------------------------------------------------------------------
## Production to Local Dev Copy

# Copy production database to local dev with PII sanitization
# Requires PROD_DB_PASSWORD env var and local postgres running (just postgres-dev-start)
prod-to-local-db:
  ./scripts/prod-to-local-db.sh

# Copy production Tigris S3 files to local dev (/tmp/kpbj)
# Usage: just prod-to-local-files <PROD_AWS_KEY_ID> <PROD_AWS_SECRET_KEY>
prod-to-local-files PROD_AWS_ACCESS_KEY_ID PROD_AWS_SECRET_ACCESS_KEY:
  ./scripts/prod-to-local-files.sh "{{PROD_AWS_ACCESS_KEY_ID}}" "{{PROD_AWS_SECRET_ACCESS_KEY}}"

# Copy both production database and files to local dev
# Usage: just prod-to-local <PROD_AWS_KEY_ID> <PROD_AWS_SECRET_KEY>
prod-to-local PROD_AWS_ACCESS_KEY_ID PROD_AWS_SECRET_ACCESS_KEY:
  ./scripts/prod-to-local.sh "{{PROD_AWS_ACCESS_KEY_ID}}" "{{PROD_AWS_SECRET_ACCESS_KEY}}"

#-------------------------------------------------------------------------------
## Streaming Services (Icecast + Liquidsoap)

# Build and load Nix streaming images locally
stream-build:
  #!/usr/bin/env bash
  set -euo pipefail
  echo "Building Icecast image..."
  nix build .#icecast-docker
  docker load -i result
  echo "Building Liquidsoap image..."
  nix build .#liquidsoap-docker
  docker load -i result
  echo "Done! Images loaded into Docker."

# Build and push streaming images to GHCR
stream-publish TAG="latest":
  #!/usr/bin/env bash
  set -euo pipefail
  for pkg in icecast liquidsoap; do
    echo "Building $pkg image with tag {{TAG}}..."
    IMAGE_TAG={{TAG}} nix build .#${pkg}-docker --impure
    image=$(docker load -i result | sed -n 's/Loaded image: //p')
    echo "Pushing $image..."
    docker push "$image"
  done
  echo "Done! Images pushed to GHCR."

# Start local streaming services (Icecast + Liquidsoap)
stream-dev-start:
  docker compose -f services/liquidsoap/docker-compose.yml \
                 -f services/liquidsoap/docker-compose.dev.yml up -d

# Stop local streaming services
stream-dev-stop:
  docker compose -f services/liquidsoap/docker-compose.yml \
                 -f services/liquidsoap/docker-compose.dev.yml down

# View local streaming service logs
stream-dev-logs:
  docker compose -f services/liquidsoap/docker-compose.yml \
                 -f services/liquidsoap/docker-compose.dev.yml logs -f

# Restart local streaming services
stream-dev-restart:
  docker compose -f services/liquidsoap/docker-compose.yml \
                 -f services/liquidsoap/docker-compose.dev.yml restart

# View local streaming service status
stream-dev-status:
  docker compose -f services/liquidsoap/docker-compose.yml \
                 -f services/liquidsoap/docker-compose.dev.yml ps

# Deploy streaming services to staging VPS
# Requires STAGING_STREAM_HOST env var (e.g., deploy@staging-stream.kpbj.fm)
stream-staging-deploy TAG="latest":
  #!/usr/bin/env bash
  set -euo pipefail
  if [ -z "${STAGING_STREAM_HOST:-}" ]; then
    echo "ERROR: STAGING_STREAM_HOST environment variable is required"
    echo "Example: STAGING_STREAM_HOST=deploy@staging-stream.kpbj.fm just stream-staging-deploy"
    exit 1
  fi
  echo "Deploying streaming services to staging (tag: {{TAG}})..."
  ssh "$STAGING_STREAM_HOST" "cd /opt/kpbj-stream && \
    echo 'IMAGE_TAG={{TAG}}' > .env.tag && \
    docker compose -f docker-compose.yml -f docker-compose.staging.yml pull && \
    docker compose -f docker-compose.yml -f docker-compose.staging.yml up -d"
  echo "Done!"

# Deploy streaming services to production VPS
# Requires PROD_STREAM_HOST env var (e.g., deploy@stream.kpbj.fm)
stream-prod-deploy TAG="latest":
  #!/usr/bin/env bash
  set -euo pipefail
  if [ -z "${PROD_STREAM_HOST:-}" ]; then
    echo "ERROR: PROD_STREAM_HOST environment variable is required"
    echo "Example: PROD_STREAM_HOST=deploy@stream.kpbj.fm just stream-prod-deploy"
    exit 1
  fi
  echo "Deploying streaming services to production (tag: {{TAG}})..."
  ssh "$PROD_STREAM_HOST" "cd /opt/kpbj-stream && \
    echo 'IMAGE_TAG={{TAG}}' > .env.tag && \
    docker compose -f docker-compose.yml -f docker-compose.prod.yml pull && \
    docker compose -f docker-compose.yml -f docker-compose.prod.yml up -d"
  echo "Done!"
