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
lint-hs:
  @echo running hlint
  @hlint {{HS_FILES}}

# Lint Haskell source code using hlint (changed files only).
lint-hs-changed:
  @echo running hlint
  @if [ -n "{{CHANGED_HS_FILES}}" ]; then \
  	hlint {{CHANGED_HS_FILES}}; \
  fi

#-------------------------------------------------------------------------------
## Key Gen
# https://ruleoftech.com/2020/generating-jwt-and-jwk-for-information-exchange-between-services

# Generate RSA key for JWK
gen-keys:
  mkdir -p backend/keys
  openssl genrsa -out backend/keys/private.pem 4096
  openssl rsa -in backend/keys/private.pem -out backend/keys/public.pem -pubout
  openssl req -key backend/keys/private.pem -new -x509 -days 3650 -subj "/C=FI/ST=Helsinki/O=Rule of Tech/OU=Information unit/CN=ruleoftech.com" -out backend/keys/cert.pem
  openssl pkcs12 -export -inkey backend/keys/private.pem -in backend/keys/cert.pem -out backend/keys/keys.pfx -name "kpbj-backend"

#-------------------------------------------------------------------------------
## Database

# Connect to local dev db with psql.
psql-dev:
  @psql $DATABASE_URL

# Create a new SQL migration.
migrations-add MIGRATION:
  sqlx migrate add {{MIGRATION}} --source backend/migrations

# Run SQL migrations.
migrations-run:
  sqlx migrate run --source backend/migrations

# Reset PG Database.
migrations-reset:
  sqlx database reset --source backend/migrations

# List all SQL migrations.
migrations-list:
  sqlx migrate info --source migrations

# Build and run a development docker container
postgres-dev-start:
  echo "🟢 Starting the Development Postgres service.."
  docker run \
    --rm \
    --name dev-postgres \
    -d -p 5432:5432 \
    -e POSTGRES_HOST_AUTH_METHOD=trust \
    -e POSTGRES_DB=dev_db \
    -v {{HOME}}/.kpbj-dev-pg-data:/var/lib/postgresql/data \
    -d postgres
  echo "✨ Success!"

# Halt the development docker container
postgres-dev-stop:
  echo "🔴 Stopping the Development Postgres service.."
  docker container stop dev-postgres
  echo ✨ "Success!"

# Connect to the development postgres db with psql.
postgres-dev-psql:
  echo "🌐 Connecting to the Development Postgres service.."
  psql -h localhost -U postgres -d dev_db

# Build and run a test docker container
postgres-test-start:
  echo "🟢 Starting the Test Postgres service.."
  docker run --rm --name test-postgres -d -p 5432:5432 -e POSTGRES_HOST_AUTH_METHOD=trust -e POSTGRES_DB=test_db -d postgres
  echo "✨ Success!"

# Halt the test docker container
postgres-test-stop:
  echo "🔴 Stopping the Test Postgres service.."
  docker container stop test-postgres
  echo ✨ "Success!"

# Connect to the test postgres db with psql.
postgres-test-psql:
  echo "🌐 Connecting to the Test Postgres service.."
  psql -h localhost -U postgres -d test_db

# Load mock data into the development database
mock-data:
  echo "📊 Loading mock data into dev_db..."
  PGPASSWORD=postgres psql -h localhost -U postgres -d dev_db -f mock-data.sql
  echo "✨ Mock data loaded successfully!"


#-------------------------------------------------------------------------------
## Deployment

# Deploy
deploy:
  nix run .#deploy
