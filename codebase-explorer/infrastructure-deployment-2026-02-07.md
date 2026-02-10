# KPBJ.fm Infrastructure and Deployment Architecture

## Architecture Overview

KPBJ 95.9FM is a community radio station with a three-service architecture:

1. **Web Service** - Haskell/Servant application (Fly.io) serving website, dashboard, and playout API
2. **Liquidsoap** - Audio automation daemon (VPS) for radio stream management  
3. **Icecast** - Streaming server (VPS) serving audio to listeners

### Service Communication Flow

```
Listeners → [HTTPS] → Nginx (VPS) → Icecast (Docker) → [MP3 Stream]
                                         ↑
                                         | MP3
                                         |
                                    Liquidsoap (Docker)
                                         |
                                         | HTTPS Polling (:00, :30)
                                         ↓
Visitors/Hosts → [HTTPS] → Fly.io Load Balancer → Web Service (Docker)
                                                        ↓
                                                   PostgreSQL (Fly.io)
                                                   Tigris S3 (Fly.io)
```

## Service Details

### 1. Web Service (Fly.io)

**Repository Location**: `/services/web/`

**Technology Stack**:
- **Language**: Haskell (GHC 9.10.3)
- **Framework**: Servant (REST API)
- **Frontend**: Server-side HTML with Lucid2 + HTMX
- **Database**: PostgreSQL (Hasql client)
- **Storage**: Tigris S3 (AWS-compatible)
- **Observability**: OpenTelemetry (stdout exporter)

**Build System**: Nix flakes with Docker image generation

**Key Dependencies**:
- `web-server-core` (custom framework from github:solomon-b/web-server)
- `lucid-form-builder`, `lucid-htmx-alpine`, `lucid-tailwind` (local libs)
- `amazonka` (S3 client)
- `hasql`, `hasql-pool`, `hasql-transaction` (database)
- `rel8` (SQL query DSL)

**Configuration Files**:
- `services/web/fly.toml` - Production config (app: `kpbj-fm`)
- `services/web/fly.staging.toml` - Staging config (app: `kpbj-fm-staging`)
- `services/web/docker.nix` - Docker image builder
- `services/web/kpbj-api.cabal` - Haskell package definition (v0.6.0)

**Docker Image**:
- Registry: `ghcr.io/solomon-b/kpbj.fm`
- Tags: `sha-<commit>` (staging), `<version>` + `latest` (production)
- Base: Nix-built static binary with busybox, cacert, sqlx-cli
- Entry: `/bin/kpbj-api` via shell wrapper

**Deployment Configuration** (fly.toml):
- App: `kpbj-fm` (production)
- Region: `lax` (Los Angeles)
- Image: `ghcr.io/solomon-b/kpbj.fm:latest`
- Release command: `sqlx migrate run --source /migrations`
- Ports: 80 (HTTP), 443 (HTTPS) → 4000 (internal)
- Resources: 1GB RAM, 1 shared CPU

**Environment Variables** (from fly.toml):
- `APP_HOSTNAME` - https://www.kpbj.fm
- `APP_ENVIRONMENT` - "Production" or "Staging"
- `APP_WARP_PORT` - 4000
- `APP_GOOGLE_ANALYTICS_GTAG` - G-ST01RRVLDB (prod only)
- `APP_SMTP_*` - Gmail SMTP config
- `AWS_ACCESS_KEY_ID` - Tigris credentials (tid_xxx format)
- `AWS_ENDPOINT_URL_S3` - https://fly.storage.tigris.dev
- `AWS_REGION` - "auto"
- `BUCKET_NAME` - production-kpbj-storage / staging-kpbj-storage

**Secrets** (managed via `fly secrets set`):
- `APP_POSTGRES_PASSWORD` - Database password
- `APP_SMTP_PASSWORD` - Email password
- `AWS_SECRET_ACCESS_KEY` - Tigris secret key
- `PLAYOUT_SECRET` - Shared secret for Liquidsoap API auth

### 2. Liquidsoap (VPS - Docker Compose)

**Repository Location**: `/services/liquidsoap/`

**Purpose**: Radio automation - polls web API for scheduled episodes, plays fallback tracks, outputs MP3 stream

**Configuration**:
- `config/radio.liq` - Main Liquidsoap script (182 lines)
- `docker-compose.yml` - Base configuration
- `docker-compose.{dev,staging,prod}.yml` - Environment overrides

**Docker Image**:
- Registry: `ghcr.io/solomon-b/kpbj-liquidsoap`
- Built with: Nix (`.#liquidsoap-docker` in flake.nix)
- Contents: liquidsoap binary, curl, cacert, bundled radio.liq script
- Command: `liquidsoap /config/radio.liq`

**Key Logic** (from radio.liq):
1. Polls `/api/playout/now` at :00 and :30 each hour
2. Maintains `last_scheduled` state to prevent replays
3. Falls back to `/api/playout/fallback` for random tracks
4. POSTs play history to `/api/playout/played`
5. Crossfades between tracks (1s fade)
6. Re-sends metadata on Icecast reconnect

**Environment Variables**:
- `API_BASE` - Web service playout API URL
  - Dev: `http://host.docker.internal:4000/api/playout`
  - Staging: `https://staging.kpbj.fm/api/playout`
  - Prod: `https://www.kpbj.fm/api/playout`
- `PLAYOUT_SECRET` - Shared secret for API authentication
- `ICECAST_HOST` - "icecast" (Docker service name)
- `ICECAST_PORT` - 8000
- `ICECAST_PASSWORD` - Source password for Icecast
- `ICECAST_MOUNT` - "/stream"

**Network**:
- No exposed ports (internal to Docker network)
- Depends on Icecast service (healthcheck)

### 3. Icecast (VPS - Docker Compose)

**Repository Location**: `/services/liquidsoap/config/icecast.xml`

**Purpose**: Streaming server - accepts MP3 from Liquidsoap, serves to listeners

**Docker Image**:
- Registry: `ghcr.io/solomon-b/kpbj-icecast`
- Built with: Nix (`.#icecast-docker` in flake.nix)
- Contents: icecast binary, config template, entrypoint script
- Config: Template at `/config/icecast.xml.template`, copied to `/tmp/icecast.xml` and modified by entrypoint

**Configuration**:
- Max clients: 100
- Max sources: 2
- Mount point: `/stream`
- Bitrate: 128kbps MP3
- Metadata: "KPBJ 95.9 FM - Community Radio for Sun Valley and Burbank"

**Environment Variables**:
- `ICECAST_SOURCE_PASSWORD` - Password for Liquidsoap connection
- `ICECAST_ADMIN_PASSWORD` - Admin interface password
- `ICECAST_RELAY_PASSWORD` - Relay password
- `ICECAST_HOSTNAME` - Public hostname (default: localhost)
- `ICECAST_MAX_CLIENTS` - Default: 100
- `ICECAST_MAX_SOURCES` - Default: 2

**Network**:
- Exposed ports:
  - Dev: 8000:8000
  - Staging: 8001:8000 (avoid conflict)
  - Prod: 8000:8000
- Healthcheck: `curl http://localhost:8000/status-json.xsl`

**TLS/HTTPS**: Nginx reverse proxy on VPS provides TLS termination using Let's Encrypt (certbot)

### 4. Webhook Service (VPS - Docker Compose)

**Purpose**: Allows remote restart of Icecast/Liquidsoap containers via authenticated webhooks

**Docker Image**:
- Registry: `ghcr.io/solomon-b/kpbj-webhook`
- Built with: Nix (`.#webhook-docker` in flake.nix)
- Contents: webhook binary, docker CLI client

**Configuration**: `services/liquidsoap/hooks.yaml`
- `restart-icecast` - Restarts Icecast container
- `restart-liquidsoap` - Restarts Liquidsoap container
- `health` - Health check endpoint

**Network**:
- Dev: 9000:9000 (public)
- Staging: 127.0.0.1:9001:9000 (localhost only)
- Prod: 127.0.0.1:9000:9000 (localhost only)

**Volumes**:
- `/var/run/docker.sock:/var/run/docker.sock` - Access to Docker daemon
- `./hooks.yaml:/config/hooks.yaml:ro` - Hook definitions

## Network Topology

### Environments

| Environment | Web Service | Stream | Stream Metadata |
|-------------|-------------|--------|-----------------|
| Local | http://localhost:4000 | http://localhost:8000/stream | http://localhost:4000/api/stream/metadata |
| Staging | https://staging.kpbj.fm | https://stream.staging.kpbj.fm/stream | https://staging.kpbj.fm/api/stream/metadata |
| Production | https://www.kpbj.fm | https://stream.kpbj.fm/stream | https://www.kpbj.fm/api/stream/metadata |

### Port Mappings

**Fly.io (Web Service)**:
- 80 → 4000 (HTTP redirect to HTTPS)
- 443 → 4000 (HTTPS/TLS)
- Internal port: 4000 (Warp server)

**VPS (Streaming)**:

Production:
- 8000 (Icecast internal)
- Nginx: 80/443 → 8000 (stream.kpbj.fm)
- Webhook: 127.0.0.1:9000

Staging:
- 8001 → 8000 (Icecast internal, avoid port conflict)
- Nginx: 80/443 → 8001 (stream.staging.kpbj.fm)
- Webhook: 127.0.0.1:9001

**Docker Networks**:
- All services on same Docker Compose network
- Liquidsoap → Icecast via service name: `icecast:8000`
- Liquidsoap → Web API via public HTTPS (polls from outside)

### DNS Configuration

**Expected DNS Records** (not in repo, manual setup):
```
www.kpbj.fm              → Fly.io (managed by Fly.io)
staging.kpbj.fm          → Fly.io (managed by Fly.io)
stream.kpbj.fm           → VPS IP address (A record)
stream.staging.kpbj.fm   → VPS IP address (A record)
```

### TLS/SSL Certificates

**Fly.io**: Automatic TLS via Fly.io platform (managed)

**VPS (Nginx)**: Let's Encrypt via certbot
- Certificates stored at: `/etc/letsencrypt/live/stream.{staging.,}kpbj.fm/`
- Renewal: Automatic via certbot systemd timer
- Setup commands documented in `services/liquidsoap/README.md`

## Storage Configuration

### PostgreSQL (Fly.io Managed)

**Staging**:
- App: `kpbj-postgres-staging`
- Database: `kpbj_fm_staging`
- Connection: Via `DATABASE_URL` secret (Fly.io internal network)
- Proxy port: 15432 (for local connections)

**Production**:
- App: `kpbj-postgres`
- Database: `kpbj_fm`
- Connection: Via `DATABASE_URL` secret (Fly.io internal network)
- Proxy port: 15433 (for local connections)

**Schema**: 34 migrations (sqlx format) in `services/web/migrations/`
- Users, sessions, metadata, host details
- Shows, schedules, episodes, tracks
- Blog posts, tags, show blogs
- Events (tags removed in recent migration)
- Ephemeral uploads, soft deletes

**Migration Management**:
- Tool: `sqlx-cli` (bundled in Docker image)
- Release command: `sqlx migrate run --source /migrations` (runs on deploy)
- Local dev: `just dev-migrations-run`

### File Storage

**Development**: Local filesystem at `/tmp/kpbj`
- Hardcoded in code: "In Development, the app always uses local storage regardless of S3 env vars"

**Staging/Production**: Tigris S3 (Fly.io managed, AWS S3-compatible)

Staging:
- Bucket: `staging-kpbj-storage`
- Access Key ID: `tid_HYzcxENXqyQRdEGrGMdkOzzLtcAeDRETMOrznzIMieQUHOnLwj`
- Endpoint: `https://fly.storage.tigris.dev`

Production:
- Bucket: `production-kpbj-storage`
- Access Key ID: `tid_CuEAcDzWgbwrUbKtuPPHDtBtEkJkoMuYeNHYDIamIYjTXAiLpf`
- Endpoint: `https://fly.storage.tigris.dev`

**File Structure** (from CLAUDE.md):
```
media/
  episodes/{slug}/{YYYY}/{MM}/{DD}/{type}/
  shows/{slug}/{YYYY}/{MM}/{DD}/{type}/
  events/{slug}/{YYYY}/{MM}/{DD}/{type}/
```

**Upload Pattern**: Two-phase staged upload
1. Upload to staging area with UUID token
2. Claim with `claimAndRelocateUpload` to move to final location

**Max Upload Size**: 500MB (fly.toml: `max_request_body_size = 524288000`)

### VPS Storage

**Streaming Services**: No persistent storage needed
- Icecast/Liquidsoap are stateless
- Logs written to `/tmp` (ephemeral)
- Only persistent data: `.env` file at `/opt/kpbj-stream/.env`

**Deployment Directory**: `/opt/kpbj-stream/`
```
/opt/kpbj-stream/
├── .env                      # Auto-generated secrets
├── .env.tag                  # IMAGE_TAG for current deployment
├── docker-compose.yml        # Base config (copied from repo)
├── docker-compose.{env}.yml  # Environment overrides (copied from repo)
└── hooks.yaml                # Webhook config (copied from repo)
```

## Secrets Management

### Web Service Secrets (Fly.io)

**Managed via**: `fly secrets set` command

**Required Secrets**:
- `APP_POSTGRES_PASSWORD` - Database password (generated by Fly.io)
- `AWS_SECRET_ACCESS_KEY` - Tigris secret key (generated by Tigris)
- `APP_SMTP_PASSWORD` - Gmail app password for email
- `PLAYOUT_SECRET` - Shared secret for Liquidsoap authentication

**Public/Non-Secret Config** (in fly.toml):
- AWS Access Key IDs (Tigris public IDs, not sensitive)
- SMTP username/from addresses
- Base URLs, app name, region

### Streaming Service Secrets (VPS)

**Managed via**: Auto-generated `.env` file on first deploy

**Location**: `/opt/kpbj-stream/.env` on VPS

**Auto-Generated Variables**:
```bash
ICECAST_SOURCE_PASSWORD=<random-32-char>
ICECAST_ADMIN_PASSWORD=<random-32-char>
ICECAST_RELAY_PASSWORD=<random-32-char>
WEBHOOK_SECRET=<random-32-char>
```

**Generation Logic** (from `stream-deploy.sh`):
- On first deploy, generates 32-character random passwords
- Stored in `.env` file with 600 permissions
- WEBHOOK_SECRET added retroactively if missing in existing .env

**Manual Configuration**:
- `PLAYOUT_SECRET` - Must match web service secret (manual setup)

## CI/CD Pipeline

### GitHub Actions Workflows

**Location**: `.github/workflows/`

#### 1. `nix.yaml` - Build Validation
- **Trigger**: Push, Pull Request (all branches)
- **Purpose**: Validate Nix build on CI
- **Cachix**: kpbj-fm cache for faster builds

#### 2. `deploy-staging.yaml` - Automatic Staging Deploy
- **Trigger**: Push to `main` branch
- **Image Tag**: `sha-<commit>`
- **Registry**: `ghcr.io/solomon-b/kpbj.fm`
- **Steps**: Build with Nix → Push to GHCR → Deploy to Fly.io staging

#### 3. `deploy-production.yaml` - Manual Production Deploy
- **Trigger**: Push to tag `v*` OR manual workflow dispatch
- **Image Tags**: `<version>` + `latest`
- **Deploys**: Fly.io production (`kpbj-fm`)

#### 4. `create-release-tag.yaml` - Automated Git Tag Creation
- **Trigger**: Pull request merged from `release/*` branch
- **Purpose**: Create git tag `v<version>` after release PR merge
- **Validation**: Ensures version in branch name matches kpbj-api.cabal

#### 5. `stream-images.yml` - Manual Streaming Images Build
- **Trigger**: Manual workflow dispatch
- **Builds**: Icecast, Liquidsoap, Webhook images
- **Tags**: Input tag OR `sha-<short-commit>`, plus `latest`

### Release Process

1. Update CHANGELOG.md with `[Unreleased]` changes
2. Create release PR: `just release-pr 0.3.2`
3. Merge PR → Automatic tag creation (`v0.3.2`)
4. Tag push triggers production deploy OR manual trigger:
   ```bash
   gh workflow run "Deploy to Production" -f tag=v0.3.2
   ```

### Deployment to Streaming VPS

**Manual SSH deployment** (not automated)

1. Build and push images:
   ```bash
   just stream-publish sha-abc123
   ```

2. Deploy to staging:
   ```bash
   export STAGING_STREAM_HOST=deploy@staging-stream.kpbj.fm
   just stream-staging-deploy sha-abc123
   ```

3. Deploy to production:
   ```bash
   export PROD_STREAM_HOST=deploy@stream.kpbj.fm
   just stream-prod-deploy sha-abc123
   ```

**Script**: `services/liquidsoap/scripts/stream-deploy.sh`
- SSH to VPS
- Copy compose files
- Auto-generate secrets if missing
- Pull images and restart containers

## Build System

### Nix Flake Configuration

**File**: `flake.nix`

**Inputs**:
- `nixpkgs` - nixos-25.05 channel
- `flake-utils` - Cross-platform build utilities
- `web-server-core` - Custom Haskell web framework (github:solomon-b/web-server)

**Outputs**:
1. **Development Shell** (`nix develop`)
   - GHC 9.10.3 + Cabal
   - HLS, hlint, weeder, ormolu 0.7.2.0
   - flyctl, just, sqlx-cli, rclone

2. **Packages** (`nix build`)
   - `.#kpbj-api` - Haskell executable
   - `.#docker` - Web service Docker image
   - `.#icecast-docker` - Icecast Docker image
   - `.#liquidsoap-docker` - Liquidsoap Docker image
   - `.#webhook-docker` - Webhook Docker image

**Haskell Package Overrides**:
- `amazonka` family from GitHub (GHC 9.10 compat)
- `hasql` 1.9.x series
- `rel8` 1.7.0.0 (hasql 1.9 compat)
- `tmp-postgres` from master
- Custom libs: `lucid-form-builder`, `lucid-htmx-alpine`, `lucid-tailwind`

**Docker Image Build**:
- Static binary via `justStaticExecutables`
- Minimal base with busybox, cacert, sqlx-cli
- Configurable tag via `IMAGE_TAG` env var (requires `--impure`)

### Justfile Commands

**File**: `Justfile` (593 lines total)

**Key Configuration**:
```
DEV_DB_PORT := "5433"
LOCAL_STORAGE_ROOT := "/tmp/kpbj"
STAGING_PROXY_PORT := "15432"
PROD_PROXY_PORT := "15433"
```

**Command Categories**:

1. **Build & Run**: `run`, `build`, `test`, `clean`
2. **Formatting**: `format-hs`, `format-nix`, `hlint`, `shellcheck`
3. **Local Dev**: `dev-postgres-*`, `dev-migrations-*`, `dev-mock-*`
4. **Staging**: `staging-deploy`, `staging-logs`, `staging-ssh`, `staging-psql`
5. **Production**: `prod-deploy`, `prod-logs`, `prod-ssh`, `prod-backup`
6. **Streaming**: `stream-dev-*`, `stream-publish`, `stream-*-deploy`
7. **Data Migration**: `prod-to-staging`, `prod-to-local`
8. **Release**: `release-pr`, `release-notes-preview`

## Infrastructure as Code Status

**Current State**: **No IaC in repository**

Despite the repository name (`kpbj.fm.terraform`), **no Terraform files exist**:
```bash
$ find . -name "*.tf" -o -name "terraform.tfvars"
# No results
```

**Implications**:
- Fly.io infrastructure created manually (via `flyctl` or web console)
- VPS setup is manual (Docker + nginx + certbot)
- DNS records configured manually (not documented in repo)
- No automated infrastructure provisioning
- No drift detection or state management

**What Would Need IaC**:
- Fly.io apps, PostgreSQL, volumes, secrets
- Tigris S3 buckets
- VPS provisioning (if using cloud provider)
- DNS records (A records for stream.kpbj.fm)
- TLS certificates (Let's Encrypt via Terraform ACME provider)

## VPS Setup Documentation

**From**: `services/liquidsoap/README.md`

**One-time VPS Setup**:
```bash
# Install Docker
curl -fsSL https://get.docker.com | sh
usermod -aG docker deploy

# Create deployment directory
mkdir -p /opt/kpbj-stream
cd /opt/kpbj-stream

# Login to GHCR
docker login ghcr.io
```

**Nginx Configuration** (manual, not in repo):

Get certificate:
```bash
sudo certbot certonly --nginx -d stream.staging.kpbj.fm
```

Create `/etc/nginx/sites-available/stream.staging.kpbj.fm` with SSL config, proxy settings, and streaming optimizations (buffering off, long timeouts).

## Development Workflow

### Local Development Setup

1. Enter Nix shell: `nix develop`
2. Create `.envrc.local` with environment variables
3. Start PostgreSQL: `just dev-postgres-start`
4. Run migrations: `just dev-migrations-run`
5. Load test data: `just dev-mock-data` (optional)
6. Start web service: `just run`

**Development Storage**: Local filesystem at `/tmp/kpbj`

### Running Full Stack Locally

```bash
# Build images
just stream-dev-build

# Start Icecast + Liquidsoap
just stream-dev-start

# Stream available at:
# http://localhost:8000/stream
```

**Requires**: Web service running on port 4000 (Liquidsoap polls it)

## Backup and Recovery

### Database Backups

**Production Backup** (Local):
```bash
# Requires PROD_DB_PASSWORD env var
just prod-backup
# Creates: backups/kpbj_fm_<timestamp>.dump (pg_dump -Fc format)
```

**Remote Backup** (from TrueNAS or other remote server):
```bash
# Requires: FLY_API_TOKEN, PROD_DB_PASSWORD
just prod-backup-remote
# Script: scripts/prod-backup-remote.sh
# Uses: fly proxy + pg_dump + rotation (default 14 days)
```

### S3 Backups

```bash
# Requires: AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY
just prod-backup-s3
# Script: scripts/prod-backup-s3.sh
# Uses: rclone with date-based snapshots (default 14 days retention)
```

### Data Cloning

```bash
# Clone production to staging (both DB and S3)
just prod-to-staging

# Clone production to local dev
just prod-to-local
```

## Monitoring and Observability

### Logging

**Web Service** (Fly.io):
```bash
just staging-logs  # or: fly logs --app kpbj-fm-staging
just prod-logs     # or: fly logs --app kpbj-fm
```

**Streaming Services** (VPS):
```bash
# SSH to VPS and run:
cd /opt/kpbj-stream
docker compose logs -f icecast
docker compose logs -f liquidsoap
```

### Observability Configuration

**From fly.toml**:
```toml
APP_OBSERVABILITY_EXPORTER = 'StdOut'
APP_OBSERVABILITY_VERBOSITY = 'Brief'
```

**OpenTelemetry traces sent to stdout** (captured by Fly.io logs)

**No External APM**: No Honeycomb, Datadog, or other integration configured

### Health Checks

**Icecast**:
```bash
curl http://localhost:8000/status-json.xsl
```

**Liquidsoap**: Telnet interface on port 1234 (not exposed)

**Web Service**: Fly.io automatic health checks on port 4000

### Analytics

**Production Only**:
```toml
APP_GOOGLE_ANALYTICS_GTAG = 'G-ST01RRVLDB'
```

Staging has no analytics configured.

## Security Considerations

### Secrets in Version Control

**FOUND**: Tigris Access Key IDs in fly.toml
- These are public IDs (tid_xxx format), not secret keys
- AWS-style separation: Access Key ID (public) + Secret Access Key (secret)

**NOT FOUND**: No actual secrets in repo
- `.env` files in `.gitignore`
- Secrets managed via Fly.io or auto-generated on VPS

### API Authentication

**Playout API** (`/api/playout/*`):
- Protected by `X-Playout-Secret` header
- Shared secret between web service and Liquidsoap

**Webhook Endpoints**:
- Protected by `X-Webhook-Secret` header
- Bound to localhost on production/staging

**User Authentication**:
- Session-based with cookies
- JWK stored at `services/web/jwk.json`

### Network Security

**Web Service**:
- TLS termination at Fly.io edge
- No direct database access from outside Fly.io network
- S3 access via Tigris internal network

**Streaming VPS**:
- Nginx TLS termination (Let's Encrypt)
- Icecast bound to localhost (only Nginx can access)
- Webhook bound to localhost
- Docker socket exposed to webhook container

### Dependency Management

**Nix Flakes**: Locked dependencies in `flake.lock`
- Reproducible builds
- SHA256 verification of all inputs

**Cabal**: Specific version constraints in `cabal.project`
- Pinned Git commits for dependencies

**Docker Images**: Base images from Nixpkgs (trusted source)

## Operational Runbooks

### Deploying a New Release

1. Update CHANGELOG.md with changes under `[Unreleased]`
2. Create release PR: `just release-pr 0.3.3`
3. Review and merge PR
4. Wait for automatic tag creation (GitHub Actions)
5. Monitor production deploy: `fly logs --app kpbj-fm`
6. Verify: `fly status --app kpbj-fm`
7. Test: https://www.kpbj.fm

### Updating Streaming Services

1. Make changes to `radio.liq` or `icecast.xml`
2. Commit and push to main
3. Build and push images: `just stream-publish sha-$(git rev-parse --short HEAD)`
4. Deploy to staging: `STAGING_STREAM_HOST=deploy@staging.kpbj.fm just stream-staging-deploy sha-abc123`
5. Test: https://stream.staging.kpbj.fm/stream
6. Deploy to production: `PROD_STREAM_HOST=deploy@stream.kpbj.fm just stream-prod-deploy sha-abc123`

### Rollback Production

**Web Service**:
```bash
fly releases --app kpbj-fm
fly releases rollback --app kpbj-fm
```

**Streaming**:
```bash
PROD_STREAM_HOST=deploy@stream.kpbj.fm just stream-prod-deploy sha-previous
```

### Handling Icecast/Liquidsoap Issues

**Icecast not serving stream**:
1. Check logs: `docker logs kpbj-icecast`
2. Verify running: `docker ps | grep icecast`
3. Check healthcheck: `curl http://localhost:8000/status-json.xsl`
4. Restart: `docker restart kpbj-icecast`

**Liquidsoap not playing**:
1. Check logs: `docker logs kpbj-liquidsoap`
2. Verify API connectivity: `curl https://www.kpbj.fm/api/playout/now`
3. Restart: `docker restart kpbj-liquidsoap`

**Remote restart via webhook**:
```bash
curl -X POST -H "X-Webhook-Secret: <secret>"   http://localhost:9000/hooks/restart-icecast
```

## Key Files Reference

### Configuration
- `/flake.nix` - Nix build configuration (325 lines)
- `/Justfile` - Task runner with deployment commands (593 lines)
- `/cabal.project` - Haskell dependencies
- `/services/web/kpbj-api.cabal` - Web service package (v0.6.0)
- `/services/web/fly.toml` - Production Fly.io config
- `/services/web/fly.staging.toml` - Staging Fly.io config
- `/services/web/docker.nix` - Web service Docker builder
- `/services/liquidsoap/docker-compose.yml` - Base streaming config
- `/services/liquidsoap/docker-compose.{dev,staging,prod}.yml` - Environment overrides
- `/services/liquidsoap/config/radio.liq` - Liquidsoap automation script (182 lines)
- `/services/liquidsoap/config/icecast.xml` - Icecast server config
- `/services/liquidsoap/hooks.yaml` - Webhook definitions

### GitHub Actions
- `/.github/workflows/nix.yaml` - Build validation
- `/.github/workflows/deploy-staging.yaml` - Auto-deploy staging
- `/.github/workflows/deploy-production.yaml` - Deploy production on tag
- `/.github/workflows/create-release-tag.yaml` - Auto-create git tags
- `/.github/workflows/stream-images.yml` - Build streaming images

### Scripts
- `/scripts/release-pr.sh` - Create release PR
- `/scripts/prod-backup-remote.sh` - Remote database backup
- `/scripts/prod-backup-s3.sh` - S3 backup with rclone
- `/scripts/prod-to-staging.sh` - Clone prod → staging
- `/scripts/prod-to-local.sh` - Clone prod → local
- `/services/liquidsoap/scripts/stream-deploy.sh` - Deploy streaming to VPS

### Migrations
- `/services/web/migrations/*.sql` - 34 database migrations (sqlx format)

### Documentation
- `/ARCHITECTURE.md` - System topology and service overview
- `/README.md` - Setup, development, deployment guide
- `/CLAUDE.md` - AI coding assistant instructions (project conventions)
- `/CHANGELOG.md` - Version history and release notes (v0.6.0 current)
- `/services/liquidsoap/README.md` - Streaming infrastructure guide

## Open Questions / Gaps

1. **DNS Management**: Where are DNS records configured? Manual via registrar?
2. **VPS Provider**: What cloud provider hosts the streaming VPS?
3. **Backup Monitoring**: Are backups running on schedule?
4. **Secrets Rotation**: How often are secrets rotated?
5. **Certificate Renewal**: Is certbot renewal working?
6. **Metrics Collection**: Any Prometheus or Grafana monitoring?
7. **Alerting**: How is team notified of outages?
8. **Disaster Recovery**: What's the RTO/RPO? Full recovery procedure?
9. **Cost Monitoring**: Fly.io billing alerts? S3 usage monitoring?
10. **Terraform**: Why is repo named `.terraform` with no Terraform files?

## Recommendations

### Infrastructure as Code
1. Create Terraform/OpenTofu configuration for Fly.io resources
2. Version secrets in encrypted form (SOPS, Vault)
3. Document manual VPS setup in IaC format
4. Add DNS management (Cloudflare/AWS Route53 provider)

### CI/CD Improvements
1. Add automated smoke tests after deploy
2. Add deployment notifications (Slack, Discord)
3. Automate streaming service deploys (webhook from GitHub Actions)
4. Add rollback automation

### Security
1. Rotate JWK regularly (document procedure)
2. Add secrets expiry monitoring
3. Document incident response procedures
4. Enable Fly.io audit logs
5. Add fail2ban or similar to VPS

### Observability
1. Add structured logging (JSON format)
2. Integrate with external APM (Honeycomb, Datadog)
3. Add custom metrics (listener count, buffer health)
4. Create Grafana dashboard

### Documentation
1. Document VPS provider details
2. Add runbook for complete system recovery
3. Document cost optimization strategies
4. Add architecture decision records (ADRs)
5. Create onboarding guide for new developers

### Backup and DR
1. Automate backup testing (restore to staging monthly)
2. Document RTO/RPO targets
3. Add backup monitoring/alerting
4. Create off-site backup copy

### Development
1. Add docker-compose for full local stack
2. Add pre-commit hooks (format, lint, test)
3. Add CHANGELOG validation in CI
4. Document debugging procedures

---

**Report Generated**: 2026-02-07  
**Repository**: kpbj.fm.terraform (main branch, terraform)  
**Latest Commit**: da211f6 (feat: Adds full db query test coverage.)  
**Version**: 0.6.0
