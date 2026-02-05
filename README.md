# KPBJ 95.9FM 📻

Community radio station website for Shadow Hills CA.

**Web Service** — Haskell/Servant app with server-side HTML (Lucid2 + HTMX). Handles the website, host dashboard, and playout API. Deployed to Fly.io with PostgreSQL and Tigris S3.

**Liquidsoap** — Audio automation. Polls the web service for scheduled episodes and streams to Icecast. Runs on a VPS.

**Icecast** — Streaming server. Serves the live audio stream to listeners. Runs alongside Liquidsoap on the VPS.

See [ARCHITECTURE.md](ARCHITECTURE.md) for full system topology.

## Development

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- Docker (for PostgreSQL)

### Environment Variables

Create a `.envrc.local` file with the following env vars:

```bash
# Required
export APP_ENVIRONMENT="Development"
export APP_HOSTNAME="localhost"
export APP_POSTGRES_DB="dev_db"
export APP_POSTGRES_HOST="localhost"
export APP_POSTGRES_PASSWORD="postgres"
export APP_POSTGRES_PORT="5433"
export APP_POSTGRES_USER="postgres"
export DATABASE_URL="postgresql://postgres@localhost:5433/dev_db" # Used by SqlX
export APP_WARP_PORT="4000"
export APP_WARP_SERVERNAME="localhost"
export APP_WARP_TIMEOUT="100"

# Optional - enables email features (password reset, verification)
export APP_SMTP_SERVER="smtp.gmail.com"
export APP_SMTP_PORT="587"
export APP_SMTP_USERNAME="your-email@gmail.com"
export APP_SMTP_PASSWORD="your-app-password"
export APP_SMTP_FROM_EMAIL="noreply@kpbj.fm"
export APP_SMTP_FROM_NAME="KPBJ 95.9FM"
export APP_BASE_URL="http://localhost:4000"

# Optional - for streaming integration
export PLAYOUT_SECRET="your-secret"
```

In development, the app uses local filesystem storage (`/tmp/kpbj`).

### Setup

```bash
nix develop                 # Enter dev shell
just dev-postgres-start     # Start PostgreSQL
just dev-migrations-run     # Run migrations
just dev-mock-data          # Load test data (optional)
just run                    # Start server at localhost:4000
```

### Commands

```bash
just build              # Build all packages
just test               # Run tests
just format-changed     # Format changed files (ormolu + nixpkgs-fmt)
just hlint-changed      # Lint changed Haskell files
```

### Streaming (optional)

To run the full stack locally with Liquidsoap and Icecast:

```bash
just stream-dev-build    # Build Docker images with Nix
just stream-dev-start    # Start Liquidsoap + Icecast
just stream-dev-logs     # View logs
just stream-dev-stop     # Stop services
```

Stream available at `http://localhost:8000/stream`. Requires the web service running.

## Deployment

Automated via GitHub Actions and Fly.io.

| Environment | URL                     | Trigger              |
|-------------|-------------------------|----------------------|
| Staging     | https://staging.kpbj.fm | Push to `main`       |
| Production  | https://www.kpbj.fm     | Git tag (`v*`)       |

**Staging** deploys automatically when you merge to `main`.

### Releasing to Production

Releases are cut from `main` and use semantic versioning (`X.Y.Z`).

**1. Update the changelog**

Before releasing, make sure `CHANGELOG.md` has your changes under the `[Unreleased]` section. The release script pulls from here.

```bash
just release-notes-preview   # See what will be included
```

**2. Create the release PR**

```bash
just release-pr 0.3.2
```

This will:
- Create a `release/0.3.2` branch from `main`
- Move `[Unreleased]` changes to `[0.3.2]` in the changelog
- Bump the version in `kpbj-api.cabal`
- Push and open a PR

**3. Merge the PR**

Review the PR, then merge it. GitHub Actions will automatically create the `v0.3.2` git tag.

**4. Deploy to production**

Trigger the production deployment manually:

```bash
gh workflow run "Deploy to Production" -f tag=v0.3.2
```

### Operations

```bash
just staging-logs / just prod-logs      # View logs
just staging-status / just prod-status  # Deployment status
just staging-ssh / just prod-ssh        # SSH into container
```

### Streaming

Liquidsoap and Icecast run on a separate VPS. See [services/liquidsoap/README.md](services/liquidsoap/README.md) for details.

```bash
just stream-publish sha-abc123          # Build and push images to GHCR
just stream-staging-deploy sha-abc123   # Deploy to staging VPS
just stream-prod-deploy sha-abc123      # Deploy to production VPS
```
