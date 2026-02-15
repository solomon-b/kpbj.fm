# KPBJ 95.9FM 📻

Community radio station website for Shadow Hills CA.

**Web Service** — Haskell/Servant app with server-side HTML (Lucid2 + HTMX). Handles the website, host dashboard, and playout API. Runs on a NixOS VPS with PostgreSQL and DigitalOcean Spaces.

**Liquidsoap** — Audio automation. Polls the web service for scheduled episodes and streams to Icecast. Runs on a VPS.

**Icecast** — Streaming server. Serves the live audio stream to listeners. Runs alongside Liquidsoap on the VPS.

See [ARCHITECTURE.md](ARCHITECTURE.md) for full system topology.

## Development

### Prerequisites

- [Nix](https://nixos.org/download.html) with flakes enabled
- Docker (for local PostgreSQL)

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

To run the full streaming stack locally (Icecast + Liquidsoap + Webhook) in a NixOS QEMU VM — the same configuration as staging/prod:

```bash
sops secrets/dev-streaming.yaml   # Create dev streaming secrets (first time only)
just stream-dev-build             # Build the VM
just stream-dev-start             # Start the VM (ports 8000, 9000)
just stream-dev-stop              # Stop the VM
```

Stream available at `http://localhost:8000/stream`. Webhook at `http://localhost:9000`. Requires the web service running on port 4000.

## Deployment

Automated via GitHub Actions and NixOS.

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

### Infrastructure (Terraform)

Infrastructure for DigitalOcean and Cloudflare is codified in `terraform/`. See [terraform/README.md](terraform/README.md) for full setup and import instructions.

```bash
just tf-init                  # Initialize with remote state backend
just tf-plan                  # Preview changes
just tf-apply                 # Apply changes
```

Terraform manages: DigitalOcean streaming droplets (prod + staging) + firewalls, and Cloudflare DNS records + proxy settings.

### Secrets (SOPS)

All secrets are SOPS-encrypted with age keys and stored in `secrets/`:

| File                             | Purpose                                               |
|----------------------------------|-------------------------------------------------------|
| `secrets/terraform.yaml`         | Terraform provider tokens (DO, Cloudflare)            |
| `secrets/dev-streaming.yaml`     | Dev VM Icecast passwords, playout/webhook secrets     |
| `secrets/prod-streaming.yaml`    | Production Icecast passwords, playout/webhook secrets |
| `secrets/prod-web.yaml`          | Production web DB password, SMTP, AWS keys            |
| `secrets/staging-streaming.yaml` | Staging Icecast passwords, playout/webhook secrets    |
| `secrets/staging-web.yaml`       | Staging web DB password, SMTP, AWS keys               |

Secrets are decrypted on the VPS at deploy time via [sops-nix](https://github.com/Mic92/sops-nix).

```bash
just tf-edit-secrets                   # Edit Terraform secrets
just sops-edit-dev-streaming           # Edit dev streaming secrets
just sops-edit-prod-streaming          # Edit production streaming secrets
just sops-edit-prod-web                # Edit production web secrets
just sops-edit-staging-streaming       # Edit staging streaming secrets
just sops-edit-staging-web             # Edit staging web secrets
just sops-host-key <host>              # Get a VPS host's age public key
```

### Operations

```bash
just staging-logs / just prod-logs      # View logs
just staging-status / just prod-status  # Deployment status
just staging-ssh / just prod-ssh        # SSH into VPS
```

### Streaming (NixOS VPS)

Liquidsoap and Icecast run on NixOS streaming droplets with secrets managed via sops-nix.

```bash
# Fresh droplet setup (after terraform apply)
just nixos-setup root@<ip> prod        # Wait for NixOS, get host age key
just nixos-setup root@<ip> staging

# Deploy NixOS configuration
just nixos-deploy-prod                 # Deploy to production VPS
just nixos-deploy-staging              # Deploy to staging VPS
```
