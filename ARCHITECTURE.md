# KPBJ 95.9FM System Architecture

## Overview

```
                                      INTERNET
                                         │
            ┌────────────────────────────┼────────────────────────────┐
            │                            │                            │
            ▼                            ▼                            ▼
     ┌─────────────┐             ┌─────────────┐             ┌─────────────┐
     │  Listeners  │             │   Visitors  │             │    Hosts    │
     │  (Stream)   │             │  (Website)  │             │  (Dashboard)│
     └──────┬──────┘             └──────┬──────┘             └──────┬──────┘
            │                           │ HTTPS                     │ HTTPS
            │ HTTPS                     └─────────────┬─────────────┘
            │                                         │
            ▼                                         ▼
┌──────────────────────────┐            ┌───────────────────────────────────┐
│      STREAMING VPS       │            │           FLY.IO                  │
│                          │            │                                   │
│  ┌─────────────────┐     │   HTTPS    │  ┌─────────────────────────────┐  │
│  │      Nginx      │ ◄───────────────────│       WEB SERVICE           │  │
│  └────────┬────────┘     │(poll+fetch)│  │                             │  │
│           │              │            │  │  • Haskell/Servant          │  │
│           │ HTTP         │            │  │  • Server-side HTML         │  │
│           ▼              │            │  │  • Episode management       │  │
│  ┌─────────────────┐     │   ┌───────────│  • User authentication      │  │
│  │     Icecast     │     │   │        │  │                             │  │
│  │                 │     │   │        │  └──────────────┬──────────────┘  │
│  │  • /stream      │     │   │        │                 │                 │
│  │  • 100 clients  │     │   │        │        ┌────────┴────────┐        │
│  │  • 128kbps MP3  │     │   │        │        │                 │        │
│  └────────▲────────┘     │   │        │        ▼                 ▼        │
│           │              │   │        │  ┌───────────┐    ┌───────────┐   │
│           │ MP3          │   │        │  │ PostgreSQL│    │ Tigris S3 │   │
│           │              │   │        │  │           │    │           │   │
│  ┌────────┴────────┐     │   │        │  │ • Users   │    │ • Audio   │   │
│  │    Liquidsoap   │     │   │        │  │ • Shows   │    │ • Images  │   │
│  │                 │─────────┘        │  │ • Episodes│    │           │   │
│  │  • Polls API    │     │            │  │ • Blogs   │    │           │   │
│  │  • Mixes audio  │     │            │  └───────────┘    └───────────┘   │
│  │  • Crossfades   │     │            │                                   │
│  └─────────────────┘     │            └───────────────────────────────────┘
│                          │
└──────────────────────────┘

```

## Services

### Web Service (Fly.io)

Haskell/Servant application with server-side HTML rendering (Lucid2 + HTMX). Serves the public website, host dashboard, and playout API. Uses PostgreSQL for data and Tigris S3 for media files.

### Liquidsoap (VPS)

Audio automation daemon. Polls the web service at :00 and :30 for scheduled episodes, falls back to ephemeral tracks when nothing is scheduled, and outputs 128kbps MP3 to Icecast.

### Icecast (VPS)

Streaming server behind NixOS-managed Nginx (automatic Let's Encrypt via ACME). Accepts audio from Liquidsoap and serves up to 100 concurrent listeners on `/stream`.

## Playout API

Liquidsoap communicates with the web service via three endpoints, authenticated with a shared secret (`X-Playout-Secret` header):

| Endpoint                | Method | Purpose                         |
|-------------------------|--------|---------------------------------|
| `/api/playout/now`      | GET    | Get currently scheduled episode |
| `/api/playout/fallback` | GET    | Get random fallback track       |
| `/api/playout/played`   | POST   | Report what's playing           |

## Environments

| Environment | Web Service             | Stream                                | Stream Metadata                              |
|-------------|-------------------------|---------------------------------------|----------------------------------------------|
| Production  | https://www.kpbj.fm     | https://stream.kpbj.fm/stream         | https://www.kpbj.fm/api/stream/metadata      |
| Staging     | https://staging.kpbj.fm | https://stream.staging.kpbj.fm/stream | https://staging.kpbj.fm/api/stream/metadata  |
| Local       | http://localhost:4000   | http://localhost:8000/stream          | http://localhost:4000/api/stream/metadata    |

## CI/CD

All builds use Nix. Docker images are pushed to GitHub Container Registry (`ghcr.io/solomon-b/`).

### Web Service

| Workflow                  | Trigger                    | Action                                                              |
|---------------------------|----------------------------|---------------------------------------------------------------------|
| `nix.yaml`                | Push, PR                   | Build with Nix                                                      |
| `deploy-staging.yaml`     | Push to `main`             | Build image → push `sha-<commit>` → deploy to Fly.io staging        |
| `create-release-tag.yaml` | Merge release PR           | Create `v*` git tag                                                 |
| `deploy-production.yaml`  | Manual (`gh workflow run`) | Build image → push version + `latest` → deploy to Fly.io production |

Config: `services/web/fly.toml` (prod), `services/web/fly.staging.toml` (staging)

### Streaming Services

**VPS Configuration**: Streaming droplets run NixOS with native systemd services. Configuration is declarative in `nixos/`:

```
nixos/
  common.nix              # SSH, firewall, base packages
  hardware-digitalocean.nix  # DigitalOcean hardware baseline
  networking-prod.nix     # Production droplet static IP (generated by nixos-setup)
  networking-staging.nix  # Staging droplet static IP (generated by nixos-setup)
  streaming.nix           # Native systemd services (Icecast, Liquidsoap, Webhook)
  nginx.nix               # Reverse proxy + ACME (Let's Encrypt)
  sops.nix                # sops-nix secrets (per-service env files + secret paths)
  prod.nix                # Production host config
  staging.nix             # Staging host config
```

**Local development** uses `docker-compose.yml` at the project root.

Service configs live in their own directories:

```
services/icecast/icecast.xml    # Icecast server config
services/liquidsoap/radio.liq   # Liquidsoap automation script
services/webhook/hooks.yaml     # Webhook definitions
```

Deploy commands: `just nixos-deploy-staging`, `just nixos-deploy-prod`

## Secrets Management (SOPS)

All secrets are SOPS-encrypted with [age](https://age-encryption.org/) keys and stored in `secrets/`:

```
secrets/
  terraform.yaml           # Terraform provider tokens (DO, Cloudflare)
  prod-streaming.yaml      # Production: Icecast passwords, playout/webhook secrets
  staging-streaming.yaml   # Staging: Icecast passwords, playout/webhook secrets
```

**Encryption keys** are defined in `.sops.yaml`: three developer PC keys plus one VPS host key per environment. VPS host keys are derived from the host's SSH ed25519 key via `ssh-to-age`.

**Streaming secrets flow:**
1. SOPS-encrypted YAML is committed to git
2. On `nixos-rebuild boot`, [sops-nix](https://github.com/Mic92/sops-nix) decrypts secrets at boot using the host's SSH key (converted to age)
3. sops-nix renders per-service env files (`kpbj-liquidsoap.env`, `kpbj-webhook.env`) and restarts affected services; Icecast reads secrets directly from sops secret file paths via its ExecStartPre script
4. The same SOPS source feeds Fly.io via `just fly-sync-secrets-prod` / `just fly-sync-secrets-staging`

## Infrastructure as Code (Terraform)

Infrastructure provisioning is managed by Terraform in `terraform/`. This covers resource creation — CI/CD workflows remain separate. Streaming VPS configuration is managed declaratively via NixOS (`nixos/`).

### What Terraform Manages

| Resource                                         | Provider       | File              |
|--------------------------------------------------|----------------|-------------------|
| DigitalOcean streaming droplets (prod + staging) | `digitalocean` | `digitalocean.tf` |
| DigitalOcean firewalls (prod + staging)          | `digitalocean` | `digitalocean.tf` |
| DigitalOcean SSH keys                            | `digitalocean` | `digitalocean.tf` |
| Cloudflare DNS records                           | `cloudflare`   | `cloudflare.tf`   |
| Cloudflare proxy settings                        | `cloudflare`   | `cloudflare.tf`   |

### What Stays Manual

- Fly.io web service (managed via `flyctl` + CI/CD)
- GitHub Actions CI/CD (`.github/workflows/`)
- NixOS streaming VPS configuration (`just nixos-deploy-*`)
- Streaming secrets (SOPS-encrypted in `secrets/`, decrypted by sops-nix on VPS, synced to Fly.io via Justfile)
- `fly.toml` / `fly.staging.toml` (used by CI deploy workflows)
