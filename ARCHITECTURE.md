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
┌─────────────────────────────────────────────────────────────────────────────┐
│                           DIGITALOCEAN VPS                                  │
│                                                                             │
│  ┌──────────────────────────────────────────────────────────────┐           │
│  │                          Nginx                               │           │
│  │  stream.kpbj.fm → Icecast              www.kpbj.fm → Web    │           │
│  └───────┬──────────────────────────────────────────┬───────────┘           │
│          │ HTTP                                     │ HTTP                  │
│          ▼                                          ▼                      │
│  ┌─────────────────┐                       ┌─────────────────┐             │
│  │     Icecast     │                       │   WEB SERVICE   │             │
│  │  • /stream      │                       │  • Haskell      │             │
│  │  • 128kbps MP3  │                       │  • Servant      │             │
│  └────────▲────────┘                       │  • HTMX         │             │
│           │ MP3                             └────────┬────────┘             │
│  ┌────────┴────────┐                                │                      │
│  │    Liquidsoap   │       localhost                 │                      │
│  │  • Polls API ───────────────────────────────────►│                      │
│  │  • Crossfades   │                       ┌────────┴────────┐             │
│  └─────────────────┘                       │   PostgreSQL    │             │
│                                            │  • Users, Shows │             │
│                                            │  • Episodes     │             │
│                                            └─────────────────┘             │
│                                                                             │
└─────────────────────────────────────────────────────────────────────────────┘
                                                     │
                                                     │ S3 API
                                                     ▼
                                              ┌───────────┐
                                              │ DO Spaces │
                                              │ • Audio   │
                                              │ • Images  │
                                              └───────────┘
```

## Services

### Web Service

Haskell/Servant application with server-side HTML rendering (Lucid2 + HTMX). Serves the public website, host dashboard, and playout API. Uses PostgreSQL for data and DigitalOcean Spaces for media files.

- **Production**: DigitalOcean VPS (native NixOS systemd service) with local PostgreSQL
- **Staging**: DigitalOcean VPS (native NixOS systemd service) with local PostgreSQL

### Liquidsoap (VPS)

Audio automation daemon. Asks the web service for the next timeline segment before each boundary, plays its items in order, and outputs 128kbps MP3 to Icecast.

### Icecast (VPS)

Streaming server behind NixOS-managed Nginx (automatic Let's Encrypt via ACME). Accepts audio from Liquidsoap and serves up to 100 concurrent listeners on `/stream`.

## Playout API

Liquidsoap communicates with the web service via two endpoints. `POST /api/playout/played` is authenticated with a shared secret (`X-Playout-Secret` header). `GET /api/playout/now` is not.

| Endpoint              | Method | Purpose                                  |
|-----------------------|--------|------------------------------------------|
| `/api/playout/now`    | GET    | Get the segment of the timeline          |
| `/api/playout/played` | POST   | Report what is playing                   |

`/now` returns `{"current": ..., "next": ...}`. `current` is one segment of the timeline, and `next` is the whole segment after it, or null. A segment is the span between two boundaries, and its items fill the span with no gaps. Every segment is at least 30 minutes long. The items are an episode, a break, ephemeral tracks, or filler. Each item has a planned start time. Each segment has `ends_in_seconds`, the seconds from the request to the end of the segment. It has `id`, a SHA-256 hash of the segment start, the segment end, and the URL, cue points, break item id, and planned start of each item. The id changes only when what airs in the segment changes, and a resumed segment keeps it.

Liquidsoap plays the items one after another and does not use the planned starts. The stored lengths are rounded up to whole seconds, so the items usually end a little before their planned ends. A filler item at the end of a segment therefore has no cue out. It plays until the swap at the boundary stops it, so the gap has audio and not silence. When the items run long, the swap cuts the filler and not a break track. A segment that does not end with filler can still end with a short silence. The filler file must be longer than 180 seconds, so that a trailing filler of the full 180 seconds has spare audio.

While a segment is on the air, Liquidsoap downloads the files of `next` and holds them. A file that does not download does not stop the files after it, and it is tried again every 5 seconds. A file that never downloads is left out of the held segment, and the rest of the segment plays. 10 seconds before the boundary, Liquidsoap asks `/now` for the next segment again. If its `id` matches the id of the held segment, Liquidsoap uses the held files. If not, it downloads the new segment. It swaps to the new segment at the boundary. If that request fails, Liquidsoap plays the held segment. If nothing is held, it asks `/now` again at the boundary. A restart asks `/now` at once, and the answer holds the segment that airs now. So a restart keeps the schedule.

`/played` takes `prPlannedStart`, the planned start time of the item. The web service uses it to ignore a repeated report of the same airing. Liquidsoap does not post filler items to `/played`.

### Known limits

After a restart, the audio runs behind the timeline by the download time of the current segment. The swap at the next boundary then cuts that many seconds from the end of the segment. The end of a segment is its break. The same occurs when the next segment changes after Liquidsoap holds it. Liquidsoap then downloads the new segment at the check, and a download of more than 10 seconds makes the audio late.

An item that still does not download at the check is dropped, and the items after it play early. Liquidsoap logs `[KPBJ] Could not resolve <url>` for each try.

### Release steps for the timeline

The first release of the timeline needs these steps, in this order:

1. Upload a filler file of at least 240 seconds to `audio/filler/kpbj-filler.mp3` in the prod bucket. On staging, `just prod-to-staging-audio` makes a placeholder for it.
2. Deploy.
3. Restart Liquidsoap right after the deploy. The deploy does not restart it, and the old script cannot read the new `/now`.
4. Fill the length of the older ephemeral uploads with `scripts/audio-durations-sql.sh ephemeral_uploads prod`. The usage at the top of the script gives the full commands.

## Environments

| Environment | Web Service             | Stream                                | Now Playing                                    |
|-------------|-------------------------|---------------------------------------|------------------------------------------------|
| Production  | https://www.kpbj.fm     | https://stream.kpbj.fm/stream         | https://www.kpbj.fm/api/stream/now-playing     |
| Staging     | https://staging.kpbj.fm | https://stream.staging.kpbj.fm/stream | https://staging.kpbj.fm/api/stream/now-playing |
| Local       | http://localhost:4000   | http://localhost:8000/stream          | http://localhost:4000/api/stream/now-playing   |

## CI/CD

All builds use Nix. Docker images are pushed to GitHub Container Registry (`ghcr.io/solomon-b/`).

### Web Service

| Workflow                  | Trigger                    | Action                                                              |
|---------------------------|----------------------------|---------------------------------------------------------------------|
| `nix.yaml`                | Push, PR                   | Build with Nix                                                      |
| `create-release-tag.yaml` | Merge release PR           | Create `v*` git tag                                                 |
| `deploy-staging.yaml`     | Push to `main`             | NixOS rebuild to staging VPS                                         |
| `deploy-production.yaml`  | Manual (`gh workflow run`) | NixOS rebuild to production VPS                                      |

### VPS Services (NixOS)

**VPS Configuration**: Droplets run NixOS with native systemd services. Configuration is declarative in `nixos/`:

```
nixos/
  common.nix              # SSH, firewall, base packages
  hardware-digitalocean.nix  # DigitalOcean hardware baseline
  networking-prod.nix     # Production droplet static IP (generated by nixos-setup)
  networking-staging.nix  # Staging droplet static IP (generated by nixos-setup)
  streaming.nix           # Streaming services (Icecast, Liquidsoap, Webhook)
  postgresql.nix          # PostgreSQL database
  web.nix                 # Web service (native systemd)
  nginx.nix               # Reverse proxy + ACME (Let's Encrypt)
  sync-host-emails.nix    # Host email sync job (Google Groups)
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

Deploy commands: `just staging-nixos-deploy`, `just prod-nixos-deploy`

## Secrets Management (SOPS)

All secrets are SOPS-encrypted with [age](https://age-encryption.org/) keys and stored in `secrets/`:

```
secrets/
  terraform.yaml           # Terraform provider tokens (DO, Cloudflare)
  prod-streaming.yaml      # Production: Icecast passwords, playout/webhook secrets
  prod-web.yaml            # Production web: DB password, SMTP, AWS keys
  staging-streaming.yaml   # Staging: Icecast passwords, playout/webhook secrets
  staging-web.yaml         # Staging web: DB password, SMTP, AWS keys
  google.yaml              # Google Groups service account credentials
  backup.yaml              # Backup/sync credentials (prod + staging)
```

**Encryption keys** are defined in `.sops.yaml`: three developer PC keys plus one VPS host key per environment. VPS host keys are derived from the host's SSH ed25519 key via `ssh-to-age`.

**VPS secrets flow:**
1. SOPS-encrypted YAML is committed to git
2. On `nixos-rebuild boot`, [sops-nix](https://github.com/Mic92/sops-nix) decrypts secrets at boot using the host's SSH key (converted to age)
3. sops-nix renders per-service env files (`kpbj-liquidsoap.env`, `kpbj-webhook.env`, `kpbj-web.env`) and restarts affected services
4. Icecast reads secrets directly from sops secret file paths via its ExecStartPre script

**Production web service** receives secrets via sops-nix, same as staging.

## Infrastructure as Code (Terraform)

Infrastructure provisioning is managed by Terraform in `terraform/`. This covers resource creation — CI/CD workflows remain separate. VPS configuration is managed declaratively via NixOS (`nixos/`).

### What Terraform Manages

| Resource                                         | Provider       | File              |
|--------------------------------------------------|----------------|-------------------|
| DigitalOcean streaming droplets (prod + staging) | `digitalocean` | `digitalocean.tf` |
| DigitalOcean firewalls (prod + staging)          | `digitalocean` | `digitalocean.tf` |
| DigitalOcean SSH keys                            | `digitalocean` | `digitalocean.tf` |
| Cloudflare DNS records                           | `cloudflare`   | `cloudflare.tf`   |
| Cloudflare proxy settings                        | `cloudflare`   | `cloudflare.tf`   |

### What Stays Manual

- GitHub Actions CI/CD (`.github/workflows/`)
- NixOS VPS configuration (`just prod-nixos-deploy` / `just staging-nixos-deploy`)
- Secrets (SOPS-encrypted in `secrets/`, decrypted by sops-nix on VPS)
