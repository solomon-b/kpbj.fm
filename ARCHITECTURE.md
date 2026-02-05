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
│  │    Liquidsoap   ├     │   │        │  │ • Shows   │    │ • Images  │   │
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

Streaming server behind Nginx (certbot for TLS). Accepts audio from Liquidsoap and serves up to 100 concurrent listeners on `/stream`.

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

| Workflow            | Trigger | Action                                                    |
|---------------------|---------|-----------------------------------------------------------|
| `stream-images.yml` | Manual  | Build Icecast + Liquidsoap images with Nix → push to GHCR |

Deployment is manual via SSH. Docker Compose files in `services/liquidsoap/`:

```
docker-compose.yml          # Base config
docker-compose.dev.yml      # Local dev overrides
docker-compose.staging.yml  # Staging VPS overrides
docker-compose.prod.yml     # Production VPS overrides
```

Deploy commands: `just stream-staging-deploy <tag>`, `just stream-prod-deploy <tag>`
