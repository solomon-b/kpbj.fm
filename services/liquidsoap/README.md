# KPBJ Streaming Service

Icecast + Liquidsoap streaming infrastructure for KPBJ 95.9 FM.

## Architecture

```
┌─────────────────┐     ┌─────────────────┐     ┌─────────────────┐
│   Liquidsoap    │────▶│    Icecast      │────▶│   Listeners     │
│  (automation)   │     │   (streaming)   │     │                 │
└────────┬────────┘     └─────────────────┘     └─────────────────┘
         │
         │ polls
         ▼
┌─────────────────┐
│  KPBJ Web API   │
│  /api/playout/* │
└─────────────────┘
```

- **Liquidsoap**: Handles audio source selection, polling the web API for scheduled content
- **Icecast**: Streams audio to listeners over HTTP

## API Endpoints Required

Liquidsoap expects these endpoints from the web service:

| Endpoint                    | Response     | Description                                             |
|-----------------------------|--------------|---------------------------------------------------------|
| `GET /api/playout/now`      | URL or empty | Returns audio URL if show is scheduled, empty otherwise |
| `GET /api/playout/fallback` | URL          | Returns URL to fallback/filler audio                    |

## Quick Start

```bash
# Start services (from project root)
just stream-dev-start

# View logs
just stream-dev-logs

# Stop services
just stream-dev-stop

# Check status
just stream-dev-status
```

## Configuration

### Environment Variables

Create a `.env` file or set these variables:

| Variable                  | Default                                        | Description                                |
|---------------------------|------------------------------------------------|--------------------------------------------|
| `ICECAST_SOURCE_PASSWORD` | `hackme`                                        | Password for sources connecting to Icecast |
| `ICECAST_ADMIN_PASSWORD`  | `hackme`                                        | Password for Icecast admin interface       |
| `ICECAST_RELAY_PASSWORD`  | `hackme`                                        | Password for relay connections             |
| `API_BASE`                | `http://host.docker.internal:4000/api/playout` | Base URL for playout API                   |

### Production

For production, create a `.env` file with secure passwords:

```bash
ICECAST_SOURCE_PASSWORD=<secure-password>
ICECAST_ADMIN_PASSWORD=<secure-password>
ICECAST_RELAY_PASSWORD=<secure-password>
API_BASE=https://kpbj.fm/api/playout
```

## Endpoints

| Service        | URL                                   | Description             |
|----------------|---------------------------------------|-------------------------|
| Stream         | http://localhost:8000/stream          | MP3 audio stream        |
| Icecast Admin  | http://localhost:8000/admin/          | Icecast admin interface |
| Icecast Status | http://localhost:8000/status-json.xsl | JSON status             |
