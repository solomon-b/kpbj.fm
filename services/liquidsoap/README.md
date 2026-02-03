# KPBJ Streaming Infrastructure

## Architecture Overview

```
                         Web Service (Fly.io)
  +----------------------------------------------------------+
  |  /api/playout/now      -> Returns current episode URL     |
  |  /api/playout/fallback -> Returns fallback track URL      |
  +----------------------------------------------------------+
                              |
                              | HTTP polling (every :00 and :30)
                              v
                       Streaming VPS
  +-----------------------------------------------------------+
  |  +----------------+      +----------------+               |
  |  |   LiquidSoap   |----->|    Icecast     |----> Listeners|
  |  |                |      |                |               |
  |  | - Polls API    | MP3  | - /stream mount|  HLS/MP3      |
  |  | - Audio mixing |      | - Public facing|               |
  |  | - Crossfades   |      | - 100 clients  |               |
  |  +----------------+      +----------------+               |
  +-----------------------------------------------------------+
```

## Components

### LiquidSoap (`ghcr.io/solomon-b/kpbj-liquidsoap`)

Audio automation that:
- Polls web API at :00 and :30 for scheduled episodes
- Falls back to random tracks when nothing scheduled
- Streams 128kbps MP3 to Icecast
- Handles crossfades and deduplication

### Icecast (`ghcr.io/solomon-b/kpbj-icecast`)

Streaming server that:
- Accepts audio from LiquidSoap on `/stream` mount
- Serves listeners with HTTP streaming
- Provides status JSON at `/status-json.xsl`

## Environments

| Environment | API_BASE | Icecast URL |
|-------------|----------|-------------|
| Local | `http://host.docker.internal:4000/api/playout` | `http://localhost:8000/stream` |
| Staging | `https://staging.kpbj.fm/api/playout` | `https://stream-staging.kpbj.fm/stream` |
| Production | `https://www.kpbj.fm/api/playout` | `https://stream.kpbj.fm/stream` |

## Local Development

```bash
# Build Nix images and load into Docker
just stream-build

# Start streaming stack
just stream-dev-start

# View logs
just stream-dev-logs

# Stop
just stream-dev-stop
```

Requires the web service running on port 4000.

## Deployment

### Manual

```bash
# Build and push images
just stream-publish sha-$(git rev-parse --short HEAD)

# Deploy to staging
just stream-staging-deploy sha-abc123

# Deploy to production
just stream-prod-deploy sha-abc123
```

### Rollback

```bash
# Deploy a previous version
just stream-staging-deploy sha-previous123
```

## Configuration

### Environment Variables

**Icecast:**

| Variable                  | Description                        | Default     |
|---------------------------|------------------------------------|-------------|
| `ICECAST_SOURCE_PASSWORD` | Password for LiquidSoap to connect | `hackme`    |
| `ICECAST_ADMIN_PASSWORD`  | Admin interface password           | `hackme`    |
| `ICECAST_RELAY_PASSWORD`  | Relay connection password          | `hackme`    |
| `ICECAST_HOSTNAME`        | Public hostname                    | `localhost` |
| `ICECAST_MAX_CLIENTS`     | Max concurrent listeners           | `100`       |
| `ICECAST_MAX_SOURCES`     | Max source connections             | `2`         |

**LiquidSoap:**

| Variable           | Description                     | Default                                        |
|--------------------|---------------------------------|------------------------------------------------|
| `API_BASE`         | Web API base URL                | `http://host.docker.internal:4000/api/playout` |
| `ICECAST_HOST`     | Icecast container hostname      | `icecast`                                      |
| `ICECAST_PORT`     | Icecast port                    | `8000`                                         |
| `ICECAST_PASSWORD` | Source password (matches above) | `hackme`                                       |
| `ICECAST_MOUNT`    | Mount point name                | `/stream`                                      |

### Secrets Management

Secrets are stored in `.env` on each VPS:

```bash
# /opt/kpbj-stream/.env
ICECAST_SOURCE_PASSWORD=<secure-random>
ICECAST_ADMIN_PASSWORD=<secure-random>
ICECAST_RELAY_PASSWORD=<secure-random>
```

## VPS Setup (One-time per server)

```bash
# Install Docker
curl -fsSL https://get.docker.com | sh
usermod -aG docker deploy

# Create deployment directory
mkdir -p /opt/kpbj-stream
cd /opt/kpbj-stream

# Copy compose files (configs are bundled in images)
scp services/liquidsoap/docker-compose*.yml user@server:/opt/kpbj-stream/

# Create secrets file
cat > .env << 'EOF'
ICECAST_SOURCE_PASSWORD=<generate-secure-password>
ICECAST_ADMIN_PASSWORD=<generate-secure-password>
ICECAST_RELAY_PASSWORD=<generate-secure-password>
EOF

# Login to GHCR
docker login ghcr.io
```

## Troubleshooting

### No audio playing

1. Check LiquidSoap logs: `docker logs kpbj-liquidsoap`
2. Verify API connectivity: `curl $API_BASE/now`
3. Check Icecast status: `curl http://localhost:8000/status-json.xsl`

### LiquidSoap can't reach API

- **Local**: Ensure `host.docker.internal` resolves (Linux needs `extra_hosts`)
- **Remote**: Check network/firewall allows HTTPS to web service

### Icecast showing no sources

1. Verify LiquidSoap is running and connected
2. Check password match between services
3. Review Icecast logs: `docker logs kpbj-icecast`

### Connection refused errors

Check that both containers are on the same Docker network:

```bash
docker network inspect kpbj-liquidsoap_default
```

## File Structure

```
services/liquidsoap/
├── config/
│   ├── icecast.xml             # Bundled into icecast image
│   └── radio.liq               # Bundled into liquidsoap image
├── docker-compose.yml          # Base (shared config)
├── docker-compose.dev.yml      # Local dev overrides
├── docker-compose.staging.yml  # Staging VPS overrides
├── docker-compose.prod.yml     # Production VPS overrides
├── .env.example                # Secret template
└── README.md                   # This file
```
