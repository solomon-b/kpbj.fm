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

Icecast passwords are auto-generated on first deploy and stored in `.env` on each VPS. These are internal-only (LiquidSoap → Icecast) and never need to be known or manually configured.

```bash
# Auto-generated at /opt/kpbj-stream/.env on first deploy
ICECAST_SOURCE_PASSWORD=<random>
ICECAST_ADMIN_PASSWORD=<random>
ICECAST_RELAY_PASSWORD=<random>
```

To regenerate secrets, delete `.env` and redeploy.

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

# Login to GHCR
docker login ghcr.io
```

Icecast secrets are auto-generated on first deploy - no manual setup needed.

## Nginx Reverse Proxy

For HTTPS access to the stream, configure nginx on the VPS to proxy to Icecast.

### Get TLS Certificate

```bash
sudo certbot certonly --nginx -d stream.staging.kpbj.fm
```

### Nginx Config

Create `/etc/nginx/sites-available/stream.staging.kpbj.fm`:

```nginx
server {
    listen 80;
    server_name stream.staging.kpbj.fm;
    return 301 https://$server_name$request_uri;
}

server {
    listen 443 ssl http2;
    server_name stream.staging.kpbj.fm;

    ssl_certificate /etc/letsencrypt/live/stream.staging.kpbj.fm/fullchain.pem;
    ssl_certificate_key /etc/letsencrypt/live/stream.staging.kpbj.fm/privkey.pem;

    location / {
        proxy_pass http://127.0.0.1:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;

        # Streaming: disable buffering for real-time audio
        proxy_buffering off;
        proxy_request_buffering off;

        # Long-lived connections for listeners
        proxy_read_timeout 24h;
        proxy_send_timeout 24h;
    }
}
```

### Enable and Reload

```bash
sudo ln -s /etc/nginx/sites-available/stream.staging.kpbj.fm /etc/nginx/sites-enabled/
sudo nginx -t && sudo systemctl reload nginx
```

Stream will be available at `https://stream.staging.kpbj.fm/stream`.

For production, repeat with `stream.kpbj.fm`.

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
├── scripts/
│   └── stream-deploy.sh        # Remote deployment with auto-generated secrets
├── docker-compose.yml          # Base (shared config)
├── docker-compose.dev.yml      # Local dev overrides
├── docker-compose.staging.yml  # Staging VPS overrides
├── docker-compose.prod.yml     # Production VPS overrides
└── README.md                   # This file
```
