#!/usr/bin/env bash
# Deploy streaming services to a remote VPS
# Usage: ./stream-deploy.sh <host> <environment> <tag>
#   host: SSH target (e.g., deploy@stream.kpbj.fm)
#   environment: staging or prod
#   tag: Docker image tag (e.g., sha-abc123 or latest)

set -euo pipefail

HOST="${1:?Usage: stream-deploy.sh <host> <environment> <tag>}"
ENV="${2:?Usage: stream-deploy.sh <host> <environment> <tag>}"
TAG="${3:-latest}"

if [[ "$ENV" != "staging" && "$ENV" != "prod" ]]; then
  echo "ERROR: environment must be 'staging' or 'prod'"
  exit 1
fi

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
COMPOSE_FILE="docker-compose.${ENV}.yml"
REMOTE_DIR="/opt/kpbj-stream"

echo "Deploying streaming services to ${ENV} (tag: ${TAG})..."

# Create remote directory if needed (no sudo - assumes directory exists or user can create it)
echo "Setting up remote directory..."
ssh "$HOST" "mkdir -p ${REMOTE_DIR}" || {
  echo "ERROR: Cannot create ${REMOTE_DIR}. Run this once on the VPS:"
  echo "  sudo mkdir -p ${REMOTE_DIR} && sudo chown \$(whoami) ${REMOTE_DIR}"
  exit 1
}

# Copy compose files
echo "Copying compose files..."
scp "${SCRIPT_DIR}/docker-compose.yml" "${SCRIPT_DIR}/${COMPOSE_FILE}" "${HOST}:${REMOTE_DIR}/"

# Deploy
ssh "$HOST" bash -s "$TAG" "$COMPOSE_FILE" << 'REMOTE_SCRIPT'
set -euo pipefail
TAG="$1"
COMPOSE_FILE="$2"

cd /opt/kpbj-stream

# Auto-generate secrets on first deploy
if [ ! -f .env ]; then
  echo "Generating Icecast secrets..."
  pw1=$(head /dev/urandom | tr -dc a-zA-Z0-9 | head -c 32)
  pw2=$(head /dev/urandom | tr -dc a-zA-Z0-9 | head -c 32)
  pw3=$(head /dev/urandom | tr -dc a-zA-Z0-9 | head -c 32)
  printf "ICECAST_SOURCE_PASSWORD=%s\nICECAST_ADMIN_PASSWORD=%s\nICECAST_RELAY_PASSWORD=%s\n" \
    "$pw1" "$pw2" "$pw3" > .env
  chmod 600 .env
fi

echo "IMAGE_TAG=${TAG}" > .env.tag
docker compose -f docker-compose.yml -f "$COMPOSE_FILE" pull
docker compose -f docker-compose.yml -f "$COMPOSE_FILE" up -d
REMOTE_SCRIPT

echo "Done!"
