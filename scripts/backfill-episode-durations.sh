#!/usr/bin/env bash
# Backfill duration_seconds for existing episodes
#
# Usage:
#   For local dev:   ./scripts/backfill-episode-durations.sh local
#   For production:  ./scripts/backfill-episode-durations.sh prod
#
# Prerequisites:
#   - ffprobe (from ffmpeg) must be installed
#   - For prod: fly CLI must be installed and authenticated
#   - For prod: PROD_DB_PASSWORD environment variable must be set

set -euo pipefail

MODE="${1:-local}"

# Check for ffprobe
if ! command -v ffprobe &> /dev/null; then
  echo "Error: ffprobe is required but not installed."
  echo "Install ffmpeg to get ffprobe: brew install ffmpeg (macOS) or apt install ffmpeg (Linux)"
  exit 1
fi

if [[ "$MODE" == "prod" ]]; then
  # Production: use fly proxy
  PROXY_PORT="15433"
  DB_APP="kpbj-postgres"
  DB_NAME="kpbj_fm"
  DB_USER="kpbj_fm"
  MEDIA_BASE="https://production-kpbj-storage.t3.storageapi.dev"

  if [[ -z "${PROD_DB_PASSWORD:-}" ]]; then
    echo "Error: PROD_DB_PASSWORD environment variable must be set for production mode"
    exit 1
  fi

  echo "Starting production database proxy..."
  fly proxy "$PROXY_PORT:5432" -a "$DB_APP" &
  PROXY_PID=$!
  trap "kill $PROXY_PID 2>/dev/null || true" EXIT
  sleep 3

  DB_URL="postgres://$DB_USER:$PROD_DB_PASSWORD@localhost:$PROXY_PORT/$DB_NAME"
else
  # Local dev - read directly from filesystem
  DB_URL="postgres://postgres@localhost:5433/dev_db"
  MEDIA_BASE="/tmp/kpbj"
fi

echo "Fetching episodes without duration..."
echo ""

# Count total episodes to process
total=$(psql "$DB_URL" -t -A -c "
  SELECT COUNT(*)
  FROM episodes
  WHERE audio_file_path IS NOT NULL
    AND duration_seconds IS NULL
")

echo "Found $total episodes to process"
echo ""

if [[ "$total" -eq 0 ]]; then
  echo "No episodes need duration backfill."
  exit 0
fi

success_count=0
fail_count=0

# Process each episode
while IFS='|' read -r id path; do
  [[ -z "$id" ]] && continue

  url="${MEDIA_BASE}/${path}"
  echo -n "Episode $id: "

  # ffprobe can read directly from URLs
  duration=$(ffprobe -v quiet -show_entries format=duration \
    -of default=noprint_wrappers=1:nokey=1 "$url" 2>/dev/null | cut -d. -f1)

  if [[ -n "$duration" && "$duration" =~ ^[0-9]+$ && "$duration" -gt 0 ]]; then
    echo "${duration}s"
    psql "$DB_URL" -q \
      -c "UPDATE episodes SET duration_seconds = $duration WHERE id = $id"
    ((success_count++)) || true
  else
    echo "failed to extract duration"
    ((fail_count++)) || true
  fi
done < <(psql "$DB_URL" -t -A -F'|' -c "
  SELECT id, audio_file_path
  FROM episodes
  WHERE audio_file_path IS NOT NULL
    AND duration_seconds IS NULL
  ORDER BY id
")

echo ""
echo "Done!"
echo "  Successfully updated: $success_count"
echo "  Failed: $fail_count"
