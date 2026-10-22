#!/usr/bin/env bash
#
# Print UPDATE statements that fill in station_ids.duration_seconds.
#
# Station IDs uploaded before uploads were measured have no length. The
# migration that makes the column NOT NULL fails while one of them is left.
#
# The script reads "id|audio_file_path" rows on stdin. It measures each file
# with ffprobe over the public bucket URL, so it needs no credentials. It
# rounds up, like Effects.AudioDuration. It changes nothing. Read the output,
# then send it to the database yourself.
#
# Lines that do not start with "<digits>|" are skipped. So the output of
# `just prod-psql` can come in as it is, with its tunnel message.
#
# Each UPDATE keeps "duration_seconds IS NULL", so a second run changes nothing.
# A file that ffprobe cannot read goes to stderr, and the script exits 1.
#
# Usage:
#   printf '%s\n' '\pset format unaligned' '\pset tuples_only on' \
#     'SELECT id, audio_file_path FROM station_ids WHERE duration_seconds IS NULL ORDER BY id;' \
#     | just prod-psql > rows.txt
#   ./scripts/station-id-durations-sql.sh prod < rows.txt > backfill.sql
#   just prod-psql --write < backfill.sql

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

if [ $# -ne 1 ]; then
  echo "Usage: $0 <prod|staging> < rows.txt" >&2
  exit 1
fi

case "$1" in
  prod) BASE_URL="https://${PROD_BUCKET}.${PROD_ENDPOINT#https://}" ;;
  staging) BASE_URL="https://${STAGING_BUCKET}.${STAGING_ENDPOINT#https://}" ;;
  *)
    echo "Error: target must be prod or staging, not '$1'" >&2
    exit 1
    ;;
esac

command -v ffprobe >/dev/null 2>&1 || { echo "Error: ffprobe is not installed" >&2; exit 1; }

FAILED=0

echo '\set ON_ERROR_STOP 1'
echo 'BEGIN;'

while IFS='|' read -r id path; do
  [[ "$id" =~ ^[0-9]+$ ]] || continue
  url="${BASE_URL}/${path}"
  seconds=$(ffprobe -v error -show_entries format=duration -of csv=p=0 "$url" || true)
  if [[ ! "$seconds" =~ ^[0-9]+(\.[0-9]+)?$ ]] || ! awk -v s="$seconds" 'BEGIN { exit !(s > 0) }'; then
    echo "station_ids.id=${id}: no length from ${url}" >&2
    FAILED=1
    continue
  fi
  rounded=$(awk -v s="$seconds" 'BEGIN { r = int(s); if (r < s) r++; print r }')
  echo "UPDATE station_ids SET duration_seconds = ${rounded} WHERE id = ${id} AND duration_seconds IS NULL RETURNING id, duration_seconds;"
done

echo 'COMMIT;'

exit "$FAILED"
