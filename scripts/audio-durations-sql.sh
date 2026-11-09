#!/usr/bin/env bash
#
# Print UPDATE statements that fill in duration_seconds for one table.
#
# The table is station_ids, ephemeral_uploads, or episodes. Rows uploaded
# before uploads were measured have no length. The playout needs the length of
# each file.
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
# Usage for station_ids:
#   printf '%s\n' '\pset format unaligned' '\pset tuples_only on' \
#     'SELECT id, audio_file_path FROM station_ids WHERE duration_seconds IS NULL ORDER BY id;' \
#     | just prod-psql > rows.txt
#   ./scripts/audio-durations-sql.sh station_ids prod < rows.txt > backfill.sql
#   just prod-psql --write < backfill.sql
#
# Usage for ephemeral_uploads:
#   printf '%s\n' '\pset format unaligned' '\pset tuples_only on' \
#     'SELECT id, audio_file_path FROM ephemeral_uploads WHERE duration_seconds IS NULL ORDER BY id;' \
#     | just prod-psql > rows.txt
#   ./scripts/audio-durations-sql.sh ephemeral_uploads prod < rows.txt > backfill.sql
#   just prod-psql --write < backfill.sql
#
# Usage for episodes:
#   printf '%s\n' '\pset format unaligned' '\pset tuples_only on' \
#     'SELECT id, audio_file_path FROM episodes WHERE duration_seconds IS NULL AND audio_file_path IS NOT NULL ORDER BY id;' \
#     | just prod-psql > rows.txt
#   ./scripts/audio-durations-sql.sh episodes prod < rows.txt > backfill.sql
#   just prod-psql --write < backfill.sql

set -euo pipefail

if [ $# -ne 2 ]; then
  echo "Usage: $0 <station_ids|ephemeral_uploads|episodes> <prod|staging> < rows.txt" >&2
  exit 1
fi

TABLE="$1"
TARGET="$2"

case "$TABLE" in
  station_ids | ephemeral_uploads | episodes) ;;
  *)
    echo "Error: table must be station_ids, ephemeral_uploads, or episodes, not '$TABLE'" >&2
    exit 1
    ;;
esac

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck disable=SC1091
source "$SCRIPT_DIR/lib/common.sh"

case "$TARGET" in
  prod) BASE_URL="https://${PROD_BUCKET}.${PROD_ENDPOINT#https://}" ;;
  staging) BASE_URL="https://${STAGING_BUCKET}.${STAGING_ENDPOINT#https://}" ;;
  *)
    echo "Error: target must be prod or staging, not '$TARGET'" >&2
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
    echo "${TABLE}.id=${id}: no length from ${url}" >&2
    FAILED=1
    continue
  fi
  rounded=$(awk -v s="$seconds" 'BEGIN { r = int(s); if (r < s) r++; print r }')
  echo "UPDATE ${TABLE} SET duration_seconds = ${rounded} WHERE id = ${id} AND duration_seconds IS NULL RETURNING id, duration_seconds;"
done

echo 'COMMIT;'

exit "$FAILED"
