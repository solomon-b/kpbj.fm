#!/usr/bin/env bash
#
# Staging Placeholder Audio Library
# Generates duration-matched placeholder tones for every audio key the staging
# database references, so staging needs no copy of production audio.
#
# Usage: source "$(dirname "${BASH_SOURCE[0]}")/lib/gen-audio.sh"
#

# Tone format. 8 kbps mono gives about 1000 bytes per second, so the file size
# tracks the duration almost exactly.
export GEN_AUDIO_BITRATE="8k"
export GEN_AUDIO_SAMPLE_RATE="22050"

# Duration bounds, in seconds. The maximum stops a corrupt file_size from
# asking for a 40 GB file.
export GEN_AUDIO_MIN_SECONDS="5"
export GEN_AUDIO_MAX_SECONDS="21600"
export GEN_AUDIO_DEFAULT_SECONDS="300"

# Assumed bitrate of the real production audio, used to turn a file size into a
# duration for the two tables that store no duration.
export GEN_AUDIO_NOMINAL_BITRATE="128000"

# Resolve the placeholder length for one row, in whole seconds.
# Prefers a real duration, falls back to a size at the nominal bitrate, then to
# a fixed default. The result is always within the clamp.
# Usage: gen_audio_duration_for DURATION_SECONDS FILE_SIZE
#   Either argument may be empty. psql writes NULL as an empty string.
gen_audio_duration_for() {
  local duration="${1:-}" size="${2:-}" result

  if [[ "$duration" =~ ^[0-9]+$ ]] && [ "$duration" -gt 0 ]; then
    result="$duration"
  elif [[ "$size" =~ ^[0-9]+$ ]] && [ "$size" -gt 0 ]; then
    result=$(( size * 8 / GEN_AUDIO_NOMINAL_BITRATE ))
  else
    result="$GEN_AUDIO_DEFAULT_SECONDS"
  fi

  if [ "$result" -lt "$GEN_AUDIO_MIN_SECONDS" ]; then
    result="$GEN_AUDIO_MIN_SECONDS"
  elif [ "$result" -gt "$GEN_AUDIO_MAX_SECONDS" ]; then
    result="$GEN_AUDIO_MAX_SECONDS"
  fi

  printf '%s' "$result"
}

# Encode the master tone once. Every generated file is a stream copy of a
# prefix of this, because re-encoding each file separately costs about 6.5
# seconds per audio-hour and would take hours across the whole work list.
# Usage: gen_audio_encode_master SECONDS OUT_PATH
gen_audio_encode_master() {
  local seconds="$1" out="$2"

  # -nostdin is required. Callers drive ffmpeg from a `while read` loop whose
  # stdin is the work list, and ffmpeg reads stdin for interactive keys. Without
  # it, ffmpeg eats work list lines and files land under truncated keys.
  ffmpeg -nostdin -hide_banner -loglevel error -y \
    -f lavfi \
    -i "aevalsrc='0.3*sin(2*PI*1000*t)*lt(mod(t,30),0.5)':d=$seconds:s=$GEN_AUDIO_SAMPLE_RATE:c=mono" \
    -c:a libmp3lame -b:a "$GEN_AUDIO_BITRATE" -ac 1 \
    "$out"
}

# Cut the first SECONDS of the master with a stream copy. No re-encode, so this
# takes well under a second even for a three hour target.
# Usage: gen_audio_slice MASTER_PATH SECONDS OUT_PATH
gen_audio_slice() {
  local master="$1" seconds="$2" out="$3"

  # -nostdin for the same reason as gen_audio_encode_master.
  ffmpeg -nostdin -hide_banner -loglevel error -y \
    -t "$seconds" -i "$master" -c copy "$out"
}

# The three tables that hold an audio path. Soft-deleted episodes are included
# on purpose: staff can restore an archived episode, and leaving its file out
# would break restore-then-play QA.
export GEN_AUDIO_WORK_LIST_SQL="
SELECT audio_file_path, duration_seconds, audio_file_size FROM episodes
  WHERE audio_file_path IS NOT NULL
UNION ALL
SELECT audio_file_path, NULL, file_size FROM ephemeral_uploads
UNION ALL
SELECT audio_file_path, NULL, file_size FROM station_ids
"

# Field separator for psql output. The ASCII unit separator, not a tab.
# Tab is IFS whitespace, so `read` collapses a run of tabs and drops the empty
# fields that a NULL duration produces. That would shift file_size into the
# duration slot for every station ID and ephemeral row.
GEN_AUDIO_FS=$'\x1f'
export GEN_AUDIO_FS

# Turn raw rows into the work list.
# Reads "key<FS>duration<FS>size" on stdin, prints "key<TAB>seconds".
# Rows with an empty key are dropped. A repeated key keeps its first row.
gen_audio_rows_to_work_list() {
  local key duration size seen_file
  seen_file=$(mktemp)
  # shellcheck disable=SC2064
  trap "rm -f '$seen_file'" RETURN

  while IFS="$GEN_AUDIO_FS" read -r key duration size; do
    [ -n "$key" ] || continue
    if grep -Fxq "$key" "$seen_file"; then
      continue
    fi
    printf '%s\n' "$key" >> "$seen_file"
    printf '%s\t%s\n' "$key" "$(gen_audio_duration_for "$duration" "$size")"
  done
}

# Query the staging database for the work list.
# Usage: gen_audio_work_list PSQL_URL
gen_audio_work_list() {
  local url="$1"

  psql "$url" --no-align --tuples-only --field-separator="$GEN_AUDIO_FS" \
    --command "$GEN_AUDIO_WORK_LIST_SQL" \
    | gen_audio_rows_to_work_list
}

# List every key already in the staging bucket under audio/.
# Handles keys containing spaces, the same way list_bucket in sync-s3.sh does.
gen_audio_staging_keys() {
  AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
  AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
  aws s3 ls "s3://$STAGING_BUCKET/audio/" --recursive \
    --endpoint-url "$STAGING_ENDPOINT" \
    | awk '{key=""; for(i=4;i<=NF;i++) key=key (i>4?" ":"") $i; print key}'
}

# Upload one generated file.
# put-object rather than cp, because cp switches to multipart above 8 MB and any
# episode over about 2.3 hours would cross that.
# The explicit content type stops Liquidsoap warning about binary/octet-stream.
# Usage: gen_audio_put LOCAL_PATH KEY
gen_audio_put() {
  local path="$1" key="$2"

  if [ -n "${GEN_AUDIO_DRY_RUN:-}" ]; then
    echo "DRY RUN would upload $key"
    return 0
  fi

  AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
  AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
  aws s3api put-object \
    --bucket "$STAGING_BUCKET" \
    --key "$key" \
    --body "$path" \
    --acl public-read \
    --content-type audio/mpeg \
    --endpoint-url "$STAGING_ENDPOINT" \
    >/dev/null
}

# Delete one staging object.
# Usage: gen_audio_delete KEY
gen_audio_delete() {
  local key="$1"

  if [ -n "${GEN_AUDIO_DRY_RUN:-}" ]; then
    echo "DRY RUN would delete $key"
    return 0
  fi

  AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
  AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
  aws s3api delete-object \
    --bucket "$STAGING_BUCKET" \
    --key "$key" \
    --endpoint-url "$STAGING_ENDPOINT" \
    >/dev/null
}

# Generate and upload the placeholder audio.
# Usage: gen_audio_generate WORK_LIST_FILE EXISTING_KEYS_FILE REPLACE PRUNE
#   WORK_LIST_FILE      "key<TAB>seconds" lines
#   EXISTING_KEYS_FILE  one staging key per line
#   REPLACE             non-empty to rewrite keys that already exist
#   PRUNE               non-empty to delete staging keys absent from the work list
# Returns non-zero if any upload or delete failed.
gen_audio_generate() {
  local work_file="$1" existing_file="$2" replace="${3:-}" prune="${4:-}"
  local tmpdir key seconds max_seconds=0
  local total=0 skipped=0 generated=0 pruned=0 failed=0 bytes=0

  tmpdir=$(mktemp -d)
  # shellcheck disable=SC2064
  trap "rm -rf '$tmpdir'" RETURN

  # Decide the work before encoding, so the master is only as long as it needs
  # to be. A shorter master is a faster encode.
  : > "$tmpdir/todo.txt"
  while IFS=$'\t' read -r key seconds; do
    [ -n "$key" ] || continue
    total=$((total + 1))
    if [ -z "$replace" ] && grep -Fxq "$key" "$existing_file"; then
      skipped=$((skipped + 1))
      continue
    fi
    printf '%s\t%s\n' "$key" "$seconds" >> "$tmpdir/todo.txt"
    if [ "$seconds" -gt "$max_seconds" ]; then
      max_seconds="$seconds"
    fi
  done < "$work_file"

  if [ "$max_seconds" -gt 0 ]; then
    echo "Encoding a ${max_seconds}s master tone..."
    gen_audio_encode_master "$max_seconds" "$tmpdir/master.mp3"
  fi

  while IFS=$'\t' read -r key seconds; do
    gen_audio_slice "$tmpdir/master.mp3" "$seconds" "$tmpdir/slice.mp3"
    if gen_audio_put "$tmpdir/slice.mp3" "$key"; then
      generated=$((generated + 1))
      bytes=$((bytes + $(stat -c%s "$tmpdir/slice.mp3")))
    else
      echo "  FAILED upload: $key" >&2
      failed=$((failed + 1))
    fi
  done < "$tmpdir/todo.txt"

  if [ -n "$prune" ]; then
    cut -f1 "$work_file" | sort > "$tmpdir/wanted.txt"
    sort "$existing_file" > "$tmpdir/have.txt"
    while IFS= read -r key; do
      [ -n "$key" ] || continue
      if gen_audio_delete "$key"; then
        pruned=$((pruned + 1))
      else
        echo "  FAILED delete: $key" >&2
        failed=$((failed + 1))
      fi
    done < <(comm -13 "$tmpdir/wanted.txt" "$tmpdir/have.txt")
  fi

  echo ""
  echo "  Rows in work list: $total"
  echo "  Skipped (present): $skipped"
  echo "  Generated:         $generated"
  echo "  Pruned:            $pruned"
  echo "  Failed:            $failed"
  echo "  Bytes written:     $bytes"

  [ "$failed" -eq 0 ]
}
