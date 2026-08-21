#!/usr/bin/env bash
#
# Tests for scripts/lib/gen-audio.sh
#
# Usage: ./scripts/test/gen-audio-test.sh
#

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# shellcheck source=../lib/gen-audio.sh
source "$SCRIPT_DIR/../lib/gen-audio.sh"

FAILURES=0

# Compare two values and report.
# Usage: expect_eq "label" "expected" "actual"
expect_eq() {
  local label="$1" expected="$2" actual="$3"
  if [ "$expected" = "$actual" ]; then
    echo "  ok   $label"
  else
    echo "  FAIL $label: expected '$expected', got '$actual'"
    FAILURES=$((FAILURES + 1))
  fi
}

echo "gen_audio_duration_for"
expect_eq "episode duration wins over size" \
  "7200" "$(gen_audio_duration_for 7200 120000000)"
expect_eq "falls back to size at 128 kbps" \
  "26" "$(gen_audio_duration_for "" 424000)"
expect_eq "falls back to the default when both are empty" \
  "300" "$(gen_audio_duration_for "" "")"
expect_eq "clamps a tiny file up to the minimum" \
  "5" "$(gen_audio_duration_for "" 1000)"
expect_eq "clamps an absurd duration down to the maximum" \
  "21600" "$(gen_audio_duration_for 99999999 "")"
expect_eq "treats a zero duration as absent" \
  "300" "$(gen_audio_duration_for 0 0)"
expect_eq "treats non-numeric input as absent" \
  "300" "$(gen_audio_duration_for "abc" "xyz")"

echo
echo "gen_audio_encode_master / gen_audio_slice"
TMPDIR_TEST=$(mktemp -d)
trap 'rm -rf "$TMPDIR_TEST"' EXIT

gen_audio_encode_master 120 "$TMPDIR_TEST/master.mp3"
expect_eq "master exists" "yes" \
  "$([ -s "$TMPDIR_TEST/master.mp3" ] && echo yes || echo no)"

master_secs=$(ffprobe -hide_banner -loglevel error \
  -show_entries format=duration -of csv=p=0 "$TMPDIR_TEST/master.mp3")
expect_eq "master is 120s (rounded)" "120" "$(printf '%.0f' "$master_secs")"

gen_audio_slice "$TMPDIR_TEST/master.mp3" 27 "$TMPDIR_TEST/slice.mp3"
slice_secs=$(ffprobe -hide_banner -loglevel error \
  -show_entries format=duration -of csv=p=0 "$TMPDIR_TEST/slice.mp3")
expect_eq "slice is 27s (rounded)" "27" "$(printf '%.0f' "$slice_secs")"

slice_bytes=$(stat -c%s "$TMPDIR_TEST/slice.mp3")
expect_eq "slice is ~1000 bytes per second" "yes" \
  "$([ "$slice_bytes" -gt 25000 ] && [ "$slice_bytes" -lt 30000 ] && echo yes || echo no)"

echo
echo "gen_audio_rows_to_work_list"
US="$GEN_AUDIO_FS"

rows_fixture=$(printf '%s\n' \
  "audio/episodes/2026/07/11/a.mp3${US}7200${US}120000000" \
  "audio/station-ids/2026/07/11/b.mp3${US}${US}424000" \
  "audio/ephemeral/2026/08/01/c.mp3${US}${US}25000000" \
  "audio/episodes/2026/01/01/d.mp3${US}${US}")

expected_work=$(printf '%s\n' \
  "audio/episodes/2026/07/11/a.mp3	7200" \
  "audio/station-ids/2026/07/11/b.mp3	26" \
  "audio/ephemeral/2026/08/01/c.mp3	1562" \
  "audio/episodes/2026/01/01/d.mp3	300")

expect_eq "maps rows to key and seconds" \
  "$expected_work" "$(printf '%s\n' "$rows_fixture" | gen_audio_rows_to_work_list)"

dup_fixture=$(printf '%s\n' \
  "audio/episodes/2026/07/11/a.mp3${US}7200${US}0" \
  "audio/episodes/2026/07/11/a.mp3${US}3600${US}0")
expect_eq "keeps the first row for a duplicate key" \
  "audio/episodes/2026/07/11/a.mp3	7200" \
  "$(printf '%s\n' "$dup_fixture" | gen_audio_rows_to_work_list)"

expect_eq "skips a row with an empty key" \
  "" "$(printf '%s3600%s0\n' "$US" "$US" | gen_audio_rows_to_work_list)"

expect_eq "keeps a key containing a space" \
  "audio/episodes/my show.mp3	60" \
  "$(printf 'audio/episodes/my show.mp3%s60%s0\n' "$US" "$US" | gen_audio_rows_to_work_list)"

echo
echo "dry run"
GEN_AUDIO_DRY_RUN=1
export GEN_AUDIO_DRY_RUN
expect_eq "put prints instead of uploading" \
  "DRY RUN would upload audio/episodes/x.mp3" \
  "$(gen_audio_put /dev/null audio/episodes/x.mp3)"
expect_eq "delete prints instead of deleting" \
  "DRY RUN would delete audio/episodes/x.mp3" \
  "$(gen_audio_delete audio/episodes/x.mp3)"
unset GEN_AUDIO_DRY_RUN

echo
echo "gen_audio_generate"
GEN_AUDIO_DRY_RUN=1
export GEN_AUDIO_DRY_RUN

work=$(mktemp); existing=$(mktemp)
printf '%s\n' \
  "audio/episodes/keep.mp3	60" \
  "audio/episodes/new.mp3	60" > "$work"
printf '%s\n' \
  "audio/episodes/keep.mp3" \
  "audio/episodes/orphan.mp3" > "$existing"

out=$(gen_audio_generate "$work" "$existing" "" "")
expect_eq "skips a key already present" "1" \
  "$(printf '%s\n' "$out" | grep -c 'would upload audio/episodes/new.mp3')"
expect_eq "does not upload the key that exists" "0" \
  "$(printf '%s\n' "$out" | grep -c 'would upload audio/episodes/keep.mp3')"
expect_eq "does not prune without the flag" "0" \
  "$(printf '%s\n' "$out" | grep -c 'would delete')"

out=$(gen_audio_generate "$work" "$existing" "1" "")
expect_eq "replace rewrites every key" "2" \
  "$(printf '%s\n' "$out" | grep -c 'would upload')"

out=$(gen_audio_generate "$work" "$existing" "" "1")
expect_eq "prune deletes the orphan" "1" \
  "$(printf '%s\n' "$out" | grep -c 'would delete audio/episodes/orphan.mp3')"
expect_eq "prune keeps a key in the work list" "0" \
  "$(printf '%s\n' "$out" | grep -c 'would delete audio/episodes/keep.mp3')"

rm -f "$work" "$existing"

# Regression: ffmpeg reads stdin for interactive keys. The generate loop's
# stdin is the work list, so without -nostdin ffmpeg eats lines and files land
# under truncated or nonsense keys. Four rows catch it; two can slip through.
work=$(mktemp); existing=$(mktemp)
printf '%s\n' \
  "audio/episodes/one.mp3	30" \
  "audio/episodes/two.mp3	31" \
  "audio/episodes/three.mp3	32" \
  "audio/episodes/four.mp3	33" > "$work"
: > "$existing"

out=$(gen_audio_generate "$work" "$existing" "" "")
expected_uploads=$(printf '%s\n' \
  "DRY RUN would upload audio/episodes/one.mp3" \
  "DRY RUN would upload audio/episodes/two.mp3" \
  "DRY RUN would upload audio/episodes/three.mp3" \
  "DRY RUN would upload audio/episodes/four.mp3")
expect_eq "ffmpeg does not consume the work list" \
  "$expected_uploads" "$(printf '%s\n' "$out" | grep 'would upload')"

rm -f "$work" "$existing"
unset GEN_AUDIO_DRY_RUN

echo
if [ "$FAILURES" -gt 0 ]; then
  echo "$FAILURES assertion(s) failed."
  exit 1
fi
echo "All assertions passed."
