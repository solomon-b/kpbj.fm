#!/usr/bin/env bash
#
# Production Media Backup Status
#
# Compares the production DO Spaces bucket with its copy on the NAS, and shows
# the last run of the TrueNAS Cloud Sync Task that makes the copy.
# A file counts as present on the NAS only if the path and size match.
# Credentials loaded from SOPS-encrypted secrets/backup.yaml.
#
# Usage: ./scripts/prod-backup-media-info.sh

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/lib/common.sh"

AWS_ACCESS_KEY_ID=$(load_secret prod aws_access_key_id)
AWS_SECRET_ACCESS_KEY=$(load_secret prod aws_secret_access_key)
API_KEY=$(load_secret nas truenas_api_key)

command -v rclone >/dev/null 2>&1 || { echo "Error: rclone is not installed" >&2; exit 1; }

WORK_DIR=$(mktemp -d)
chmod 700 "$WORK_DIR"
cleanup() { rm -rf "$WORK_DIR"; }
trap cleanup EXIT

cat > "$WORK_DIR/rclone.conf" <<EOF
[spaces]
type = s3
provider = Other
access_key_id = ${AWS_ACCESS_KEY_ID}
secret_access_key = ${AWS_SECRET_ACCESS_KEY}
endpoint = ${PROD_ENDPOINT}
acl = private
EOF

# Each listing has one "path<TAB>size" line per file.
rclone --config "$WORK_DIR/rclone.conf" lsf -R --files-only --format ps --separator $'\t' \
  "spaces:${PROD_BUCKET}" | sort > "$WORK_DIR/bucket"

# shellcheck disable=SC2029  # NAS_BACKUP_PATH expands on this side on purpose
ssh "$NAS_HOST" "cd '$NAS_BACKUP_PATH' && find . -type f -exec stat -f '%N	%z' {} +" |
  sed 's|^\./||' | sort > "$WORK_DIR/nas"

# The header comes from stdin, so the key does not show in the process list.
# The certificate is self-signed, so -k skips the CA check and the pin checks the key.
tasks=$(printf 'Authorization: Bearer %s\n' "$API_KEY" |
  curl -sS --fail-with-body --max-time 15 \
    -k --pinnedpubkey "$NAS_TLS_PIN" \
    -H @- \
    "https://${NAS_HOST}/api/v2.0/cloudsync")

count() { wc -l < "$1" | tr -d ' '; }
gib() { awk -F'\t' '{s += $2} END {printf "%.1f GiB", s / 1073741824}' "$1"; }

comm -23 "$WORK_DIR/bucket" "$WORK_DIR/nas" > "$WORK_DIR/missing"
comm -13 "$WORK_DIR/bucket" "$WORK_DIR/nas" > "$WORK_DIR/extra"

printf '%-16s %8s %10s\n' "" "objects" "size"
printf '%-16s %8s %10s\n' "prod bucket" "$(count "$WORK_DIR/bucket")" "$(gib "$WORK_DIR/bucket")"
printf '%-16s %8s %10s\n' "NAS copy" "$(count "$WORK_DIR/nas")" "$(gib "$WORK_DIR/nas")"
printf '%-16s %8s\n' "missing on NAS" "$(count "$WORK_DIR/missing")"
printf '%-16s %8s\n' "only on NAS" "$(count "$WORK_DIR/extra")"

if [ -s "$WORK_DIR/missing" ]; then
  echo
  echo "Missing on NAS:"
  cut -f1 "$WORK_DIR/missing" | sed 's/^/  /'
fi

echo
echo "$tasks" | jq -r --arg path "$NAS_BACKUP_PATH" '
  [.[] | select(.path == $path)] | first |
  if . == null then "Last sync: no Cloud Sync Task writes to \($path)"
  elif .job == null then "Last sync: never"
  else
    "Last sync: \(if .job.time_finished then (.job.time_finished["$date"] / 1000 | strftime("%Y-%m-%d %H:%M:%S UTC")) else "running" end) (\(.job.state))" ,
    (if .job.error then "Error:     \(.job.error)" else empty end)
  end'
