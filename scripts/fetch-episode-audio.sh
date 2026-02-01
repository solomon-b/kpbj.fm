#!/usr/bin/env bash
#
# Fetch episode audio files from Tigris S3 storage within a date range.
# Queries the database to get only current versions (not replaced uploads).
#
# Usage:
#   ./fetch-episode-audio.sh START_DATE END_DATE [TARGET_DIR]
#
# Arguments:
#   START_DATE  - Start date in YYYY-MM-DD format (inclusive)
#   END_DATE    - End date in YYYY-MM-DD format (inclusive)
#   TARGET_DIR  - Optional target directory (defaults to current directory)
#
# Environment:
#   PROD_DATABASE_URL    - Production PostgreSQL connection string (required)
#   BUCKET_NAME          - S3 bucket name (default: production-kpbj-storage)
#   AWS_ENDPOINT_URL_S3  - S3 endpoint URL (default: https://fly.storage.tigris.dev)
#   AWS_REGION           - AWS region (default: auto)
#   AWS_ACCESS_KEY_ID    - AWS access key (required)
#   AWS_SECRET_ACCESS_KEY - AWS secret key (required)
#
# Examples:
#   ./fetch-episode-audio.sh 2024-01-01 2024-01-31
#   ./fetch-episode-audio.sh 2024-01-01 2024-01-31 ./downloads
#

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
NC='\033[0m' # No Color

usage() {
    echo "Usage: $0 START_DATE END_DATE [TARGET_DIR]"
    echo ""
    echo "Fetch episode audio files from Tigris S3 within a date range."
    echo ""
    echo "Arguments:"
    echo "  START_DATE  Start date in YYYY-MM-DD format (inclusive)"
    echo "  END_DATE    End date in YYYY-MM-DD format (inclusive)"
    echo "  TARGET_DIR  Optional target directory (defaults to current directory)"
    echo ""
    echo "Examples:"
    echo "  $0 2024-01-01 2024-01-31"
    echo "  $0 2024-01-01 2024-01-31 ./downloads"
    exit 1
}

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1" >&2
}

# Validate date format (YYYY-MM-DD)
validate_date() {
    local date="$1"
    if ! [[ "$date" =~ ^[0-9]{4}-[0-9]{2}-[0-9]{2}$ ]]; then
        log_error "Invalid date format: $date (expected YYYY-MM-DD)"
        exit 1
    fi
    # Check if it's a valid date
    if ! date -d "$date" &>/dev/null; then
        log_error "Invalid date: $date"
        exit 1
    fi
}

# Convert date to seconds since epoch for comparison
date_to_epoch() {
    date -d "$1" +%s
}

# Check required arguments
if [[ $# -lt 2 ]]; then
    usage
fi

START_DATE="$1"
END_DATE="$2"
TARGET_DIR="${3:-.}"

# Validate dates
validate_date "$START_DATE"
validate_date "$END_DATE"

START_EPOCH=$(date_to_epoch "$START_DATE")
END_EPOCH=$(date_to_epoch "$END_DATE")

if [[ $START_EPOCH -gt $END_EPOCH ]]; then
    log_error "Start date must be before or equal to end date"
    exit 1
fi

# S3 configuration with defaults
BUCKET_NAME="${BUCKET_NAME:-production-kpbj-storage}"
AWS_ENDPOINT_URL_S3="${AWS_ENDPOINT_URL_S3:-https://fly.storage.tigris.dev}"
AWS_REGION="${AWS_REGION:-auto}"

export AWS_REGION

# Check for AWS credentials
if [[ -z "${AWS_ACCESS_KEY_ID:-}" ]] || [[ -z "${AWS_SECRET_ACCESS_KEY:-}" ]]; then
    log_error "AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY must be set"
    exit 1
fi

# Check for PROD_DATABASE_URL
if [[ -z "${PROD_DATABASE_URL:-}" ]]; then
    log_error "PROD_DATABASE_URL must be set"
    exit 1
fi

# Production database configuration
FLY_PG_APP="kpbj-postgres"
PROXY_PORT=15432

# Start fly proxy if not already running
if lsof -i ":$PROXY_PORT" &>/dev/null; then
    log_warn "Port $PROXY_PORT already in use, assuming fly proxy is running"
    PROXY_PID=""
else
    log_info "Starting fly proxy to $FLY_PG_APP..."
    fly proxy "$PROXY_PORT:5432" -a "$FLY_PG_APP" &
    PROXY_PID=$!
    sleep 2
fi

# Cleanup proxy on exit
cleanup() {
    if [[ -n "${PROXY_PID:-}" ]]; then
        kill "$PROXY_PID" 2>/dev/null || true
    fi
}
trap cleanup EXIT

# Replace host in DATABASE_URL with localhost proxy
LOCAL_DATABASE_URL=$(echo "$PROD_DATABASE_URL" | sed -E "s|@[^/@]+/|@localhost:$PROXY_PORT/|")
log_info "Using database: $(echo "$LOCAL_DATABASE_URL" | sed -E 's|://[^:]+:[^@]+@|://***:***@|')"

# Check for AWS CLI
if ! command -v aws &>/dev/null; then
    log_error "AWS CLI not found. Please install it first."
    exit 1
fi

# Check for psql
if ! command -v psql &>/dev/null; then
    log_error "psql not found. Please install PostgreSQL client."
    exit 1
fi

# Check for fly CLI
if ! command -v fly &>/dev/null; then
    log_error "fly CLI not found. Please install it first."
    exit 1
fi

# Create target directory if it doesn't exist
mkdir -p "$TARGET_DIR"
TARGET_DIR=$(realpath "$TARGET_DIR")

log_info "Fetching episode audio from $START_DATE to $END_DATE"
log_info "Bucket: $BUCKET_NAME"
log_info "Endpoint: $AWS_ENDPOINT_URL_S3"
log_info "Target directory: $TARGET_DIR"
echo ""

# Query database for current episode audio paths within date range
log_info "Querying database for current episode audio files..."

# Query episodes table for audio_file_path where scheduled_at is in range
# and the episode is not deleted and has an audio file.
# Input dates are Pacific time, convert to UTC for database comparison.
audio_paths=$(psql "$LOCAL_DATABASE_URL" -t -A -F $'\t' <<SQL
SELECT audio_file_path, audio_file_size
FROM episodes
WHERE scheduled_at >= '${START_DATE}'::timestamp AT TIME ZONE 'America/Los_Angeles'
  AND scheduled_at < ('${END_DATE}'::date + interval '1 day')::timestamp AT TIME ZONE 'America/Los_Angeles'
  AND deleted_at IS NULL
  AND audio_file_path IS NOT NULL
ORDER BY scheduled_at
SQL
)

if [[ -z "$audio_paths" ]]; then
    log_info "No episodes found in date range."
    exit 0
fi

# Count episodes
episode_count=$(echo "$audio_paths" | wc -l)
log_info "Found $episode_count episode(s) with audio files"
echo ""

# Track statistics
total_files=0
total_bytes=0
downloaded_files=0
skipped_files=0

# Process each audio file
while IFS=$'\t' read -r key size; do
    # Skip empty lines
    [[ -z "$key" ]] && continue

    # Extract just the filename (flatten the path)
    filename=$(basename "$key")
    target_path="$TARGET_DIR/$filename"

    ((total_files++)) || true
    if [[ -n "$size" ]]; then
        total_bytes=$((total_bytes + size))
    fi

    # Check if file already exists
    if [[ -f "$target_path" ]]; then
        if [[ -n "$size" ]]; then
            existing_size=$(stat -c%s "$target_path" 2>/dev/null || echo "0")
            if [[ "$existing_size" == "$size" ]]; then
                log_warn "Skipping (exists): $filename"
                ((skipped_files++)) || true
                continue
            fi
        else
            log_warn "Skipping (exists): $filename"
            ((skipped_files++)) || true
            continue
        fi
    fi

    # Download the file
    size_display=""
    if [[ -n "$size" ]]; then
        size_display=" ($(numfmt --to=iec-i --suffix=B "$size" 2>/dev/null || echo "${size} bytes"))"
    fi
    log_info "Downloading: $filename$size_display"

    if aws s3 cp \
        "s3://$BUCKET_NAME/$key" \
        "$target_path" \
        --endpoint-url "$AWS_ENDPOINT_URL_S3" \
        --quiet; then
        ((downloaded_files++)) || true
    else
        log_error "Failed to download: $key"
    fi

done <<< "$audio_paths"

echo ""
log_info "Complete!"
log_info "Total files found: $total_files"
log_info "Downloaded: $downloaded_files"
log_info "Skipped (already exist): $skipped_files"
log_info "Total size: $(numfmt --to=iec-i --suffix=B "$total_bytes" 2>/dev/null || echo "$total_bytes bytes")"
