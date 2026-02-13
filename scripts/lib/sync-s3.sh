#!/usr/bin/env bash
#
# Incremental S3 Sync Library
# Compares two S3-compatible buckets by key and file size, then copies only
# new/changed files and removes stale ones from the destination.
#
# Usage: source "$(dirname "${BASH_SOURCE[0]}")/lib/sync-s3.sh"
#
# Required variables (set before calling sync_s3_buckets):
#   PROD_AWS_ACCESS_KEY_ID
#   PROD_AWS_SECRET_ACCESS_KEY
#   STAGING_AWS_ACCESS_KEY_ID
#   STAGING_AWS_SECRET_ACCESS_KEY
#   PROD_BUCKET
#   STAGING_BUCKET
#   PROD_ENDPOINT
#   STAGING_ENDPOINT
#

# List a bucket as tab-separated "size\tkey" lines.
# Handles keys that contain spaces.
list_bucket() {
  local access_key="$1"
  local secret_key="$2"
  local bucket="$3"
  local endpoint="$4"

  AWS_ACCESS_KEY_ID="$access_key" \
  AWS_SECRET_ACCESS_KEY="$secret_key" \
  aws s3 ls "s3://$bucket" --recursive \
    --endpoint-url "$endpoint" \
    | awk '{key=""; for(i=4;i<=NF;i++) key=key (i>4?" ":"") $i; print $3 "\t" key}' \
    | sort -t$'\t' -k2
}

# Incrementally sync the production bucket to the staging bucket.
# Only copies files that are new or have a different size, and
# deletes files from staging that no longer exist in production.
sync_s3_buckets() {
  local tmpdir
  tmpdir=$(mktemp -d)
  # shellcheck disable=SC2064
  trap "rm -rf '$tmpdir'" RETURN

  echo ""
  echo "Listing production bucket..."
  list_bucket "$PROD_AWS_ACCESS_KEY_ID" "$PROD_AWS_SECRET_ACCESS_KEY" "$PROD_BUCKET" "$PROD_ENDPOINT" \
    > "$tmpdir/prod.txt"

  echo "Listing staging bucket..."
  list_bucket "$STAGING_AWS_ACCESS_KEY_ID" "$STAGING_AWS_SECRET_ACCESS_KEY" "$STAGING_BUCKET" "$STAGING_ENDPOINT" \
    > "$tmpdir/staging.txt"

  local prod_count staging_count
  prod_count=$(wc -l < "$tmpdir/prod.txt")
  staging_count=$(wc -l < "$tmpdir/staging.txt")
  echo "  Production: $prod_count files"
  echo "  Staging:    $staging_count files"

  # Compare listings: copy files that are new or have a different size,
  # delete files that exist in staging but not in production.
  touch "$tmpdir/to_copy.txt" "$tmpdir/to_delete.txt"
  awk -F'\t' '
    NR == FNR { prod[$2] = $1; next }
    { staging[$2] = $1 }
    END {
      for (key in prod) {
        if (!(key in staging) || prod[key] != staging[key]) {
          print key > copy_file
        }
      }
      for (key in staging) {
        if (!(key in prod)) {
          print key > delete_file
        }
      }
    }
  ' copy_file="$tmpdir/to_copy.txt" delete_file="$tmpdir/to_delete.txt" \
    "$tmpdir/prod.txt" "$tmpdir/staging.txt"

  local copy_count delete_count
  copy_count=$(wc -l < "$tmpdir/to_copy.txt")
  delete_count=$(wc -l < "$tmpdir/to_delete.txt")

  echo ""
  echo "  To copy:   $copy_count"
  echo "  To delete: $delete_count"

  if [ "$copy_count" -eq 0 ] && [ "$delete_count" -eq 0 ]; then
    echo ""
    echo "Staging is already in sync. Nothing to do."
    return
  fi

  if [ "$copy_count" -gt 0 ]; then
    echo ""
    echo "Copying new/changed files..."
    while IFS= read -r key; do
      echo "  -> $key"
      AWS_ACCESS_KEY_ID="$PROD_AWS_ACCESS_KEY_ID" \
      AWS_SECRET_ACCESS_KEY="$PROD_AWS_SECRET_ACCESS_KEY" \
      aws s3 cp "s3://$PROD_BUCKET/$key" "$tmpdir/transfer" \
        --endpoint-url "$PROD_ENDPOINT" --quiet

      AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
      AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
      aws s3 cp "$tmpdir/transfer" "s3://$STAGING_BUCKET/$key" \
        --endpoint-url "$STAGING_ENDPOINT" --acl public-read --quiet

      rm -f "$tmpdir/transfer"
    done < "$tmpdir/to_copy.txt"
  fi

  if [ "$delete_count" -gt 0 ]; then
    echo ""
    echo "Deleting stale files from staging..."
    while IFS= read -r key; do
      echo "  x  $key"
      AWS_ACCESS_KEY_ID="$STAGING_AWS_ACCESS_KEY_ID" \
      AWS_SECRET_ACCESS_KEY="$STAGING_AWS_SECRET_ACCESS_KEY" \
      aws s3 rm "s3://$STAGING_BUCKET/$key" \
        --endpoint-url "$STAGING_ENDPOINT" --quiet
    done < "$tmpdir/to_delete.txt"
  fi

  echo ""
  echo "  Copied $copy_count, deleted $delete_count files."
}
