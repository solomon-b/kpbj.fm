#!/usr/bin/env bash
set -euo pipefail

# ──────────────────────────────────────────────────────────────
# tf-import.sh — Generate Terraform import blocks for Cloudflare
# ──────────────────────────────────────────────────────────────
#
# Discovers existing Cloudflare DNS record IDs via API and writes
# declarative `import {}` blocks to terraform/imports.tf.
#
# DigitalOcean resources are provisioned fresh by Terraform, so
# they are not included. See the commented section at the bottom
# of the generated file if you need to import DO resources manually.
#
# Prerequisites:
#   - just tf-init completed
#   - sops configured with age keys
#   - jq installed
#
# Usage:
#   ./scripts/tf-import.sh
#   just tf-plan     # review imports
#   just tf-apply    # apply imports into state
#   rm terraform/imports.tf
# ──────────────────────────────────────────────────────────────

command -v jq >/dev/null 2>&1 || { echo "ERROR: jq required"; exit 1; }
command -v sops >/dev/null 2>&1 || { echo "ERROR: sops required"; exit 1; }

IMPORTS_FILE="terraform/imports.tf"
FAILED=()

# Extract Cloudflare credentials from project config
CLOUDFLARE_API_TOKEN=$(sops -d --extract '["cloudflare_api_token"]' secrets/terraform.yaml)
CLOUDFLARE_ZONE_ID=$(sops -d --extract '["cloudflare_zone_id"]' secrets/terraform.yaml)

if [ -z "$CLOUDFLARE_API_TOKEN" ]; then
  echo "ERROR: Could not extract cloudflare_api_token from secrets.yaml"
  exit 1
fi

if [ -z "$CLOUDFLARE_ZONE_ID" ]; then
  echo "ERROR: Could not extract cloudflare_zone_id from secrets.yaml"
  exit 1
fi

echo "═══════════════════════════════════════════════════════"
echo "  Fetching Cloudflare DNS records"
echo "═══════════════════════════════════════════════════════"
echo ""

dns_json=$(curl -s \
  -H "Authorization: Bearer $CLOUDFLARE_API_TOKEN" \
  -H "Content-Type: application/json" \
  "https://api.cloudflare.com/client/v4/zones/$CLOUDFLARE_ZONE_ID/dns_records?per_page=500")

if [ "$(echo "$dns_json" | jq -r '.success')" != "true" ]; then
  echo "ERROR: Cloudflare API request failed:"
  echo "$dns_json" | jq '.errors'
  exit 1
fi

records=$(echo "$dns_json" | jq '.result')
record_count=$(echo "$records" | jq 'length')
echo "  Found ${record_count} DNS records"
echo ""

# Find a record ID by name + type, optionally filtering by content substring
find_record() {
  local name="$1" type="$2" content_match="${3:-}"
  if [ -n "$content_match" ]; then
    echo "$records" | jq -r --arg name "$name" --arg type "$type" --arg match "$content_match" \
      '[.[] | select(.name == $name and .type == $type and (.content | contains($match)))][0].id // empty'
  else
    echo "$records" | jq -r --arg name "$name" --arg type "$type" \
      '[.[] | select(.name == $name and .type == $type)][0].id // empty'
  fi
}

# Collect import blocks as an array of strings
IMPORT_BLOCKS=()

add_import() {
  local tf_name="$1" record_id="$2"
  if [ -n "$record_id" ]; then
    IMPORT_BLOCKS+=("import {
  to = cloudflare_dns_record.${tf_name}
  id = \"${CLOUDFLARE_ZONE_ID}/${record_id}\"
}")
    echo "  OK: cloudflare_dns_record.${tf_name}"
  else
    echo "  WARNING: DNS record not found for ${tf_name}"
    FAILED+=("cloudflare_dns_record.${tf_name}")
  fi
}

echo "── Resolving DNS record IDs ──"

# Web (AAAA → Fly.io)
add_import root_aaaa    "$(find_record kpbj.fm AAAA)"
add_import www_aaaa     "$(find_record www.kpbj.fm AAAA)"
add_import staging_aaaa "$(find_record staging.kpbj.fm AAAA)"

# Streaming (A → DigitalOcean VPS)
add_import stream         "$(find_record stream.kpbj.fm A)"
add_import stream_staging "$(find_record stream.staging.kpbj.fm A)"

# Other services
add_import libretime "$(find_record libretime.kpbj.fm A)"
add_import planka    "$(find_record planka.kpbj.fm A)"

# Uploads (CNAME → Fly.io)
add_import uploads         "$(find_record uploads.kpbj.fm CNAME)"
add_import uploads_staging "$(find_record uploads.staging.kpbj.fm CNAME)"

# ACME challenges (CNAME → Fly DNS)
add_import acme_challenge_root            "$(find_record _acme-challenge.kpbj.fm CNAME)"
add_import acme_challenge_staging         "$(find_record _acme-challenge.staging.kpbj.fm CNAME)"
add_import acme_challenge_uploads         "$(find_record _acme-challenge.uploads.kpbj.fm CNAME)"
add_import acme_challenge_uploads_staging "$(find_record _acme-challenge.uploads.staging.kpbj.fm CNAME)"

# MX records (match by content)
add_import mx_primary "$(find_record kpbj.fm MX aspmx.l.google.com)"
add_import mx_alt1    "$(find_record kpbj.fm MX alt1.aspmx)"
add_import mx_alt2    "$(find_record kpbj.fm MX alt2.aspmx)"
add_import mx_alt3    "$(find_record kpbj.fm MX alt3.aspmx)"
add_import mx_alt4    "$(find_record kpbj.fm MX alt4.aspmx)"

# Email auth (TXT — match by content substring)
add_import spf                      "$(find_record kpbj.fm TXT "v=spf1")"
add_import dmarc                    "$(find_record _dmarc.kpbj.fm TXT)"
add_import dkim_default             "$(find_record default._domainkey.kpbj.fm TXT)"
add_import google_site_verification "$(find_record kpbj.fm TXT "google-site-verification")"
add_import nms_verification         "$(find_record kpbj.fm TXT "nms-domain-verification")"

# DKIM CNAME (Mailchimp)
add_import dkim_k2 "$(find_record k2._domainkey.kpbj.fm CNAME)"
add_import dkim_k3 "$(find_record k3._domainkey.kpbj.fm CNAME)"

echo ""

# ── Write imports.tf ─────────────────────────────────────────

{
  cat <<'HEADER'
# ──────────────────────────────────────────────────────────────
# Generated by scripts/tf-import.sh — DO NOT COMMIT
# ──────────────────────────────────────────────────────────────
#
# Delete this file after applying:
#   just tf-plan
#   just tf-apply
#   rm terraform/imports.tf
# ──────────────────────────────────────────────────────────────

HEADER

  for block in "${IMPORT_BLOCKS[@]}"; do
    echo "$block"
    echo ""
  done

  cat <<'FOOTER'
# ──────────────────────────────────────────────────────────────
# DigitalOcean resources (provisioned fresh — not imported)
# ──────────────────────────────────────────────────────────────
#
# If you need to import existing DO resources, add blocks like:
#
# import {
#   to = digitalocean_droplet.stream_prod
#   id = "<droplet-id>"    # doctl compute droplet list
# }
#
# import {
#   to = digitalocean_droplet.stream_staging
#   id = "<droplet-id>"    # doctl compute droplet list
# }
#
# import {
#   to = digitalocean_firewall.stream_prod
#   id = "<firewall-id>"   # doctl compute firewall list
# }
#
# import {
#   to = digitalocean_firewall.stream_staging
#   id = "<firewall-id>"   # doctl compute firewall list
# }
#
# import {
#   to = digitalocean_ssh_key.stream["<machine-name>"]
#   id = "<ssh-key-id>"    # doctl compute ssh-key list
# }
FOOTER
} > "$IMPORTS_FILE"

# ── Summary ──────────────────────────────────────────────────

echo "═══════════════════════════════════════════════════════"
if [ ${#FAILED[@]} -eq 0 ]; then
  echo "  Wrote ${#IMPORT_BLOCKS[@]} import blocks to ${IMPORTS_FILE}"
else
  echo "  Wrote ${#IMPORT_BLOCKS[@]} import blocks to ${IMPORTS_FILE}"
  echo "  ${#FAILED[@]} record(s) not found:"
  for f in "${FAILED[@]}"; do
    echo "    - $f"
  done
fi
echo "═══════════════════════════════════════════════════════"
echo ""
echo "Next steps:"
echo "  1. just tf-plan    — review imports (DO resources show as new)"
echo "  2. just tf-apply   — apply imports into state"
echo "  3. rm ${IMPORTS_FILE}"
