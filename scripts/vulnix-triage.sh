#!/usr/bin/env bash
#
# Triage vulnix Findings for a Server
# Collects the evidence for each vulnix finding on a NixOS system. Then
# Claude classifies each CVE from that evidence only, with no tools.
#
# Evidence for each package:
#   - the NVD record of each CVE (CPE matches, CNA product, references)
#   - the patches that nixpkgs applies, and the CVE IDs in them
#   - the dependency chain from the system to the package
#   - the version in the latest stable and unstable nixpkgs
#
# NVD responses are cached for 7 days in $XDG_CACHE_HOME/kpbj-vulnix-triage.
# Without NVD_API_KEY, the NVD API allows 5 requests in 30 seconds, so the
# first run is slow. Each run keeps its files in the runs/ subdirectory.
#
# Usage: ./scripts/vulnix-triage.sh staging|prod [--bundle-only]
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
PROMPT_FILE="$SCRIPT_DIR/vulnix-triage-prompt.md"

usage() {
  echo "Usage: $0 staging|prod [--bundle-only]" >&2
  exit 1
}

ENV="${1:-}"
MODE="${2:-}"
case "$ENV" in staging | prod) ;; *) usage ;; esac
case "$MODE" in "" | --bundle-only) ;; *) usage ;; esac

CONFIG="kpbj-$ENV"
CACHE_DIR="${XDG_CACHE_HOME:-$HOME/.cache}/kpbj-vulnix-triage"
NVD_DIR="$CACHE_DIR/nvd"
RUN_DIR="$CACHE_DIR/runs/$ENV-$(date +%Y%m%d-%H%M%S)"
mkdir -p "$NVD_DIR" "$RUN_DIR"

NVD_URL="https://services.nvd.nist.gov/rest/json/cves/2.0"
NVD_ARGS=()
NVD_DELAY=6
if [ -n "${NVD_API_KEY:-}" ]; then
  NVD_ARGS=(-H "apiKey: $NVD_API_KEY")
  NVD_DELAY=1
fi

log() { echo "$*" >&2; }

cd "$REPO_ROOT"

# -----------------------------------------------------------------------------
# vulnix report

# vulnix exits 1 when it finds only whitelisted CVEs, and 2 when it finds a
# CVE or has an internal error. So check the output, not only the exit code.
log "Running vulnix on $CONFIG..."
rc=0
nix run ".#vulnix-$ENV" -- --json > "$RUN_DIR/vulnix.json" || rc=$?
case "$rc" in 0 | 1 | 2) ;; *)
  log "ERROR: vulnix failed with exit code $rc."
  exit 1
  ;;
esac
if ! jq -e 'type == "array"' "$RUN_DIR/vulnix.json" > /dev/null 2>&1; then
  log "ERROR: vulnix did not write a JSON report. See $RUN_DIR/vulnix.json"
  exit 1
fi
if jq -e 'length == 0' "$RUN_DIR/vulnix.json" > /dev/null; then
  log "vulnix found no CVEs."
  exit 0
fi

# -----------------------------------------------------------------------------
# System facts

TOPLEVEL=$(nix eval --raw ".#nixosConfigurations.$CONFIG.config.system.build.toplevel.outPath")
nix path-info -r "$TOPLEVEL" | sort > "$RUN_DIR/closure.txt"

log "Reading the open ports and the nginx routes..."
nix eval --json ".#nixosConfigurations.$CONFIG.config" --apply '
  c: {
    firewall = { inherit (c.networking.firewall) allowedTCPPorts allowedUDPPorts; };
    nginxVirtualHosts = builtins.mapAttrs
      (_: v: builtins.mapAttrs (_: l: l.proxyPass) v.locations)
      c.services.nginx.virtualHosts;
  }' > "$RUN_DIR/exposure.json"

# -----------------------------------------------------------------------------
# Evidence for each package

# Print the patches of a derivation, with the CVE IDs in each patch name and
# patch body, as a JSON array.
patch_evidence() {
  local drv_json=$1 patch
  jq -r '.env.patches // (.structuredAttrs.patches // [] | join(" "))' <<< "$drv_json" |
    tr ' ' '\n' | sed '/^$/d' |
    while IFS= read -r patch; do
      [ -e "$patch" ] || nix-store --realise "$patch" > /dev/null 2>&1 < /dev/null || true
      local cves readable=false
      if [ -e "$patch" ]; then
        readable=true
        cves=$({ basename "$patch"; grep -rohiE 'CVE-[0-9]{4}-[0-9]{4,}' "$patch" || true; } |
          grep -oiE 'CVE-[0-9]{4}-[0-9]{4,}' | tr '[:lower:]' '[:upper:]' | sort -u || true)
      else
        cves=$(basename "$patch" | grep -oiE 'CVE-[0-9]{4}-[0-9]{4,}' | tr '[:lower:]' '[:upper:]' | sort -u || true)
      fi
      jq -n --arg name "$(basename "$patch" | cut -d- -f2-)" --arg cves "$cves" --argjson readable "$readable" \
        '{name: $name, readable: $readable, cves: ($cves | split("\n") | map(select(. != "")))}'
    done | jq -s .
}

# Print the dependency chain from the system to each output of a derivation
# that is in the system closure, as a JSON object keyed by output path.
dependency_chains() {
  local drv_json=$1 store=$2 output
  jq -r --arg store "$store" '.outputs[].path | "\($store)/\(.)"' <<< "$drv_json" |
    while IFS= read -r output; do
      grep -qxF "$output" "$RUN_DIR/closure.txt" || continue
      jq -n --arg output "$(basename "$output" | cut -d- -f2-)" \
        --arg chain "$(nix why-depends "$TOPLEVEL" "$output" < /dev/null 2>&1 |
          sed -E 's/\x1b\[[0-9;]*m//g; s#/nix/store/[a-z0-9]{32}-##g')" \
        '{($output): $chain}'
    done | jq -s 'add // {}'
}

log "Collecting patches and dependency chains..."
: > "$RUN_DIR/evidence.jsonl"
while IFS= read -r -u 3 drv; do
  log "  $(basename "$drv")"
  drv_json=$(nix derivation show "$drv" | jq '.derivations | to_entries[0].value')
  jq -n --arg derivation "$drv" \
    --argjson patches "$(patch_evidence "$drv_json")" \
    --argjson chains "$(dependency_chains "$drv_json" "$(dirname "$drv")")" \
    '{derivation: $derivation, patches: $patches, dependency_chains: $chains}' \
    >> "$RUN_DIR/evidence.jsonl"
done 3< <(jq -r '.[].derivation' "$RUN_DIR/vulnix.json")

# -----------------------------------------------------------------------------
# NVD records

# Download the NVD record of a CVE into the cache, unless the cache has a
# record younger than 7 days. A CVE with no record gets null in the bundle.
fetch_nvd() {
  local id=$1 file="$NVD_DIR/$1.json" code attempt
  if [ -n "$(find "$file" -mtime -7 2> /dev/null)" ]; then return 0; fi
  for attempt in 1 2 3 4 5; do
    code=$(curl -s -o "$file.tmp" -w '%{http_code}' "${NVD_ARGS[@]}" "$NVD_URL?cveId=$id" || echo 000)
    sleep "$NVD_DELAY"
    if [ "$code" = 200 ] && jq -e '.vulnerabilities | length > 0' "$file.tmp" > /dev/null 2>&1; then
      mv "$file.tmp" "$file"
      return 0
    fi
    log "  NVD returned $code for $id (attempt $attempt), retry in 30 seconds"
    sleep 30
  done
  rm -f "$file.tmp"
  log "WARNING: no NVD record for $id"
}

mapfile -t CVES < <(jq -r '[.[].affected_by[]] | unique | .[]' "$RUN_DIR/vulnix.json")
log "Fetching ${#CVES[@]} NVD records..."
for id in "${CVES[@]}"; do fetch_nvd "$id"; done

# Some CVEs list more than 100 downstream products. The verdicts need only
# the distinct products, so keep the first few of each list and its length.
for id in "${CVES[@]}"; do
  file="$NVD_DIR/$id.json"
  [ -s "$file" ] || continue
  jq -c --arg id "$id" '.vulnerabilities[0].cve
    | [.configurations[]?.nodes[]?.cpeMatch[]? | select(.vulnerable)
       | {criteria} + with_entries(select(.key | startswith("version")))] as $cpe
    | ([.affected[]?.affectedData[]?
        | {vendor, product, versions: [(.versions // [])[] | select(.status == "affected") | .version]}]
       | unique_by([.vendor, .product])) as $cna
    | {($id): {
        status: .vulnStatus,
        published,
        lastModified,
        description: ([.descriptions[]? | select(.lang == "en") | .value] | first),
        cpe: $cpe[:20],
        cpe_total: ($cpe | length),
        cna: $cna[:10],
        cna_total: ($cna | length),
        references: [.references[]? | {url, tags}][:10]
      }}' "$file"
done | jq -s 'add // {}' > "$RUN_DIR/nvd.json"

# -----------------------------------------------------------------------------
# Newer versions

NIXPKGS_LOCK=$(jq '.nodes.nixpkgs | {ref: .original.ref, rev: .locked.rev, lastModified: (.locked.lastModified | todate)}' flake.lock)
STABLE_REF=$(jq -r .ref <<< "$NIXPKGS_LOCK")
PNAMES=$(jq -r '[.[].pname] | unique | map(@json) | join(" ")' "$RUN_DIR/vulnix.json")

# Print the version of each pname in one nixpkgs branch, as a top-level
# package and as a Haskell package.
versions_in() {
  local branch=$1
  nix eval --json "github:NixOS/nixpkgs/$branch#legacyPackages.x86_64-linux" --apply "
    pkgs:
    let
      try = v: let r = builtins.tryEval v; in if r.success then r.value else null;
      lookup = n: {
        name = n;
        value = {
          nixpkgs = try (pkgs.\${n}.version or null);
          haskellPackages = try (pkgs.haskellPackages.\${n}.version or null);
        };
      };
    in
    builtins.listToAttrs (map lookup [ $PNAMES ])" ||
    { log "WARNING: version lookup in $branch failed"; echo '{}'; }
}

log "Looking up versions in $STABLE_REF and nixpkgs-unstable..."
versions_in "$STABLE_REF" > "$RUN_DIR/versions-stable.json"
versions_in nixpkgs-unstable > "$RUN_DIR/versions-unstable.json"

# -----------------------------------------------------------------------------
# Bundle

jq -nc \
  --arg config "$CONFIG" \
  --arg toplevel "$TOPLEVEL" \
  --argjson nixpkgs "$NIXPKGS_LOCK" \
  --slurpfile exposure "$RUN_DIR/exposure.json" \
  --slurpfile vulnix "$RUN_DIR/vulnix.json" \
  --slurpfile evidence <(jq -s 'map({(.derivation): .}) | add' "$RUN_DIR/evidence.jsonl") \
  --slurpfile nvd "$RUN_DIR/nvd.json" \
  --slurpfile stable "$RUN_DIR/versions-stable.json" \
  --slurpfile unstable "$RUN_DIR/versions-unstable.json" \
  '{
    system: {
      config: $config,
      toplevel: ($toplevel | sub("/nix/store/[a-z0-9]{32}-"; "")),
      nixpkgs: $nixpkgs,
      exposure: $exposure[0]
    },
    packages: [$vulnix[0][] | . as $p | {
      name, pname, version, derivation,
      latest: {
        stable: ($stable[0][$p.pname] // null),
        unstable: ($unstable[0][$p.pname] // null)
      },
      dependency_chains: $evidence[0][$p.derivation].dependency_chains,
      patches: $evidence[0][$p.derivation].patches,
      cves: [$p.affected_by[] | {id: ., cvss: $p.cvssv3_basescore[.], nvd: ($nvd[0][.] // null)}]
    }]
  }' > "$RUN_DIR/bundle.json"

log "Bundle: $RUN_DIR/bundle.json"
if [ "$MODE" = --bundle-only ]; then exit 0; fi

# -----------------------------------------------------------------------------
# Analysis

# The verdicts must rest on the bundle only. So the session has no built-in
# tools, no MCP servers, and no hooks. It runs in the run directory, so it
# loads no project CLAUDE.md, project memory, or git status. The request comes
# before --tools, because --tools takes every value that follows it.
log "Asking Claude to classify the findings..."
(
  cd "$RUN_DIR"
  claude -p "Classify the vulnix findings in this bundle." \
    --tools "" \
    --strict-mcp-config \
    --settings '{"disableAllHooks": true}' \
    --system-prompt "$(cat "$PROMPT_FILE")" \
    < bundle.json
) | tee "$RUN_DIR/triage.md"
log "Report: $RUN_DIR/triage.md"
