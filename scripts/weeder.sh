#!/usr/bin/env bash
#
# weeder.sh
# Run weeder over every local component of the monorepo.
#
# Weeder reads .hie files. By default it walks the whole working directory,
# which picks up two kinds of stale artifact in dist-newstyle:
#
#   1. Build trees from an older GHC. Weeder stops with an error on those.
#   2. Build trees from an older package version. Those report every finding
#      a second time under a name that no longer exists.
#
# So this script asks cabal which directories belong to the current build.
# dist-newstyle/cache/plan.json lists every component of the plan, and the
# local ones carry the dist-dir that holds their .hie files. The list follows
# a GHC upgrade, a version bump, and a new package with no edit here.
#
# Every local component needs -fwrite-ide-info in its ghc-options, or weeder
# cannot see it. The script names the components that produced no .hie files,
# and it separates the two reasons a component has none. Either the stanza does
# not ask for them, or it does and the build tree does not hold them yet.
#
# Usage:
#   ./weeder.sh              # report findings, exit 0
#   ./weeder.sh --strict     # exit 228 when weeder finds something
#
# Run `cabal build all --enable-tests` first. `just weeder` does that for you.

set -euo pipefail

STRICT=0
if [ "${1:-}" = "--strict" ]; then
  STRICT=1
  shift
fi

REPO_ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$REPO_ROOT"

PLAN="dist-newstyle/cache/plan.json"
if [ ! -f "$PLAN" ]; then
  echo "error: $PLAN not found. Run 'cabal build all --enable-tests' first." >&2
  exit 1
fi

# Map a package name to its .cabal file, so an unchecked component can say
# which of the two reasons applies.
declare -A CABAL_OF
while IFS= read -r cabal_file; do
  pkg_name=$(grep -m1 -oP '^name:\s*\K\S+' "$cabal_file")
  CABAL_OF["$pkg_name"]="$cabal_file"
done < <(grep -oP '^\s+\K\S+\.cabal' cabal.project)

# Does this component's own stanza set -fwrite-ide-info?
# plan.json names components as lib, lib:<name>, exe:<name>, test:<name> and
# bench:<name>. Each maps to one stanza header. If the header is not found,
# fall back to the whole file rather than guess.
component_sets_flag() {
  local cabal_file="$1" component="$2" header
  case "$component" in
    lib) header='library' ;;
    lib:*) header="library ${component#lib:}" ;;
    exe:*) header="executable ${component#exe:}" ;;
    test:*) header="test-suite ${component#test:}" ;;
    bench:*) header="benchmark ${component#bench:}" ;;
    *) header='' ;;
  esac

  if [ -z "$header" ] || ! grep -qiE "^${header}[[:space:]]*$" "$cabal_file"; then
    grep -q -- '-fwrite-ide-info' "$cabal_file"
    return
  fi

  awk -v header="$header" '
    tolower($0) ~ "^" tolower(header) "[ \t]*$" { inside = 1; next }
    inside && /^[A-Za-z]/ { inside = 0 }
    inside && /-fwrite-ide-info/ { found = 1 }
    END { exit(found ? 0 : 1) }
  ' "$cabal_file"
}

# Local components only. A component built from hackage or from a
# source-repository-package is somebody else's code and is not our dead code.
mapfile -t PLAN_ROWS < <(
  jq -r '
    .["install-plan"][]
    | select(.["pkg-src"].type == "local")
    | select(has("dist-dir"))
    | "\(.["pkg-name"])\t\(.["component-name"] // "lib")\t\(.["dist-dir"])"
  ' "$PLAN" | sort -u
)

if [ "${#PLAN_ROWS[@]}" -eq 0 ]; then
  echo "error: no local components in $PLAN." >&2
  exit 1
fi

ARGS=()
COVERED=0
NEEDS_FLAG=()
NEEDS_BUILD=()
for row in "${PLAN_ROWS[@]}"; do
  IFS=$'\t' read -r pkg_name component dir <<<"$row"
  [ -d "$dir" ] || continue

  if [ -n "$(find "$dir" -name '*.hie' -print -quit)" ]; then
    ARGS+=(--hie-directory "$dir")
    COVERED=$((COVERED + 1))
    continue
  fi

  cabal_file="${CABAL_OF[$pkg_name]:-}"
  if [ -n "$cabal_file" ] && component_sets_flag "$cabal_file" "$component"; then
    NEEDS_BUILD+=("${dir#"$REPO_ROOT"/}")
  else
    NEEDS_FLAG+=("${dir#"$REPO_ROOT"/}")
  fi
done

if [ "${#NEEDS_FLAG[@]}" -gt 0 ]; then
  echo "warning: ${#NEEDS_FLAG[@]} component(s) are not checked, because their stanza"
  echo "         does not set -fwrite-ide-info. Add it to their ghc-options."
  for dir in "${NEEDS_FLAG[@]}"; do
    echo "         $dir"
  done
  echo
fi

if [ "${#NEEDS_BUILD[@]}" -gt 0 ]; then
  echo "warning: ${#NEEDS_BUILD[@]} component(s) are not checked. Each one asks for"
  echo "         .hie files, so the build tree holds none for it yet."
  echo "         Run 'just weeder-fresh'. A plain build does not help, because"
  echo "         cabal does not rebuild for a missing .hie file and reports"
  echo "         'Up to date' instead."
  for dir in "${NEEDS_BUILD[@]}"; do
    echo "         $dir"
  done
  echo
fi

if [ "${#ARGS[@]}" -eq 0 ]; then
  echo "error: no component produced .hie files. Nothing to check." >&2
  exit 1
fi

echo "weeder: reading $COVERED component(s)"
set +e
weeder "${ARGS[@]}" -N "$@"
STATUS=$?
set -e

# 228 is weeder's exit code for "found something". That is a report, not a
# failure, unless the caller asked for --strict.
if [ "$STATUS" -eq 228 ] && [ "$STRICT" -eq 0 ]; then
  exit 0
fi
exit "$STATUS"
