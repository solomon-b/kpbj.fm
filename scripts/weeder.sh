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
# cannot see it. The script names the components that produced no .hie files.
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

# Local components only. A component built from hackage or from a
# source-repository-package is somebody else's code and is not our dead code.
mapfile -t DIST_DIRS < <(
  jq -r '
    .["install-plan"][]
    | select(.["pkg-src"].type == "local")
    | select(has("dist-dir"))
    | .["dist-dir"]
  ' "$PLAN" | sort -u
)

if [ "${#DIST_DIRS[@]}" -eq 0 ]; then
  echo "error: no local components in $PLAN." >&2
  exit 1
fi

ARGS=()
COVERED=0
UNCOVERED=()
for dir in "${DIST_DIRS[@]}"; do
  [ -d "$dir" ] || continue
  if [ -z "$(find "$dir" -name '*.hie' -print -quit)" ]; then
    UNCOVERED+=("${dir#"$REPO_ROOT"/}")
    continue
  fi
  ARGS+=(--hie-directory "$dir")
  COVERED=$((COVERED + 1))
done

if [ "${#UNCOVERED[@]}" -gt 0 ]; then
  echo "warning: ${#UNCOVERED[@]} component(s) produced no .hie files and are not checked."
  echo "         Add -fwrite-ide-info to their ghc-options."
  for dir in "${UNCOVERED[@]}"; do
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
