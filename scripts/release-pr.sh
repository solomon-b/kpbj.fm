#!/usr/bin/env bash
#
# Create a Release PR
# Bumps the version in cabal, updates CHANGELOG.md, and creates a PR.
#
# Usage: ./scripts/release-pr.sh <VERSION>
#
# Arguments:
#   VERSION  - Semantic version (X.Y.Z format, e.g., 0.3.3)
#

set -euo pipefail

if [ $# -lt 1 ]; then
  echo "Usage: $0 <VERSION>"
  echo "Example: $0 0.3.3"
  exit 1
fi

VERSION="$1"
TODAY=$(date +%Y-%m-%d)

# Validate version format (X.Y.Z)
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "ERROR: Invalid version format. Expected X.Y.Z (e.g., 0.3.3)"
  exit 1
fi

# Check that CHANGELOG.md exists
if [ ! -f "CHANGELOG.md" ]; then
  echo "ERROR: CHANGELOG.md not found. Please create it first."
  exit 1
fi

# Check that there's an [Unreleased] section with content
if ! grep -q "^\## \[Unreleased\]" CHANGELOG.md; then
  echo "ERROR: No [Unreleased] section found in CHANGELOG.md"
  exit 1
fi

# Check for uncommitted changes (ignores untracked files)
if git status --porcelain | grep -qv '^??'; then
  echo "ERROR: You have uncommitted changes. Please commit or stash them first."
  exit 1
fi

# Ensure we're on main and up to date
git checkout main
git pull origin main

# Create release branch
BRANCH="release/$VERSION"
echo "Creating branch: $BRANCH"
git checkout -b "$BRANCH"

# Extract release notes from [Unreleased] section
# Gets everything between [Unreleased] and the next ## heading (or ---)
echo "Extracting release notes from CHANGELOG.md..."
RELEASE_NOTES=$(awk '/^## \[Unreleased\]/{flag=1; next} /^## \[|^---/{flag=0} flag' CHANGELOG.md | sed '/^$/N;/^\n$/d')

if [ -z "$RELEASE_NOTES" ]; then
  echo "ERROR: [Unreleased] section is empty. Please add release notes first."
  git checkout main
  git branch -D "$BRANCH"
  exit 1
fi

# Update CHANGELOG.md: rename [Unreleased] to [VERSION] and add new [Unreleased]
echo "Updating CHANGELOG.md..."
sed -i "s/^## \[Unreleased\]/## [Unreleased]\n\n_No changes yet._\n\n---\n\n## [$VERSION] - $TODAY/" CHANGELOG.md

# Update cabal version
echo "Updating kpbj-api.cabal version to $VERSION"
sed -i "s/^version:.*$/version:            $VERSION/" services/web/kpbj-api.cabal

# Verify the cabal change
CABAL_VERSION=$(grep -oP '^version:\s*\K[0-9]+\.[0-9]+\.[0-9]+' services/web/kpbj-api.cabal)
if [ "$CABAL_VERSION" != "$VERSION" ]; then
  echo "ERROR: Failed to update cabal version"
  exit 1
fi

# Commit and push
git add services/web/kpbj-api.cabal CHANGELOG.md
git commit -m "chore: bump version to $VERSION"
git push -u origin "$BRANCH"

# Create PR with release notes using a temp file (avoids Just parsing issues)
echo "Creating pull request..."
TMPFILE=$(mktemp)
{
  echo "## Release v${VERSION}"
  echo ""
  echo "This PR releases version ${VERSION}"
  echo ""
  echo "### What's Changed"
  echo ""
  echo "$RELEASE_NOTES"
  echo ""
  echo "---"
  echo ""
  echo "When merged, a git tag v${VERSION} will be automatically created, which triggers the production deployment."
} > "$TMPFILE"

gh pr create \
  --title "Release v$VERSION" \
  --body-file "$TMPFILE" \
  --base main

rm -f "$TMPFILE"

echo ""
echo "Release PR created with release notes from CHANGELOG.md!"
echo "Once merged, v$VERSION will be automatically tagged and deployed to production."
