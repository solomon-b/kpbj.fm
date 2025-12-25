#!/usr/bin/env bash
#
# Load Mock Images
# Copies mock media files from mock-data/media/ to /tmp/kpbj/ with proper structure
#
# Usage: ./scripts/load-mock-images.sh
#

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(dirname "$SCRIPT_DIR")"
MOCK_DATA_DIR="$PROJECT_ROOT/mock-data/media"
TARGET_ROOT="/tmp/kpbj"

# Use a fixed date for mock data (makes paths predictable for database references)
MOCK_YEAR="2025"
MOCK_MONTH="01"
MOCK_DAY="01"

echo "========================================"
echo "KPBJ Mock Image Loader"
echo "========================================"
echo ""
echo "Source: $MOCK_DATA_DIR"
echo "Target: $TARGET_ROOT"
echo ""

# Create target directory structure
echo "Creating directory structure..."
mkdir -p "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/avatars"
mkdir -p "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/logos"
mkdir -p "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/banners"
mkdir -p "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/artwork"
mkdir -p "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/event-posters"
mkdir -p "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/blog-heroes"

# Copy avatars
echo ""
echo "Copying avatars..."
if [ -d "$MOCK_DATA_DIR/avatars" ]; then
    count=$(find "$MOCK_DATA_DIR/avatars" -type f | wc -l)
    cp "$MOCK_DATA_DIR/avatars/"* "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/avatars/" 2>/dev/null || true
    echo "  Copied $count avatar files"
else
    echo "  No avatars directory found"
fi

# Copy show logos
echo ""
echo "Copying show logos..."
if [ -d "$MOCK_DATA_DIR/shows/logos" ]; then
    count=$(find "$MOCK_DATA_DIR/shows/logos" -type f | wc -l)
    cp "$MOCK_DATA_DIR/shows/logos/"* "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/logos/" 2>/dev/null || true
    echo "  Copied $count logo files"
else
    echo "  No show logos directory found"
fi

# Copy show banners
echo ""
echo "Copying show banners..."
if [ -d "$MOCK_DATA_DIR/shows/banners" ]; then
    count=$(find "$MOCK_DATA_DIR/shows/banners" -type f | wc -l)
    cp "$MOCK_DATA_DIR/shows/banners/"* "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/banners/" 2>/dev/null || true
    echo "  Copied $count banner files"
else
    echo "  No show banners directory found"
fi

# Copy episode artwork
echo ""
echo "Copying episode artwork..."
if [ -d "$MOCK_DATA_DIR/episodes/artwork" ]; then
    count=$(find "$MOCK_DATA_DIR/episodes/artwork" -type f | wc -l)
    cp "$MOCK_DATA_DIR/episodes/artwork/"* "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/artwork/" 2>/dev/null || true
    echo "  Copied $count artwork files"
else
    echo "  No episode artwork directory found"
fi

# Copy event posters
echo ""
echo "Copying event posters..."
if [ -d "$MOCK_DATA_DIR/events/posters" ]; then
    count=$(find "$MOCK_DATA_DIR/events/posters" -type f | wc -l)
    cp "$MOCK_DATA_DIR/events/posters/"* "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/event-posters/" 2>/dev/null || true
    echo "  Copied $count event poster files"
else
    echo "  No event posters directory found"
fi

# Copy blog hero images
echo ""
echo "Copying blog hero images..."
if [ -d "$MOCK_DATA_DIR/blog/heroes" ]; then
    count=$(find "$MOCK_DATA_DIR/blog/heroes" -type f | wc -l)
    cp "$MOCK_DATA_DIR/blog/heroes/"* "$TARGET_ROOT/images/$MOCK_YEAR/$MOCK_MONTH/$MOCK_DAY/blog-heroes/" 2>/dev/null || true
    echo "  Copied $count blog hero files"
else
    echo "  No blog heroes directory found"
fi

echo ""
echo "========================================"
echo "Summary"
echo "========================================"
echo ""
echo "Files copied to $TARGET_ROOT:"
find "$TARGET_ROOT" -type f | wc -l
echo ""
echo "Directory structure:"
find "$TARGET_ROOT" -type d | sort
echo ""
echo "Mock images loaded successfully!"
