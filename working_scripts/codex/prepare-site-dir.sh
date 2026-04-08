#!/usr/bin/env bash
set -euo pipefail

ROOT_DIR="${1:-/workspace/ggpaintr}"
SITE_DIR="$ROOT_DIR/docs"
REFERENCE_DIR="$SITE_DIR/reference"

mkdir -p "$REFERENCE_DIR"

# pkgdown updates files in-place under docs/ and can fail on bind-mounted
# worktrees if a previous run left directories without owner write bits.
chmod -R u+rwX "$SITE_DIR"
chmod u+rwx "$SITE_DIR" "$REFERENCE_DIR"

echo "Prepared pkgdown site directory: $SITE_DIR"
