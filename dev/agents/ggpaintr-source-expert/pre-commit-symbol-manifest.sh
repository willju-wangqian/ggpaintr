#!/bin/sh
# Pre-commit sidecar — rebuild the ggpaintr-source-expert symbol manifest
# when R/ changes.
#
# This project routes all git hooks through `dev/githooks/` via
# `core.hooksPath`. The orchestrator hook is `dev/githooks/pre-commit`
# (currently enforces /implementable plan stamps). This file is a SIDECAR
# invoked from that orchestrator — do NOT install it as the top-level
# pre-commit (that would override the implementable check).
#
# Install (one-time): append this line to `dev/githooks/pre-commit`
#
#   sh dev/agents/ggpaintr-source-expert/pre-commit-symbol-manifest.sh || exit 1
#
# Each sidecar runs in its own subprocess so its exit code bubbles up but
# its `set -e` doesn't affect the orchestrator.
#
# What it does:
#   1. If any staged file under R/ matches *.R, rebuild symbol-lines.json.
#   2. Stage the refreshed JSON so the commit includes it.
#   3. Optionally also refresh function-level citations in knowledge/*.md
#      (commented out by default — uncomment if you want auto-refresh on commit).
#
# Aborts the commit if the manifest build fails.

set -e

# Find staged R source files (added/modified, not deleted).
staged_r=$(git diff --cached --name-only --diff-filter=AM | grep -E '^R/.*\.R$' || true)

if [ -z "$staged_r" ]; then
  exit 0
fi

echo "[pre-commit] R/ changed — rebuilding symbol-lines.json..."

if ! command -v Rscript >/dev/null 2>&1; then
  echo "[pre-commit] ERROR: Rscript not on PATH; cannot rebuild manifest." >&2
  exit 1
fi

if ! Rscript dev/agents/ggpaintr-source-expert/build-symbol-manifest.R; then
  echo "[pre-commit] manifest build FAILED — commit aborted" >&2
  exit 1
fi

git add dev/agents/ggpaintr-source-expert/symbol-lines.json

# --- Optional: also auto-refresh curriculum citations on commit ---
# Uncomment the block below if you want function-level line citations in
# knowledge/*.md to auto-rewrite from the new manifest. By default this is
# off — you may not want every commit to also touch the markdown.
#
# echo "[pre-commit] refreshing curriculum citations..."
# if Rscript dev/agents/ggpaintr-source-expert/refresh-curriculum-citations.R --apply; then
#   git add dev/agents/ggpaintr-source-expert/knowledge/*.md
# else
#   echo "[pre-commit] citation refresh failed (manifest rebuilt OK); continuing"
# fi

exit 0
