#!/usr/bin/env bash
set -euo pipefail

cd /workspace/ggpaintr

git config --global --add safe.directory /workspace/ggpaintr

R -q -e "pak::pkg_install(c('local::.', 'testthat', 'rmarkdown', 'knitr', 'pkgdown', 'styler'))"

echo
echo "Container bootstrap finished."
echo "Repo: /workspace/ggpaintr"
if codex_version="$(codex --version 2>/dev/null | tail -n 1)"; then
  echo "Codex: $codex_version"
else
  echo "Codex: failed to start"
fi
