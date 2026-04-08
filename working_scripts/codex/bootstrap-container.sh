#!/usr/bin/env bash
set -euo pipefail

cd /workspace/ggpaintr

git config --global --add safe.directory /workspace/ggpaintr

R -q -e "pak::pkg_install(c('local::.', 'testthat', 'rmarkdown', 'knitr', 'pkgdown', 'styler'))"

echo
echo "Container bootstrap finished."
echo "Repo: /workspace/ggpaintr"
echo "Codex: $(codex --version | tail -n 1)"
