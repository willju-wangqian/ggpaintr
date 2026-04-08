#!/usr/bin/env bash
set -euo pipefail

cd /workspace/ggpaintr

git config --global --add safe.directory /workspace/ggpaintr

/workspace/ggpaintr/working_scripts/codex/prepare-site-dir.sh /workspace/ggpaintr

R -q -e "pak::pkg_install(c('local::.', 'devtools', 'testthat', 'rmarkdown', 'knitr', 'pkgdown', 'styler'))"

R -q -e "cat('pkgdown=', as.character(utils::packageVersion('pkgdown')), '\n', 'devtools=', as.character(utils::packageVersion('devtools')), '\n', sep = '')"

echo
echo "Container bootstrap finished."
echo "Repo: /workspace/ggpaintr"
if codex_version="$(codex --version 2>/dev/null | tail -n 1)"; then
  echo "Codex: $codex_version"
else
  echo "Codex: failed to start"
fi
