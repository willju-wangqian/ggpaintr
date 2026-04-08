#!/usr/bin/env bash
set -euo pipefail

cd /workspace/ggpaintr

git config --global --add safe.directory /workspace/ggpaintr

/workspace/ggpaintr/working_scripts/codex/prepare-site-dir.sh /workspace/ggpaintr

export R_LIBS_USER="${R_LIBS_USER:-/tmp/Rlibs}"
mkdir -p "$R_LIBS_USER"

R -q -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); options(pkgType = 'source'); install.packages(c('remotes', 'roxygen2', 'pkgload', 'rcmdcheck', 'testthat', 'rmarkdown', 'knitr', 'pkgdown', 'styler'), repos = 'https://cloud.r-project.org', lib = Sys.getenv('R_LIBS_USER'))"

R -q -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); options(pkgType = 'source'); remotes::install_local('.', upgrade = 'never', dependencies = TRUE, lib = Sys.getenv('R_LIBS_USER'))"

R -q -e ".libPaths(c(Sys.getenv('R_LIBS_USER'), .libPaths())); stopifnot(requireNamespace('pkgdown', quietly = TRUE)); stopifnot(requireNamespace('roxygen2', quietly = TRUE)); stopifnot(requireNamespace('pkgload', quietly = TRUE)); stopifnot(requireNamespace('rcmdcheck', quietly = TRUE)); cat('R_LIBS_USER=', Sys.getenv('R_LIBS_USER'), '\n', 'pkgdown=', as.character(utils::packageVersion('pkgdown')), '\n', 'roxygen2=', as.character(utils::packageVersion('roxygen2')), '\n', 'pkgload=', as.character(utils::packageVersion('pkgload')), '\n', 'rcmdcheck=', as.character(utils::packageVersion('rcmdcheck')), '\n', sep = '')"

echo
echo "Container bootstrap finished."
echo "Repo: /workspace/ggpaintr"
if codex_version="$(codex --version 2>/dev/null | tail -n 1)"; then
  echo "Codex: $codex_version"
else
  echo "Codex: failed to start"
fi
