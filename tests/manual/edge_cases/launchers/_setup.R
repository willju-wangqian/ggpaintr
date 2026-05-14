# Shared boot for every per-edge-case launcher.
# Sourced from each launcher; expects the launcher to be invoked from the
# repo root via Rscript so the relative `.` resolves to the package.
#
# After source(_setup.R), the launcher has:
#   library(ggpaintr) functions loaded via load_all
#   library(shiny), library(ggplot2), library(dplyr)
#   FIXTURES_DIR / CSS_DIR pointing at the edge-case fixture trees
#   PORT = 4321

PORT <- 4321L
FIXTURES_DIR <- "tests/manual/edge_cases/fixtures"
CSS_DIR      <- "tests/manual/edge_cases/css"
stopifnot(dir.exists(FIXTURES_DIR), dir.exists(CSS_DIR))

devtools::load_all(".", export_all = FALSE, quiet = TRUE)
suppressPackageStartupMessages({
  library(shiny); library(ggplot2); library(dplyr)
})
