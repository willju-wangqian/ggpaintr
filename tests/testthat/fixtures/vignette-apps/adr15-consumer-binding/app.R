# Boot scaffolding (NOT vignette code): load the in-development package so the
# e2e test exercises dev source, not a stale system install. Fixture for
# ADR 0015 PLAN-01: a bare ppUpload data-source layer carrying TWO consumer
# pickers under one layer — one non-shared (`ppVar(mpg)` at x, exercising
# `ptr_setup_consumer_uis()`) and one shared (`ppVar(shared = "v")` at y,
# exercising `ptr_bind_shared_consumer_uis()`). Both must populate after a
# file lands in `geom_point_0_ppUpload_NA`, with NO inner-tab navigation.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  ggplot() + geom_point(data = ppUpload(df), aes(x = ppVar(mpg), y = ppVar(shared = "v")))
)
