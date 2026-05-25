# ADR 0020 PLAN-02 fixture — spec= at boot overrides ppLayerOff default.
# Loads the in-development package into this child app process so the e2e
# test exercises dev source, not any stale system install of ggpaintr.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  formula = "ggplot() + ppLayerOff(geom_point(), TRUE)",
  spec = list(geom_point_checkbox = TRUE)
)
