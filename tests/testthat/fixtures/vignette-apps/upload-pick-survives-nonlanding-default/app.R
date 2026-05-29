# Boot scaffolding (NOT vignette code): load the in-development package so the
# e2e test exercises dev source, not a stale system install.
#
# Regression fixture for the 2026-05-29 bug: a layer-data `ppUpload()` source
# with a consumer (`ppVar(carb)`) whose formula default is NOT a column of the
# dataset the user loads via the shortcut (`iris` has no `carb`/`gear`). The
# `has_rendered` latch only flipped when the default landed, so the picker
# never latched -> the user's column pick was discarded on every Update Plot
# click. The root layer uses a literal `mtcars` (no source placeholder) so the
# root pickers seed normally and the layer source is the only swap point.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(wt))) +
    geom_point() +
    geom_rug(data = ppUpload(),
             aes(x = ppVar(carb), y = ppVar(gear)),
             inherit.aes = FALSE)
)
