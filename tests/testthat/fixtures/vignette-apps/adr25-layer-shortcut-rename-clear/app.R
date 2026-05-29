# Boot scaffolding (NOT vignette code): load the in-development package so the
# e2e test exercises dev source, not a stale system install.
#
# ADR 0025 contract (ii) regression fixture (layer-source variant). The bug:
# shortcut-renaming a BARE-DATA-LAYER `ppUpload()` to a different dataset did
# NOT clear the layer's column pickers (the layer source binds its name under
# `layer_name`, not the source node id, so the new-source identity edge never
# tripped). `ChickWeight` and `PlantGrowth` both carry a `weight` column, so a
# stale pick survives visibly (a non-shared column would be silently dropped by
# the picker's intersect() and mask the bug).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(wt))) +
    geom_point() +
    geom_rug(data = ppUpload(), aes(x = ppVar(carb), y = ppVar(gear)), inherit.aes = FALSE)
)
