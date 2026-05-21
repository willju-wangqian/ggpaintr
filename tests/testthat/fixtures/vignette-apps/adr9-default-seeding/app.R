# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. This fixture is NOT vignette-paired — it covers the
# ADR-0009 default-argument seeding path: a placeholder's `default = ...` in
# the formula reaches the rendered widget's initial value via PLAN-01 (the
# validator factories), PLAN-06 (the parser populating node$default), and
# PLAN-07 (invoke_build_ui threading node$default into the widget hook's
# `selected` / `value` arg). Browser-only because the assertion observes
# the Shiny input's initial DOM/value state with no user interaction.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  "ggplot(mtcars,
          aes(x = ppVar(mpg),
              y = ppVar(cyl))) +
     geom_point()"
)
