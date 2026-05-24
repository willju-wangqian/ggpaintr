# ADR 0020 PLAN-02 fixture — ppVerbOff(hide=TRUE) e2e.
# Loads the in-development package into this child app process so the e2e
# test exercises dev source, not any stale system install of ggpaintr.
# The filter stage contains a `ppNum` placeholder so the per-layer Data
# panel emits a `.ptr-stage` block whose head-checkbox boot value reflects
# the carrier ptr_call's `default_stage_enabled = FALSE` (the wrapped TRUE
# in ppVerbOff(..., TRUE) means hide=TRUE).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
ptr_app(
  quote(ggplot(
    mtcars |> ppVerbOff(filter(mpg > ppNum), TRUE),
    aes(x = mpg, y = wt)
  ) + geom_point())
)
