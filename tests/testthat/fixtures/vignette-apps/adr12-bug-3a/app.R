# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the ADR 0012 §3.7 /
# PLAN-04 bug-3a regression test (in-filter ppVar picker populates after
# upload under `|>` input, and the in-aes ppVar picker populates from the
# same upstream resolution path — verifying the per-layer fast-path
# deletion did not break the in-aes branch).
#
# Two verb stages above the source (`dplyr::filter(...)` then
# `dplyr::mutate(...)`) so PLAN-02's GATE 0 accepts the lift and the
# layer's data_arg canonicalises to a `ptr_pipeline`. A single verb
# stage reduces to a single-stage chain at the layer level and GATE 0
# correctly rejects it — the consumer-uniformity path that populates
# the in-filter ppVar picker only fires on the lifted `ptr_pipeline`
# shape (per PLAN-04's prune_walk.ptr_pipeline branch). Placeholder ids
# under the two-verb formula: in-filter ppVar -> `ggplot_2_1_1_ppVar_NA`,
# in-aes ppVars -> `ggplot_1_1_ppVar_NA` / `ggplot_1_2_ppVar_NA`
# (verified by direct ptr_translate observation).
ptr_app(
  "ppUpload |> dplyr::filter(ppVar > ppNum) |> dplyr::mutate(y = ppVar * 2) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
)
