# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the ADR 0012 §3.7
# bug-3a regression test (in-filter ppVar picker populates after upload
# under `|>` input, and the in-aes ppVar picker populates from the same
# upstream resolution path — verifying the per-layer fast-path deletion
# did not break the in-aes branch). Placeholder ids under the formula:
# in-filter ppVar -> `ggplot_2_1_1_ppVar_NA`, in-aes ppVars ->
# `ggplot_1_1_ppVar_NA` / `ggplot_1_2_ppVar_NA`.
ptr_app(
  "ppUpload |> dplyr::filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
)
