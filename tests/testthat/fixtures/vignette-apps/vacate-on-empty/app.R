# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired — this fixture exists for ADR 0025 §7 /
# PLAN-05 (vacate-on-empty A1). Formula matches `prologue-csv-upload`'s
# pipeline-head `ppUpload()` shape so the same `_ppUpload_NA` id contract
# applies, plus an `aes(x = ppVar(...))` so a consumer picker populates
# after upload. The test asserts vacate via the user-visible code-panel
# prologue line (present after upload, absent after clearing the
# shortcut textbox) — load-bearing observable downstream of the
# vacate_source_binding() mechanism asserted by the unit test in
# test-vacate-on-empty.R.
ptr_app("ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()")
