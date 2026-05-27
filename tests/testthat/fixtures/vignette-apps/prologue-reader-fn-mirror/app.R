# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- ADR 0025 §4 / PLAN-04. Reused across
# csv/tsv/xlsx/rds extension drivers in test-prologue-reader-fn-mirror.R;
# each sub-test boots a fresh AppDriver and uploads a different fixture
# to assert the right reader-fn name appears in the prologue.
ptr_app(
  "ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"
)
