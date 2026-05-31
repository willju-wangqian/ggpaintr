# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- ADR 0025 §4 / PLAN-04. Two `ppUpload()`
# sources in one formula -- the multi-source ordering scenario: the code
# panel must emit exactly two prologue lines in formula / declaration
# order.
ptr_app(
  "ggplot() + geom_point(data = ppUpload(), aes(x = ppVar('mpg'), y = ppVar('cyl'))) + geom_line(data = ppUpload(), aes(x = ppVar('mpg'), y = ppVar('wt')))"
)
