# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for ADR 0025 §2 /
# PLAN-03 (UI mutex, boot-seed exception). `ppUpload(penguins)` seeds the
# shortcut textInput with "penguins" at boot (ADR 0010); the test asserts
# that boot-seed write does NOT trip either mutex observer (both are
# wrapped in `ignoreInit = TRUE`), so the fileInput stays empty + live
# rather than being reset by a phantom "first interaction".
penguins <- data.frame(
  species         = c("Adelie", "Gentoo", "Chinstrap"),
  bill_length_mm  = c(39.1, 50.0, 49.5),
  bill_depth_mm   = c(18.7, 15.5, 17.5),
  stringsAsFactors = FALSE
)
ptr_app(
  "ppUpload(penguins) |> ggplot(aes(ppVar, ppVar)) + geom_point()"
)
