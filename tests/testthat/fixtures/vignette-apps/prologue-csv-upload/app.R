# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: NOT vignette-paired -- this fixture exists for ADR 0025 §4 /
# PLAN-04 (code-panel upload prologue). It drives the pipeline-head
# ppUpload through an actual file upload and asserts the code panel
# leads with the `<auto_name> <- read.csv("<file>")` prologue line.
ptr_app(
  "ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"
)
