# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> lifted: vignettes/ggpaintr-customization.Rmd chunk `ptr-app-bslib-source`.
#     That chunk is eval=FALSE and displays the *source* of the exported
#     ptr_app_bslib(); the runnable equivalent is calling the exported
#     wrapper it documents (the customization vignette presents bslib via
#     this function). >>>
ptr_app_bslib(
  "ggplot(iris, aes(x = ppVar, y = ppVar, color = ppVar)) + geom_point()"
)
# <<<
