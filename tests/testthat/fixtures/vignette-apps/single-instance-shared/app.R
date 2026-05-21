# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `single-instance-shared` >>>
# One instance, one key used twice: the widget auto-renders inline. No coordinator.
ptr_app(
  "ggplot(iris, aes(x = ppVar(shared = 'col'), y = ppVar - ppVar(shared = 'col'),
                    color = Species)) + geom_point()"
)
# <<<
