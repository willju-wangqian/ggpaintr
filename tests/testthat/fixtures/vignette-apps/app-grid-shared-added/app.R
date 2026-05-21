# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `app-grid-shared-added` >>>
ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = ppVar(shared = 'metric'), y = Sepal.Length, fill = Species)) +
       geom_boxplot()",
    "ggplot(iris, aes(x = ppVar(shared = 'metric'), y = Sepal.Width, fill = Species)) +
       geom_violin()"
  ),
  shared_ui = list(metric = function(id) selectInput(id, "Metric", names(iris)))
)
# <<<
