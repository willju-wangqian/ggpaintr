# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: archive/retired_vignettes/ggpaintr-customization.Rmd chunk `source-dataset`
#     (registration) + chunk `source-dataset-app` (the app) >>>
ptr_define_placeholder_source(
  keyword       = "dataset",
  build_ui      = function(node, label = NULL, ...) {
    selectInput(node$id, label = label %||% "Built-in dataset",
                choices = c("iris", "mtcars", "diamonds", "economics"))
  },
  resolve_data  = function(value, node, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    switch(value,
           iris      = iris,
           mtcars    = mtcars,
           diamonds  = ggplot2::diamonds,
           economics = ggplot2::economics,
           NULL)
  },
  ui_text_defaults = list(label = "Built-in dataset for {param}")
)

ptr_app("ggplot(dataset, aes(ppVar, ppVar)) + geom_point()")
# <<<
