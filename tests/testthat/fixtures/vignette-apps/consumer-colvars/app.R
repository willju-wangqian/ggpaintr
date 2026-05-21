# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-customization.Rmd chunk `consumer-colvars`
#     (registration) + chunk `consumer-colvars-app` (the app) >>>
ptr_define_placeholder_consumer(
  keyword       = "colvars",
  build_ui      = function(node, cols = character(), label = NULL,
                           selected = character(0), ...) {
    selectInput(node$id, label = label %||% "Columns",
                choices = cols, selected = intersect(selected, cols),
                multiple = TRUE)
  },
  resolve_expr  = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)

ptr_app("mtcars |> dplyr::select(colvars) |> 
        ggplot(aes(x = ppVar, y = ppVar)) + geom_point()")
# <<<
