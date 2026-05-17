# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-customization.Rmd chunk `value-range`
#     (registration) + chunk `value-range-app` (the app) >>>
ptr_define_placeholder_value(
  keyword       = "range",
  build_ui      = function(node, label = NULL, ...) {
    sliderInput(node$id, label = label %||% "Range",
                min = 0, max = 100, value = c(0, 100), step = 0.1)
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Range for {param}")
)

ptr_app("ggplot(mtcars, aes(mpg, hp)) + geom_point() + xlim(range)")
# <<<
