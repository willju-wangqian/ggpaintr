# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. This fixture is NOT vignette-paired — it covers the
# ADR-0009 named_args registry slot end-to-end: a custom placeholder
# declares named_args = list(hint = ptr_default_string()), the formula
# passes hint = "..." in the call form, and the value flows
# parser -> node$named_args (PLAN-06)
#   -> invoke_build_ui's do.call (PLAN-07)
#   -> the custom build_ui hook's `named_args` formal
#   -> a visible textInput placeholder= attribute in the DOM.
# Browser-only because the assertion is on the rendered widget attribute.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_define_placeholder_value(
  keyword    = "hinted_text",
  named_args = list(hint = ptr_default_string()),
  build_ui   = function(node, label = NULL, named_args = list(), ...) {
    shiny::textInput(
      node$id,
      label       = label %||% "Text",
      placeholder = named_args$hint %||% ""
    )
  },
  resolve_expr = function(value, node, ...) {
    if (!is.character(value) || !nzchar(value)) return(NULL)
    value
  }
)

ptr_app(
  "ggplot(mtcars, aes(x = ppVar(mpg),
                      y = ppVar(cyl))) +
     geom_point() +
     labs(title = hinted_text(hint = 'Type your plot title'))"
)
