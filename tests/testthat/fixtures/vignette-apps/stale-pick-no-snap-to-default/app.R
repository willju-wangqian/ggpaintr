# Boot scaffolding (NOT vignette code): Tier-3 regression for stale-pick
# under upstream data swap. User picks a column, then the upstream
# dataset swaps to one that doesn't contain that column -- the picker
# must drop the stale pick (via intersect() inside ptr_builtin_var_build_ui)
# but must NOT snap back to the formula default. The closure-flag fix
# is responsible: on the post-swap re-render, `has_rendered = TRUE` so
# `current = <stale-col>` flows through verbatim (intersect then empties
# it at the hook layer), never re-routing through invoke_build_ui's
# branch A default-injection.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_define_placeholder_source(
  keyword  = "dataset",
  build_ui = function(node, label = NULL, ...) {
    selectInput(node$id, label = label %||% "Dataset",
                choices = c("mtcars", "iris"), selected = "mtcars")
  },
  resolve_data = function(value, node, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    switch(value, mtcars = mtcars, iris = iris, NULL)
  }
)

ptr_app("ggplot(dataset, aes(x = ppVar(mpg))) + geom_point()")
