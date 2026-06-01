# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install.
#
# DIVERGED FROM VIGNETTE (shared_ui removal, 2026-05-28): `shared_ui` is no
# longer a supported argument of `ptr_app_grid()` (see ?ptr_shared). The
# vignette chunk `app-grid-shared-added` still shows the old `shared_ui =
# list(metric = ...)` form and is pending a follow-up rewrite; THIS fixture is
# the corrected shape. The shared `metric` widget is now a custom consumer
# placeholder `ppMetric` whose `build_ui` is the same `selectInput`, wired into
# the formulas via `ppMetric(shared = 'metric')`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# Custom consumer placeholder: column picker rendered as a `selectInput`
# (the look the removed `shared_ui` override used to supply). Process-local
# registration (project memory `shinytest2-appdir-pkgload`).
ppMetric <- ptr_define_placeholder_consumer(
  keyword      = "ppMetric",
  build_ui     = function(node, cols = character(), label = NULL,
                          selected = character(0), ...) {
    selectInput(
      node$id, label = label %||% "Metric",
      choices = cols,
      selected = if (length(selected)) selected[[1L]] else NULL
    )
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  parse_positional_arg  = ptr_arg_symbol_or_string()
)

ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = ppMetric(shared = 'metric'), y = Sepal.Length, fill = Species)) +
       geom_boxplot()",
    "ggplot(iris, aes(x = ppMetric(shared = 'metric'), y = Sepal.Width, fill = Species)) +
       geom_violin()"
  )
)
