# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install.
#
# DIVERGED FROM VIGNETTE (shared_ui removal, 2026-05-28): `shared_ui` is no
# longer a supported argument (see ?ptr_shared). The vignette chunk
# `l2-shared` still shows the old `shared_ui = list(metric = ...)` form and is
# pending a follow-up rewrite; THIS fixture is the corrected shape. The shared
# `metric` widget is now a custom consumer placeholder `ppMetric` whose
# `build_ui` is the same `selectInput` the override used to supply, wired into
# the formula via `ppMetric(shared = 'metric')` -- customization comes from the
# placeholder's `build_ui`, not a `shared_ui` override.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# Custom consumer placeholder: a column picker rendered as a `selectInput`
# (the look the removed `shared_ui` override used to supply). Registration is
# process-local to this child app (project memory `shinytest2-appdir-pkgload`).
ppMetric <- ptr_define_placeholder_consumer(
  keyword      = "ppMetric",
  build_ui     = function(node, cols = character(), label = NULL,
                          selected = character(0), ...) {
    shiny::selectInput(
      node$id, label = label %||% "Metric",
      choices = cols,
      selected = if (length(selected)) selected[[1L]] else NULL
    )
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  default_arg  = ptr_arg_symbol_or_string()
)

plots <- list(
  "ggplot(iris, aes(x = ppMetric(shared = 'metric'), y = Sepal.Length, fill = Species)) + geom_boxplot()",
  "ggplot(iris, aes(x = ppMetric(shared = 'metric'), y = Sepal.Width,  fill = Species)) + geom_violin()"
)

obj <- ptr_shared(formulas = plots)

ui <- shiny::fluidPage(
  shiny::titlePanel("My host app"),
  ptr_shared_panel(obj),
  shiny::fluidRow(
    shiny::column(6, ptr_ui(plots[[1]], "plot_1", shared = obj)),
    shiny::column(6, ptr_ui(plots[[2]], "plot_2", shared = obj))
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "plot_1", shared_state = sh)
  ptr_server(plots[[2]], "plot_2", shared_state = sh)
}

shiny::shinyApp(ui, server)
