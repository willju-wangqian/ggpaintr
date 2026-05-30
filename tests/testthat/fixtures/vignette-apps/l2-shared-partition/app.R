# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. The two `ptr_ui()` calls pass `shared = obj` (the W2 #B2
# panel-key exclude), so the panel key `sz` is rendered once (standalone panel
# only), never inline.
#
# DIVERGED FROM VIGNETTE (shared_ui removal, 2026-05-28): `shared_ui` is no
# longer supported (see ?ptr_shared). The vignette chunk `l2-shared-partition`
# still shows `shared_ui = list(sz = ...)` and is pending a follow-up rewrite;
# THIS fixture is the corrected shape. `sz`'s 1-6 slider look is now carried by
# a custom value placeholder `ppSize` whose `build_ui` is the slider, used via
# `ppSize(shared = 'sz')`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

# Custom value placeholder: a 1-6 slider (the look the removed `shared_ui`
# override used to supply). Process-local registration.
ppSize <- ptr_define_placeholder_value(
  keyword      = "ppSize",
  positional_arg  = ptr_arg_numeric(),
  build_ui     = function(node, label = NULL, selected = NULL, ...) {
    val <- if (is.null(selected) || length(selected) == 0L) 3 else
      suppressWarnings(as.numeric(selected[[1L]]))
    if (length(val) != 1L || is.na(val)) val <- 3
    shiny::sliderInput(node$id, label = label %||% "Size", min = 1, max = 6, value = val)
  },
  resolve_expr = function(value, node, ...) {
    out <- suppressWarnings(as.numeric(value))
    if (length(out) != 1L || is.na(out)) return(NULL)
    out
  }
)

plots <- list(
  "ggplot(iris, aes(x = ppVar(shared = 'ax1'), y = ppVar - ppVar(shared = 'ax1'),
                    color = Species)) + geom_point(size = ppSize(shared = 'sz'))",
  "ggplot(iris, aes(x = ppVar(shared = 'ax2'), y = Sepal.Width,
                    color = Species)) + geom_point(size = ppSize(shared = 'sz'))"
)

obj <- ptr_shared(formulas = plots)
# sz → both formulas → standalone panel.  ax1 → only plot_1's inline section.
# ax2 → only plot_2's inline section.
obj$panel_keys                       # "sz"

ui <- shiny::fluidPage(
  ptr_shared_panel(obj),             # holds sz only
  shiny::fluidRow(
    shiny::column(6, ptr_ui(plots[[1]], "plot_1", shared = obj)),  # ax1 inline
    shiny::column(6, ptr_ui(plots[[2]], "plot_2", shared = obj))   # ax2 inline
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "plot_1", shared_state = sh)
  ptr_server(plots[[2]], "plot_2", shared_state = sh)
}

shiny::shinyApp(ui, server)
