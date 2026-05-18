# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by
# diff). The two `ptr_module_ui()` calls pass `shared = obj` (the W2 #B2
# panel-key exclude) in BOTH the chunk and here, so the panel key `sz` is
# rendered once (standalone panel only), never inline.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `l2-shared-partition` >>>
plots <- list(
  "ggplot(iris, aes(x = var(shared = 'ax1'), y = var(shared = 'ax1'),
                    color = Species)) + geom_point(size = num(shared = 'sz'))",
  "ggplot(iris, aes(x = var(shared = 'ax2'), y = Sepal.Width,
                    color = Species)) + geom_point(size = num(shared = 'sz'))"
)

obj <- ptr_shared(
  formulas  = plots,
  shared_ui = list(sz = function(id) shiny::sliderInput(id, "Size", 1, 6, 3))
)
# sz → both formulas → standalone panel.  ax1 → only plot_1's inline section.
# ax2 → only plot_2's inline section.
obj$panel_keys                       # "sz"

ui <- shiny::fluidPage(
  ptr_shared_panel(obj),             # holds sz only
  shiny::fluidRow(
    shiny::column(6, ptr_module_ui("plot_1", plots[[1]], shared = obj)),  # ax1 inline here
    shiny::column(6, ptr_module_ui("plot_2", plots[[2]], shared = obj))   # ax2 inline here
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_module_server("plot_1", plots[[1]], shared_state = sh)
  ptr_module_server("plot_2", plots[[2]], shared_state = sh)
}

shiny::shinyApp(ui, server)
# <<<
