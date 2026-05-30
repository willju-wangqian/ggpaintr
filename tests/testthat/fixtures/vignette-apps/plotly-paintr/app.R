# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-gallery.Rmd chunk `custom-placeholders-setup`
#     (the `range` registration this chunk depends on) + chunk `plotly-paintr` >>>
ptr_define_placeholder_value(
  keyword = "range",
  build_ui = function(node, label = NULL, ...) {
    shiny::sliderInput(node$id, label = label %||% "Range",
                       min = -100, max = 100, value = c(0, 50), step = 0.1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    rlang::expr(c(!!value[1], !!value[2]))
  },
  ui_text_defaults = list(label = "Range for {param}")
)

library(plotly)

formula <- "ggplot(data = mpg,
                   aes(x = ppVar, y = ppVar, color = ppVar,
                       text = paste(manufacturer, model, sep = ' '))) +
              geom_point(size = ppNum, alpha = ppNum) +
              coord_cartesian(xlim = range, ylim = range)"

ui <- fluidPage(
  fluidRow(
    column(5, ptr_ui(formula, "plotly_demo")),
    column(7, plotly::plotlyOutput("interactive_plot", height = "500px"))
  )
)

server <- function(input, output, session) {
  state <- ptr_server(formula, "plotly_demo")
  output$interactive_plot <- plotly::renderPlotly({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    plotly::ggplotly(res$plot, tooltip = "text")
  })
}

shinyApp(ui, server)
# <<<
