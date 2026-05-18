# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. Everything below the >>> marker is verbatim-equivalent to
# the named vignette chunk (factory <=> vignette equivalence, reviewable by diff).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# >>> verbatim: vignettes/ggpaintr-use-cases.Rmd chunk `l3-plotly` >>>
formula <- "ggplot(iris, aes(x = var, y = var, color = var)) + geom_point()"

ui <- ptr_ui_page(
  shiny::fluidRow(
    shiny::column(5, ptr_ui_controls("plot1", formula)),                   # widgets only
    shiny::column(7, plotly::plotlyOutput(shiny::NS("plot1")("custom_plot"),
                                          height = "500px") |>
                    ptr_ui_toggle_code(ptr_ui_code("plot1")))               # your own output
  )
)
server <- function(input, output, session) {
  shiny::moduleServer("plot1", function(input, output, session) {
    state <- ptr_server(input, output, session, formula)   # runtime wired; ptr_plot unbound = inert
    output$custom_plot <- plotly::renderPlotly({
      res <- state$runtime()
      shiny::req(isTRUE(res$ok), res$plot)
      plotly::ggplotly(res$plot)
    })
  })
}

shiny::shinyApp(ui, server)
# <<<
