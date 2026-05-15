library(plotly)
formula <- "ggplot(iris, aes(x = var, y = var, color = var)) + geom_point()"
ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(5, ptr_module_ui("p", formula)),
    shiny::column(7, plotly::plotlyOutput("custom_plot", height = "500px"))
  )
)
server <- function(input, output, session) {
  state <- ptr_module_server("p", formula)
  output$custom_plot <- plotly::renderPlotly({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    plotly::ggplotly(res$plot)
  })
}
shiny::shinyApp(ui, server)
