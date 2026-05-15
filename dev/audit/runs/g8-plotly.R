source("dev/audit/runs/_gallery_helpers.R", local = TRUE)
library(plotly)
formula <- "ggplot(data = mpg,
                   aes(x = var, y = var, color = var,
                       text = paste(manufacturer, model, sep = ' '))) +
              geom_point(size = num, alpha = num) +
              coord_cartesian(xlim = range, ylim = range)"
ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(5, ptr_module_ui("plotly_demo", formula)),
    shiny::column(7, plotly::plotlyOutput("interactive_plot", height = "500px"))
  )
)
server <- function(input, output, session) {
  state <- ptr_module_server("plotly_demo", formula)
  output$interactive_plot <- plotly::renderPlotly({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    plotly::ggplotly(res$plot, tooltip = "text")
  })
}
shiny::shinyApp(ui, server)
