formula <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()"
ui <- shiny::fluidPage(
  shiny::actionButton("add_log", "Toggle log-scale"),
  ptr_module_ui("p", formula)
)
server <- function(input, output, session) {
  state <- ptr_module_server("p", formula)
  shiny::observeEvent(input$add_log, {
    ptr_gg_extra(state, ggplot2::scale_x_log10())
  })
}
shiny::shinyApp(ui, server)
