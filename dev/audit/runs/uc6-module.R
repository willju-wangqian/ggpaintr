formula <- "ggplot(iris, aes(var, var, color = var)) + geom_point()"
ui <- shiny::fluidPage(
  shiny::titlePanel("My host app"),
  ptr_module_ui("p", formula)
)
server <- function(input, output, session) {
  ptr_module_server("p", formula)
}
shiny::shinyApp(ui, server)
