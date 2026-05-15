formula <- "ggplot(iris, aes(var, var)) + geom_point()"
ui <- shiny::fluidPage(ptr_controls_ui(formula = formula), shiny::plotOutput("ptr_plot"))
server <- function(input, output, session) {
  state <- ptr_init_state(formula = formula)
  ptr_register_plot(output, state)
}
shiny::shinyApp(ui, server)
