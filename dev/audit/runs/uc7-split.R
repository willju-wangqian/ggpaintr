formula <- "ggplot(iris, aes(var, var, color = var)) + geom_point()"
ui <- shiny::fluidPage(
  shiny::titlePanel("Custom layout"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(ptr_controls_ui(formula = formula)),
    shiny::mainPanel(shiny::tags$h3("Plot + code + errors"), ptr_outputs_ui())
  )
)
server <- function(input, output, session) {
  ptr_server(input, output, session, formula)
}
shiny::shinyApp(ui, server)
