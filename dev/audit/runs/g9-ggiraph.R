library(ggiraph); library(colourpicker)
ptr_clear_placeholder()
ptr_define_placeholder_value(
  keyword       = "color",
  build_ui      = function(node, label = NULL, ...) {
    colourpicker::colourInput(node$id, label = label %||% "Highlight colour",
                              value = "#e63946")
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value) || identical(value, "")) return(NULL)
    as.character(value)
  },
  copy_defaults = list(label = "Highlight colour for {param}")
)
formula <- "ggplot(data = mpg, aes(x = var, y = var)) +
              geom_point_interactive(aes(tooltip = var),
                                     size = num, color = color)"
ui <- shiny::fluidPage(
  shiny::fluidRow(
    shiny::column(5, ptr_module_ui(formula, "ggiraph_demo")),
    shiny::column(7, ggiraph::girafeOutput("interactive_plot", height = "500px"))
  )
)
server <- function(input, output, session) {
  state <- ptr_module_server(formula, "ggiraph_demo")
  output$interactive_plot <- ggiraph::renderGirafe({
    res <- state$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    ggiraph::girafe(code = print(res$plot))
  })
}
shiny::shinyApp(ui, server)
