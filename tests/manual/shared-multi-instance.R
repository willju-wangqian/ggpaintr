# Manual demo: two `ptr_module_ui()` instances embedded in a user-authored
# Shiny app, sharing a `var()` consumer ("col") and a `text()` value
# ("title") via one `ptr_shared_ui()` / `ptr_shared_server()` pair.
#
# Click "Draw all" to redraw both modules. Switching the shared column
# updates both pickers; editing the shared title updates both subtitles.

library(shiny)
library(ggpaintr)

formulas <- c(
  'ggplot(mtcars) +
     geom_point(aes(x = var(shared = "col"), y = mpg)) +
     labs(subtitle = text(shared = "title"))',
  'ggplot(mtcars) +
     geom_line(aes(x = var(shared = "col"), y = hp)) +
     labs(subtitle = text(shared = "title"))'
)

ui <- fluidPage(
  titlePanel("ptr_shared_ui() multi-instance demo"),
  ptr_shared_ui(formulas),
  fluidRow(
    column(6, ptr_module_ui("plot_1", formulas[[1]])),
    column(6, ptr_module_ui("plot_2", formulas[[2]]))
  )
)

server <- function(input, output, session) {
  shared_state <- ptr_shared_server(formulas)
  ptr_module_server("plot_1", formulas[[1]], shared_state = shared_state)
  ptr_module_server("plot_2", formulas[[2]], shared_state = shared_state)
}

shinyApp(ui, server)
