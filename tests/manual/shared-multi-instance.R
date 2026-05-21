# Manual demo: two `ptr_ui()` instances embedded in a user-authored
# Shiny app, sharing a `ppVar()` consumer ("col") and a `ppText()` value
# ("title") via one `ptr_shared_ui()` / `ptr_shared_server()` pair.
#
# Click "Draw all" to redraw both modules. Switching the shared column
# updates both pickers; editing the shared title updates both subtitles.

library(shiny)
library(ggpaintr)

formulas <- c(
  'ggplot(mtcars) +
     geom_point(aes(x = ppVar(shared = "col"), y = mpg)) +
     labs(subtitle = ppText(shared = "title"))',
  'ggplot(mtcars) +
     geom_line(aes(x = ppVar(shared = "col"), y = hp)) +
     labs(subtitle = ppText(shared = "title"))'
)

ui <- fluidPage(
  titlePanel("ptr_shared_ui() multi-instance demo"),
  ptr_shared_ui(formulas),
  fluidRow(
    column(6, ptr_ui(formulas[[1]], "plot_1")),
    column(6, ptr_ui(formulas[[2]], "plot_2"))
  )
)

server <- function(input, output, session) {
  shared_state <- ptr_shared_server(formulas)
  ptr_server(formulas[[1]], "plot_1", shared_state = shared_state)
  ptr_server(formulas[[2]], "plot_2", shared_state = shared_state)
}

shinyApp(ui, server)
