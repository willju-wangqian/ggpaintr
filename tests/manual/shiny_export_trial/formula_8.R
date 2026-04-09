library(ggpaintr)
library(shiny)
library(shinyWidgets)

input_formula <- "
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point() +
  labs(title = text) +
  facet_wrap(expr)"

# Edit custom_ui_text to customize UI labels, help text, and placeholders.
custom_ui_text <- list(shell = list(
  title = list(label = "Exploratory Plot Builder"),
  draw_button = list(label = "Render plot")
), params = list(
  x = list(var = list(label = "Pick the field for the x-axis")),
  y = list(var = list(label = "Pick the field for the y-axis")),
  title = list(text = list(label = "Chart title"))
), layers = list(
  facet_wrap = list(expr = list(`__unnamed__` = list(label = "Split the plot by")))
))

ui_text <- ptr_merge_ui_text(custom_ui_text)

title_copy <- ptr_resolve_ui_text("title", ui_text = ui_text)
draw_copy <- ptr_resolve_ui_text("draw_button", ui_text = ui_text)
export_copy <- ptr_resolve_ui_text("export_button", ui_text = ui_text)

ui <- fluidPage(
  titlePanel(title_copy$label),
  sidebarLayout(
    sidebarPanel(
      # Modify or add controls here.
      uiOutput("controlPanel"),
      actionButton("draw", draw_copy$label),
      downloadButton("shinyExport", export_copy$label)
    ),
    mainPanel(
      # Modify or add outputs here.
      plotOutput("outputPlot"),
      uiOutput("outputError"),
      verbatimTextOutput("outputCode")
    )
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server(input, output, session, input_formula, ui_text = ui_text)

  # Add custom observers or outputs below.
  # observe({
  #   runtime_result <- ptr_state$runtime()
  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {
  #     message(runtime_result$code_text)
  #   }
  # })
}

shinyApp(ui, server)
