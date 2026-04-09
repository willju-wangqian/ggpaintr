library(ggpaintr)
library(shiny)
library(shinyWidgets)

input_formula <- "
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num, alpha = num) +
  labs(title = text, x = text, y = text) +
  facet_wrap(expr) +
  theme(legend.position = text)"

# Replace NULL with a named list to customize UI labels, help text, and placeholders.
ui_text <- NULL

# Replace NULL with a named list of ptr_define_placeholder() calls to register custom placeholder types.
placeholders <- NULL

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
  ptr_state <- ptr_server(input, output, session, input_formula, ui_text = ui_text, placeholders = placeholders)

  # Add custom observers or outputs below.
  # observe({
  #   runtime_result <- ptr_state$runtime()
  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {
  #     message(runtime_result$code_text)
  #   }
  # })
}

shinyApp(ui, server)
