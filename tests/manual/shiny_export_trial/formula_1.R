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

ids <- ptr_build_ids(control_panel = "controlPanel", draw_button = "draw", export_button = "shinyExport", plot_output = "outputPlot", error_output = "outputError", code_output = "outputCode")

title_copy <- ptr_resolve_ui_text("title", ui_text = ui_text)
draw_copy <- ptr_resolve_ui_text("draw_button", ui_text = ui_text)
export_copy <- ptr_resolve_ui_text("export_button", ui_text = ui_text)

ui <- fluidPage(
  titlePanel(title_copy$label),
  sidebarLayout(
    sidebarPanel(
      # Modify or add controls here.
      uiOutput(ids$control_panel),
      actionButton(ids$draw_button, draw_copy$label),
      downloadButton(ids$export_button, export_copy$label)
    ),
    mainPanel(
      # Modify or add outputs here.
      plotOutput(ids$plot_output),
      uiOutput(ids$error_output),
      verbatimTextOutput(ids$code_output)
    )
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server(input, output, session, input_formula, ui_text = ui_text, placeholders = placeholders, ids = ids)

  # Add custom observers or outputs below.
  # observe({
  #   runtime_result <- ptr_state$runtime()
  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {
  #     message(runtime_result$code_text)
  #   }
  # })
}

shinyApp(ui, server)
