library(ggpaintr)
library(shiny)
library(shinyWidgets)

input_formula <- "
ggplot(data = upload, aes(x = var, y = var)) +
  geom_point(size = num, alpha = num) +
  labs(title = text) +
  facet_grid(expr)"

# Replace NULL with a named list to customize UI labels, help text, and placeholders.
copy_rules <- NULL

title_copy <- paintr_resolve_copy("title", copy_rules = copy_rules)
draw_copy <- paintr_resolve_copy("draw_button", copy_rules = copy_rules)
export_copy <- paintr_resolve_copy("export_button", copy_rules = copy_rules)

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
  paintr_state <- ggpaintr_server(input, output, session, input_formula, copy_rules = copy_rules)

  # Add custom observers or outputs below.
  # observe({
  #   runtime_result <- paintr_state$runtime()
  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {
  #     message(runtime_result$code_text)
  #   }
  # })
}

shinyApp(ui, server)
