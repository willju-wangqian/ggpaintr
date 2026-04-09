library(ggpaintr)
library(shiny)
library(shinyWidgets)

input_formula <- "
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point() +
  labs(title = text) +
  facet_wrap(expr)"

# Edit custom_copy_rules to customize UI labels, help text, and placeholders.
custom_copy_rules <- list(shell = list(
  title = list(label = "Exploratory Plot Builder"),
  draw_button = list(label = "Render plot")
), params = list(
  x = list(var = list(label = "Pick the field for the x-axis")),
  y = list(var = list(label = "Pick the field for the y-axis")),
  title = list(text = list(label = "Chart title"))
), layers = list(
  facet_wrap = list(expr = list(`__unnamed__` = list(label = "Split the plot by")))
))

copy_rules <- ggpaintr_effective_copy_rules(custom_copy_rules)

title_copy <- ggpaintr_resolve_copy("title", copy_rules = copy_rules)
draw_copy <- ggpaintr_resolve_copy("draw_button", copy_rules = copy_rules)
export_copy <- ggpaintr_resolve_copy("export_button", copy_rules = copy_rules)

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
  ggpaintr_state <- ggpaintr_server(input, output, session, input_formula, copy_rules = copy_rules)

  # Add custom observers or outputs below.
  # observe({
  #   runtime_result <- ggpaintr_state$runtime()
  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {
  #     message(runtime_result$code_text)
  #   }
  # })
}

shinyApp(ui, server)
