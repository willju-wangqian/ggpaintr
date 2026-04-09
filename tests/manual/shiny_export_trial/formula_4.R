library(ggpaintr)
library(shiny)
library(shinyWidgets)

input_formula <- "
ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +
  geom_point(aes(color = var), size = 2) +
  geom_point(data = upload, aes(x = var, y = var, color = var), size = num) +
  labs(title = text)
"

ui <- fluidPage(
  titlePanel("ggpaintr demo"),
  sidebarLayout(
    sidebarPanel(
      # Modify or add controls here.
      uiOutput("controlPanel"),
      actionButton("draw", "click to draw the plot"),
      downloadButton("shinyExport", "export the shiny app")
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
  ptr_state <- ptr_server(input, output, session, input_formula)

  # Add custom observers or outputs below.
  # observe({
  #   runtime_result <- ptr_state$runtime()
  #   if (!is.null(runtime_result) && isTRUE(runtime_result$ok)) {
  #     message(runtime_result$code_text)
  #   }
  # })
}

shinyApp(ui, server)
