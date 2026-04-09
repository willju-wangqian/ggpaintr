library(ggpaintr)
library(shiny)
library(shinyWidgets)

input_formula <- "
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point() +
  labs(title = text) +
  facet_wrap(expr)"

# Replace NULL with a named list to customize UI labels, help text, and placeholders.
# copy_rules <- list(
#   shell = list(
#     title = list(label = "Exploratory Plot Builder"),
#     draw_button = list(label = "Render plot"), export_button = list(
#       label = "Export Shiny app"
#     )
#   ), upload = list(file = list(
#     label = "Choose a data file"
#   ), name = list(
#     label = "Optional dataset name",
#     placeholder = "For example: sales_data", help = "Accepted formats: .csv and .rds. Leave the name blank to use the file name in generated code."
#   )),
#   layer_checkbox = list(label = "Include this layer in the plot"),
#   defaults = list(
#     var = list(
#       label = "Choose a column for {param}",
#       empty_text = "Choose one column"
#     ), text = list(label = "Enter text for {param}"),
#     num = list(label = "Enter a number for {param}"), expr = list(
#       label = "Enter an expression for {param}"
#     )
#   ), params = list(
#     x = list(
#       var = list(label = "Pick the field for the x-axis"),
#       text = list(label = "X-axis label")
#     ), y = list(var = list(
#       label = "Pick the field for the y-axis"
#     ), text = list(
#       label = "Y-axis label"
#     )), color = list(var = list(
#       label = "Choose the color column"
#     )), fill = list(
#       var = list(label = "Choose the fill column")
#     ), group = list(
#       var = list(label = "Choose the grouping column")
#     ),
#     shape = list(var = list(label = "Choose the shape column")),
#     size = list(
#       var = list(label = "Choose the size column"),
#       num = list(label = "Point size", help = "Enter a number such as 2 or 3.")
#     ),
#     alpha = list(
#       var = list(label = "Choose the transparency column"),
#       num = list(label = "Transparency", help = "Enter a value between 0 and 1.")
#     ),
#     label = list(var = list(label = "Choose the label column")),
#     title = list(text = list(label = "Chart title")), subtitle = list(
#       text = list(label = "Plot subtitle")
#     ), caption = list(
#       text = list(label = "Plot caption")
#     ), legend.position = list(
#       text = list(label = "Legend position", help = "Examples: right, left, top, bottom, none.")
#     ),
#     labeller = list(expr = list(
#       label = "Facet label function",
#       help = "Enter an R helper such as label_both."
#     )),
#     ncol = list(num = list(label = "Number of facet columns")),
#     nrow = list(num = list(label = "Number of facet rows")),
#     linewidth = list(num = list(label = "Line width")), stroke = list(
#       num = list(label = "Stroke width")
#     ), bins = list(
#       num = list(label = "Number of bins")
#     ), binwidth = list(
#       num = list(label = "Bin width")
#     )
#   ), layers = list(
#     facet_wrap = list(expr = list(`__unnamed__` = list(
#       label = "Split the plot by",
#       placeholder = "~ Species", help = "Enter a faceting formula such as ~ Species."
#     ))),
#     facet_grid = list(expr = list(`__unnamed__` = list(
#       label = "Facet layout",
#       placeholder = "Species ~ .", help = "Enter a faceting formula such as Species ~ . or . ~ Species."
#     )))
#   )
# )

custom_copy_rules <- list(
  shell = list(
    title = list(label = "Exploratory Plot Builder"),
    draw_button = list(label = "Render plot")
  ),
  params = list(
    x = list(var = list(label = "Pick the field for the x-axis")),
    y = list(var = list(label = "Pick the field for the y-axis")),
    title = list(text = list(label = "Chart title"))
  ),
  layers = list(
    facet_wrap = list(
      expr = list(
        `__unnamed__` = list(
          label = "Split the plot by",
          placeholder = "~ Species"
        )
      )
    )
  )
)


copy_rules <- ggpaintr:::ggpaintr_effective_copy_rules(custom_copy_rules)

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
