library(shiny)
library(shinyWidgets)

# Please load your data first

ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tabsetPanel(
        tabPanel("ggplot", pickerInput("ggplot+3+2", paste0(
          "x",
          ": "
        ),
        choices = c(
          "carat", "cut", "color", "clarity", "depth",
          "table", "price", "x", "y", "z"
        ), selected = "", multiple = TRUE,
        options = pickerOptions(maxOptions = 1)
        )), tabPanel(
          "geom_smooth",
          checkboxInput("geom_smooth+checkbox", label = paste(
            "Keep the layer of",
            "geom_smooth"
          ), value = TRUE)
        ), tabPanel(
          "facet_wrap",
          checkboxInput("facet_wrap+checkbox", label = paste(
            "Keep the layer of",
            "facet_wrap"
          ), value = TRUE), pickerInput("facet_wrap+2+2",
            paste0("facet_wrap argument 1", ": "),
            choices = c(
              "carat",
              "cut", "color", "clarity", "depth", "table", "price",
              "x", "y", "z"
            ), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1)
          ),
          numericInput(
            "facet_wrap+3", paste0("num for ", "ncol", ":"),
            NA
          )
        ), tabPanel("geom_hline-1", checkboxInput("geom_hline-1+checkbox",
          label = paste("Keep the layer of", "geom_hline-1"), value = TRUE
        )),
        tabPanel("geom_hline-2", checkboxInput("geom_hline-2+checkbox",
          label = paste("Keep the layer of", "geom_hline-2"), value = TRUE
        )),
        tabPanel("geom_hline-3", checkboxInput("geom_hline-3+checkbox",
          label = paste("Keep the layer of", "geom_hline-3"), value = TRUE
        )),
        tabPanel("scale_y_continuous", checkboxInput("scale_y_continuous+checkbox",
          label = paste("Keep the layer of", "scale_y_continuous"),
          value = TRUE
        )), tabPanel(
          "labs", checkboxInput("labs+checkbox",
            label = paste("Keep the layer of", "labs"), value = TRUE
          ),
          textInput("labs+2", paste0("text for ", "title", ":")),
          textInput("labs+3", paste0("text for ", "subtitle", ":")),
          textInput("labs+4", paste0("text for ", "x", ":")), textInput(
            "labs+5",
            paste0("text for ", "y", ":")
          )
        ), tabPanel(
          "theme_minimal",
          checkboxInput("theme_minimal+checkbox", label = paste(
            "Keep the layer of",
            "theme_minimal"
          ), value = TRUE)
        )
      ),
      actionButton("draw", "click to draw the plot"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outputPlot")
    )
  )
)

server <- function(input, output) {
  observe({
    p_expr <- parse_expr('expr(ggplot(data = diamonds, aes(x = .data[[input[["ggplot+3+2"]]]],
    y = price)) +
  geom_smooth(method = "loess", se = FALSE, linewidth = 0.8) +
  facet_wrap(~.data[[input[["facet_wrap+2+2"]]]], ncol = input[["facet_wrap+3"]]) +
  geom_hline(data = qdat, aes(yintercept = q50), linetype = 2) +
  geom_hline(data = qdat, aes(yintercept = q90), linetype = 3) +
  geom_hline(data = qdat, aes(yintercept = 50), linetype = 4) +
  scale_y_continuous(labels = scales::dollar) +
  labs(title = input[["labs+2"]], subtitle = input[["labs+3"]],
    x = input[["labs+4"]], y = input[["labs+5"]]) +
  theme_minimal(base_size = 12))')

    p_expr_active <- expr_remove_empty_input(p_expr, input)

    output$outputPlot <- renderPlot({
      ptr_assemble_plot(expr_plot_eval(p_expr_active))
    })
  }) %>% bindEvent(input$draw)
}

shinyApp(ui, server)
