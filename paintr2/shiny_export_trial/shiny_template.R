library(shiny)
library(shinyWidgets)

# Please load your data first

ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      $text_ui$,
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

    p <- $text_server$

    output$outputPlot <- renderPlot({
      p
    })

  }) %>% bindEvent(input$draw)
}

shinyApp(ui, server)
