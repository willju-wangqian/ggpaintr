library(ggpaintr)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(palmerpenguins)
library(rlang)



paintr_get_ui_all <- function(paintr_obj, type = "ui", verbose = FALSE) {

  stopifnot(class(paintr_obj) == "paintr_obj")

  type <- match.arg(type, c("ui", "id"))

  ui_list_all <- unname(unlist(
    paintr_obj[['shiny_components']][[type]], recursive = FALSE
  ))

  return(ui_list_all)
}

# uis <- paintr_get_ui_all(paintr_obj)
# do.call(column, c(12, uis))



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textAreaInput("formula", label = "Enter the paintr formula", value = "geom_point(aes(x, y, color))"),
      actionButton("enter", "click to enter the formula"),
      pickerInput("defaultData", "select a default dataset:",
                  choices = c("iris", "mtcars","penguins", "faithfuld"),
                  selected = "",
                  multiple = TRUE,
                  options = pickerOptions(maxOptions = 1)),
      uiOutput("controlPanel"),
      actionButton("draw", "click to draw the plot"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outputPlot"),
      verbatimTextOutput('outputCode')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  control_id <- "control_id"

  # data
  data_container <- reactive({
    req(input$defaultData)
    get(input$defaultData)
  })

  paintr_rctv <- eventReactive({
    input$enter
    data_container()
  }, {
    req(data_container())

    paintr_expr <- parse_expr(input$formula)

    paintr(control_id,
           data_container(), data_path = input$defaultData,
           !!paintr_expr
    )
  })


  # place ui
  output$controlPanel <- renderUI({
    req(paintr_rctv())

    do.call(column,
            c(12, paintr_get_ui_all(paintr_rctv())))
  })

  # take results and plot
  observe({
    req(paintr_rctv(), data_container(), input$defaultData)
    data <- data_container()

    paintr_results <- paintr_plot_code(paintr_rctv())

    # Plot output
    output$outputPlot <- renderPlot({

      paintr_results[['plot']]

    })

    # Code output
    output$outputCode <- renderText({

      paintr_results[['code']]

    })

    browser()

  }) %>% bindEvent(input$draw)


}

# Run the application
shinyApp(ui = ui, server = server)







