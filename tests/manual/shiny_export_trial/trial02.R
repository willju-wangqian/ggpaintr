library(shiny)
library(shinyWidgets)


ui <- fluidPage(

titlePanel("ggpaintr demo"),

  sidebarLayout(
    sidebarPanel(
      tabsetPanel(tabPanel("ggplot", pickerInput("ggplot+3+2", paste0("x",
                                                                      ": "), choices = c("manufacturer", "model", "displ", "year",
                                                                                         "cyl", "trans", "drv", "cty", "hwy", "fl", "class"), selected = "",
                                                 multiple = TRUE, options = pickerOptions(maxOptions = 1)),
                           pickerInput("ggplot+3+3", paste0("y", ": "), choices = c("manufacturer",
                                                                                    "model", "displ", "year", "cyl", "trans", "drv", "cty", "hwy",
                                                                                    "fl", "class"), selected = "", multiple = TRUE, options = pickerOptions(maxOptions = 1))),
                  tabPanel("geom_point"), tabPanel("labs", textInput("labs+2",
                                                                     paste0("text for ", "x", ":")), textInput("labs+3", paste0("text for ",
                                                                                                                                "y", ":")))),
      actionButton("draw", "click to draw the plot"),
    ),

    mainPanel(
      plotOutput("outputPlot")
    )
  )
)

server <- function(input, output) {


  observe({

    output$outputPlot <- renderPlot({
      ggplot(data = mpg, aes(x = .data[[input[["ggplot+3+2"]]]], y = .data[[input[["ggplot+3+3"]]]])) +
        geom_point() +
        labs(x = input[["labs+2"]], y = input[["labs+3"]])
    })

  }) %>% bindEvent(input$draw)
}

shinyApp(ui, server)
