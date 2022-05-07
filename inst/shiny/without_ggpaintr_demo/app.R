library(ggpaintr)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(palmerpenguins)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("ggpaintr demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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

    # construct paintr object
    paintr_rctv <- reactive({

        req(data_container())

        paintr(control_id,
               data_container(), data_path = input$defaultData,
               geom_boxplot(aes(x, y, fill)) +
                   coord_flip +
                   labs(x, y, title) +
                   scale_fill
        )

    })

    scale_fill_rctv <- scaleColor_build_reactivity(control_id, paintr_rctv, "fill")


    # place ui
    output$controlPanel <- renderUI({
        req(paintr_rctv())

        column(
            12,
            paintr_get_ui(paintr_rctv(), "x"),
            paintr_get_ui(paintr_rctv(), "y"),
            paintr_get_ui(paintr_rctv(), "fill"),
            paintr_get_ui(paintr_rctv(), "coord_flip"),
            paintr_get_ui(paintr_rctv(), "labs"),
            paintr_get_ui(paintr_rctv(), "scale_fill"),
        )
    })

    # take results and plot
    observe({
        req(paintr_rctv(), data_container(), input$defaultData)
        data <- data_container()

        paintr_results <- paintr_plot_code(paintr_rctv(),
                                           selected_fill_rctv = scale_fill_rctv)

        # Plot output
        output$outputPlot <- renderPlot({

            paintr_results[['plot']]

        })

        # Code output
        output$outputCode <- renderText({

            paintr_results[['code']]

        })

    }) %>% bindEvent(input$draw)


}

# Run the application
shinyApp(ui = ui, server = server)
