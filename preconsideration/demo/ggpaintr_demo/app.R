#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggpaintr)
library(shinyWidgets)
library(tidyverse)

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
            uiOutput("someUI"),
            actionButton("draw", "label: draw")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mainPlot"),
            verbatimTextOutput('mycode')
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    box_control_id <- "boxControl"

    ns_box <- NS(box_control_id)

    # data
    data_container <- reactive({
        req(input$defaultData)
        get(input$defaultData)
    })

    # construct paintr object
    box_main <- reactive({

        req(data_container())
        data <- data_container()

        paintr(
            id = box_control_id,
            data = data,
            data_path = input$defaultData,
            # expr = geom_point(aes(x, y, color), alpha) +
            #     theme_choose +
            #     labs(x, y, title)
            expr = geom_contour(aes(x,y,z))
        )

    })

    # place ui
    output$someUI <- renderUI({
        req(box_main())

        column(
            12,
            paintr_get_ui(box_main(), "x"),
            paintr_get_ui(box_main(), "y"),
            paintr_get_ui(box_main(), "z"),
            # paintr_get_ui(box_main(), "color"),
            # paintr_get_ui(box_main(), "fill"),
            # paintr_get_ui(box_main(), "alpha"),
            # paintr_get_ui(box_main(), "labs"),
            # paintr_get_ui(box_main(), "theme_choose"),
            # paintr_get_ui(box_main(), "theme"),
            # paintr_get_ui(box_main(), "position"),
            # paintr_get_ui(box_main(), "facet_grid")
            # ...
        )
    })

    # take results and plot
    observe({
        req(box_main(), data_container(), input$defaultData)
        data <- data_container()

        paintr_list <- paintr_plot_code(box_main())

        output$mainPlot <- renderPlot({

            validate(need(paintr_list[['plot']], "plot is not rendered"))

            paintr_list[['plot']]
        })

        output$mycode <- renderText({

            paintr_list[['code']]

        })

    }) %>% bindEvent(input$draw)

}

# Run the application
shinyApp(ui = ui, server = server)
