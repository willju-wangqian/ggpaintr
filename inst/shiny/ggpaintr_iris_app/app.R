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
library(tidyverse)
library(assertthat)

data <- iris

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("someUI"),
            actionButton("drawBox", "call UI"),
            actionButton("draw", "label: draw")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("mainPlot")
        )
    )
)


box_control_id <- "boxControl"

ns_box <- NS(box_control_id)

# Define server logic required to draw a histogram
server <- function(input, output) {

    code_container <- reactiveValues()
    code_container[['data']] <- "abc"

    box_main <- reactive({
        req(data)

        boxUI <-
            controlUI(box_control_id, data,
                      mapping = c('x', 'y', 'color', 'size')
            )

        boxUI

    }) %>% bindCache(input$drawBox) %>% bindEvent(input$drawBox)

    observe({
        output$someUI <- renderUI({
            req(box_main())

            column(
                12,
                box_main()[['ui']][['mapping_ui']][['x']],
                box_main()[['ui']][['mapping_ui']][['y']],
                box_main()[['ui']][['mapping_ui']][['color']],
                box_main()[['ui']][['mapping_ui']][['size']]
            )
        })

    }) %>% bindEvent(input$drawBox)


    observe({
        boxComponent <- ggGeomGenerator(id = box_control_id,
                                        data = data,
                                        geom_FUN = "geom_point",
                                        id_list = box_main()[['ids']],
                                        params_list = list(
                                            mapping = c('x', 'y', 'color', 'size')
                                        )
        )

        results <- get_plot_code(boxComponent,
                                 data = data,
                                 data_path = code_container[['data']])

        output$mainPlot <- renderPlot({

            validate(need(results[['plot']], "plot is not rendered"))

            results[['plot']]
        })

    }) %>% bindEvent(input$draw)


}

# Run the application
shinyApp(ui = ui, server = server)
