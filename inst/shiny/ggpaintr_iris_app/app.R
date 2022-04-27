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
library(rlang)
library(shinyWidgets)

# sapply(list.files("R_funcs/"), function(fileName) {
#     source(paste0("R_funcs/", fileName))
# })

data <- iris

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            uiOutput("someUI"),
            # actionButton("drawBox", "call UI"),
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

    code_container <- reactiveValues()
    code_container[['data']] <- "iris"

    box_main <- reactive({

        paintr_expr <- expr(
            geom_boxplot(aes(x, y, fill), position) +
                coord_flip +
                facet_grid +
                theme(legend.position) +
                labs(x, y) +
                theme_choose
        )

        paintr(
            box_control_id,
            names(data),
            !!paintr_expr
        )

    })

    output$someUI <- renderUI({
        req(box_main())

        column(
            12,
            paintr_get_ui(box_main(), "x"),
            paintr_get_ui(box_main(), "y"),
            paintr_get_ui(box_main(), "fill"),
            paintr_get_ui(box_main(), "coord_flip"),
            paintr_get_ui(box_main(), "facet_grid")
        )
    })


    observe({
        req(box_main())

        paintr_list <- paintr_plot_code(box_main(),
                                        box_control_id, data)

        results <- get_plot_code(paintr_list,
                                 data = data,
                                 data_path = code_container[['data']])

        output$mainPlot <- renderPlot({

            validate(need(results[['plot']], "plot is not rendered"))

            results[['plot']]
        })

        output$mycode <- renderText({

            results[['code']]

        })

    }) %>% bindEvent(input$draw)


}

# Run the application
shinyApp(ui = ui, server = server)
