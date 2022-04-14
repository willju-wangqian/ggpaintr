#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#

server = function(input, output, session) {

    # reactive to fileData
    dataInput <- reactive({
        req(input$fileData)

        path <- input$fileData$datapath

        ext <- tools::file_ext(path) # [TODO] add Imports tools

        validate(
            need(ext %in% c("csv", "rds"), "Please select either csv or rds file")
        )

        if (ext == "csv") {
            inputData <- read.csv(path)
        } else if (ext == "rds") {
            inputData <- readRDS(path)
        }

        inputData
    })

    # reactive to mytable_rows_all:
    #   apply the filters
    dataContainer <- reactive({
        req(input$mytable_rows_all, dataInput())
        dataInput()[input$mytable_rows_all, ]
    })

    # render the table
    output$mytable = DT::renderDataTable({
        req(dataInput())
        datatable(dataInput(), filter = 'top',
                  options = list(
                      pageLength = 10,
                      scrollX = TRUE,
                      stateSave = TRUE
                  ))
    })








    #############################################################
    source("inst/R/box_server.R", local = TRUE)

    #############################################################
    # server part for the bar chart
    source("inst/R/bar_server.R", local = TRUE)

    #############################################################
    # ui for line chart
    observeEvent(input$drawLine, {

        dataLine <- dataContainer() # dataContainer$data

        output$drawControls <- renderUI({
            # provide the complete UI of all components
            column(12,
                   lineControlUI("lineControl", dataLine),
                   actionButton(NS("lineControl")("buttonDraw"), "Draw the plot")
            )
        })
    })

    observeEvent(input[[NS("lineControl")("buttonDraw")]], {
        dataLine <- dataContainer() # dataContainer$data

        lineComponent <- geomLineGenerator("lineControl", dataLine)

        geomComponents <-
            ggplot(data = dataLine) +
            lineComponent()
        if(!is.null(input[[NS("lineControl")("addTextButton")]])) {
            if(input[[NS("lineControl")("addTextButton")]]) {
                textComponent <- geomTextGenerator("lineControl", dataLine)
                geomComponents <- geomComponents + textComponent()
            }
        }

        reactiveList <- reactiveValues(
            pp = reactive(geomComponents)
        )

        # add all plot settings
        reactiveList <- plotSettingServer("lineControl", reactiveList)

        pp <- plotGenerator(reactiveList)

        output$mainPlot <- renderPlot({
            pp
        })
    })

    #############################################################
    # ui for scatter chart
    observeEvent(input$drawScatter, {

        dataScatter <- dataContainer() # dataContainer$data
        dataScatterWider <- dataScatter %>% select(-score_se) %>%
            pivot_wider(names_from = subject, values_from = score_mean)

        output$drawControls <- renderUI({
            # provide the complete UI of all components
            column(12,
                   pointControlUI("pointControl", dataScatterWider),
                   actionButton(NS("pointControl")("buttonDraw"), "Draw the plot")
            )
        })
    })

    observeEvent(input[[NS("pointControl")("buttonDraw")]], {
        dataScatter <- dataContainer() # dataContainer$data
        dataScatterWider <- dataScatter %>% select(-score_se) %>%
            pivot_wider(names_from = subject, values_from = score_mean)

        pointComponent <- geomPointGenerator("pointControl", dataScatterWider)

        geomComponents <-
            ggplot(data = dataScatterWider) +
            pointComponent()

        # if(!is.null(input[[NS("pointControl")("addTextButton")]])) {
        #     if(input[[NS("pointControl")("addTextButton")]]) {
        #         textComponent <- geomTextGenerator("pointControl", dataScatterWider)
        #         geomComponents <- geomComponents + textComponent()
        #     }
        # }

        reactiveList <- reactiveValues(
            pp = reactive(geomComponents)
        )

        # add all plot settings
        reactiveList <- plotSettingServer("pointControl", reactiveList)

        pp <- plotGenerator(reactiveList)

        output$mainPlot <- renderPlot({
            pp
        })
    })

    #############################################################
    # ui for lollipop chart
    observeEvent(input$drawLolli, {
        dataLolli <- dataContainer() # dataContainer$data
        # ui
        output$drawControls <- renderUI({
            column(12,
                   lolliControlUI("lolliControl", dataLolli),
                   actionButton(NS("lolliControl")("buttonDraw"), "Draw the plot")
            )
        })
    })

    # server for lollipop chart
    observeEvent(input[[NS("lolliControl")("buttonDraw")]], {
        dataLolli <- dataContainer() # dataContainer$data

        linerangeComponent <- geomLinerangeGenerator("lolliControl", dataLolli)
        pointComponent <- geomPointGenerator("lolliControl", dataLolli)

        geomComponents <-
            ggplot(data = dataLolli) +
            linerangeComponent() +
            pointComponent()

        if(!is.null(input[[NS("lolliControl")("addTextButton")]])) {
            if(input[[NS("lolliControl")("addTextButton")]]) {
                textComponent <- geomTextGenerator("lolliControl", dataLolli)
                geomComponents <- geomComponents + textComponent()
            }
        }

        reactiveList <- reactiveValues(
            pp = reactive(geomComponents)
        )

        # add all plot settings
        reactiveList <- plotSettingServer("lolliControl", reactiveList)

        pp <- plotGenerator(reactiveList)

        output$mainPlot <- renderPlot({
            pp
        })
    })




}


