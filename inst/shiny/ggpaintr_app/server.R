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
        req(input$mytable_rows_all)
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
    # server part for the bar chart
    box_main <- reactive({
        req(dataContainer())

        dataBox <- dataContainer()
        boxUI <-
            controlUI("boxControl", dataBox,
                      mapping = list( x = "mapX",
                                      y = "mapY",
                                      color = "mapColor"),
                      plot_settings = list( misc = "otherMisc" ,
                                            theme = "otherTheme"),
                      geom_args = list(stat = "argsStat",
                                       position = "argsPosition"),
                      extra_uiFunc = list(misc = miscUI,
                                          theme = themeUI,
                                          stat = myStatUI),
                      extra_uiFuncArgs = list(misc = list(NS("boxControl"), dataBox),
                                              theme = list(NS("boxControl")),
                                              stat = list(2, 5) ))

        boxUI

    }) %>% bindEvent(input$drawBox)

    output$drawControls <- renderUI({
        req(box_main())

        column(
            12,
            bsCollapse(
                id = NS("boxControl")("boxControlCollapse"), open = "mapping",
                multiple = FALSE,
                bsCollapsePanel(
                    "mapping",
                    column(
                        12, offset = 0, style='padding:0px;',
                        br(),
                        box_main()[['ui']][['mapping_ui']][['x']],
                        box_main()[['ui']][['mapping_ui']][['y']],
                        box_main()[['ui']][['mapping_ui']][['color']]
                    )
                ),
                bsCollapsePanel(
                    "advanced settings",
                    br(),
                    h3("misc"),
                    box_main()[['ui']][['plot_settings_ui']][['misc']],
                    br(),
                    h3("theme settings"),
                    box_main()[['ui']][['plot_settings_ui']][['theme']]
                )
            ),
            actionButton(NS("boxControl")("buttonDraw"), "Draw the plot"),
            box_main()[['ui']][['geom_args_ui']][['stat']]
        )
    }) %>% bindEvent(input$drawBox)

    observe({
        req(dataContainer(), box_main())

        str(box_main()[['param']][['mapping']])
        cat('\n')
    }) %>% bindEvent(input[[NS("boxControl")("buttonDraw")]])

    # observeEvent(input[[NS("boxControl")("buttonDraw")]], {
    #     dataBox <- dataContainer() #dataContainer$data
    #
    #     cat(box_UI()$mapping)
    #     # barComponent <- geomBarGenerator("barControl", dataBar)
    #     #
    #     # geomComponents <-
    #     #     ggplot(data = dataBar) +
    #     #     barComponent()
    #     # if(!is.null(input$`barControl-addTextButton`)) {
    #     #     if(input$`barControl-addTextButton`) {
    #     #         textComponent <- geomTextGenerator("barControl", dataBar)
    #     #         geomComponents <- geomComponents + textComponent()
    #     #     }
    #     # }
    #     #
    #     # reactiveList <- reactiveValues(
    #     #     pp = reactive(geomComponents)
    #     # )
    #     #
    #     # # add all plot settings
    #     # reactiveList <- plotSettingServer("barControl", reactiveList)
    #     #
    #     # pp <- plotGenerator(reactiveList)
    #     #
    #     # output$mainPlot <- renderPlot({
    #     #     pp
    #     # })
    # })

    #############################################################
    # server part for the bar chart
    observeEvent(input$drawBar, {
        # browser()

        dataBar <- dataContainer() # dataContainer$data

        output$drawControls <- renderUI({
            # provide the complete UI of all components
            column(12,
                barControlUI("barControl", dataBar),
                actionButton(NS("barControl")("buttonDraw"), "Draw the plot")
            )
        })
    })

    observeEvent(input[[NS("barControl")("buttonDraw")]], {
        dataBar <- dataContainer() #dataContainer$data

        barComponent <- geomBarGenerator("barControl", dataBar)

        geomComponents <-
            ggplot(data = dataBar) +
            barComponent()
        if(!is.null(input$`barControl-addTextButton`)) {
            if(input$`barControl-addTextButton`) {
                textComponent <- geomTextGenerator("barControl", dataBar)
                geomComponents <- geomComponents + textComponent()
            }
        }

        reactiveList <- reactiveValues(
            pp = reactive(geomComponents)
        )

        # add all plot settings
        reactiveList <- plotSettingServer("barControl", reactiveList)

        pp <- plotGenerator(reactiveList)

        output$mainPlot <- renderPlot({
            pp
        })
    })

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


