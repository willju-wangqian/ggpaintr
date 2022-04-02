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

    data_path <- system.file("extdata", "kindergartners_data_firsttofifth.csv", package = "ggpaintr")

    data <- read_csv(data_path)
    data$grade <- factor(
        data$grade,
        levels = c("first_grade", "second_grade", "third_grade", "fourth_grade", "fifth_grade")
    )
    names(data)[4] <- "category_levels"
    names(data)[5] <- "school_year"
    names(data)[6] <- "score_mean"
    names(data)[7] <- "score_se"
    data <- data[,-1]

    dataContainer <- reactiveValues(data = data)

    output$mytable = DT::renderDataTable({
        dataContainer$data
        # data
    })

    output$mytableFilter <- renderUI({
        fluidRow(
            column(4,
                   pickerInput("mytableFilterInputSubject", "choose a subject for investigation:",
                               choices = c(unique(data$subject), "no filter"),
                               selected = "no filter",
                               multiple = TRUE,
                               options = pickerOptions(maxOptions = 1))
            ),
            column(4,
                   pickerInput("mytableFilterInputCategory", "choose a category for investigation:",
                               choices = c(unique(data$category), "no filter"),
                               selected = "no filter",
                               multiple = TRUE,
                               options = pickerOptions(maxOptions = 1))
            ),
            column(4,
                   pickerInput("mytableFilterInputGrade", "choose a grade for investigation:",
                               choices = c(unique(as.character(data$school_year)), "no filter"),
                               selected = "no filter",
                               multiple = TRUE,
                               options = pickerOptions(maxOptions = 1))
            )
        )

    })

    observe({
        if(
            input$mytableFilterInputCategory == "no filter" ||
            is.null(input$mytableFilterInputCategory) ||
            input$mytableFilterInputCategory == ""
        ) {
            c_pattern <- ""
        } else {
            c_pattern <- input$mytableFilterInputCategory
        }

        if(
            input$mytableFilterInputSubject == "no filter" ||
            is.null(input$mytableFilterInputSubject) ||
            input$mytableFilterInputSubject == ""
        ) {
            s_pattern <- ""
        } else {
            s_pattern <- input$mytableFilterInputSubject
        }

        if(
            input$mytableFilterInputGrade == "no filter" ||
            is.null(input$mytableFilterInputGrade) ||
            input$mytableFilterInputGrade == ""
        ) {
            g_pattern <- ""
        } else {
            g_pattern <- input$mytableFilterInputGrade
        }

        dataContainer$data <- data %>%
            filter(str_detect(school_year, g_pattern)) %>%
            filter(str_detect(subject, s_pattern)) %>%
            filter(str_detect(category, c_pattern))


    })

    #############################################################
    # server part for the bar chart
    observeEvent(input$drawBar, {
        # browser()

        dataBar <- dataContainer$data

        output$drawControls <- renderUI({
            # provide the complete UI of all components
            column(12,
                barControlUI("barControl", dataBar),
                actionButton(NS("barControl")("buttonDraw"), "Draw the plot")
            )
        })
    })

    observeEvent(input[[NS("barControl")("buttonDraw")]], {
        dataBar <- dataContainer$data

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

        dataLine <- dataContainer$data

        output$drawControls <- renderUI({
            # provide the complete UI of all components
            column(12,
                   lineControlUI("lineControl", dataLine),
                   actionButton(NS("lineControl")("buttonDraw"), "Draw the plot")
            )
        })
    })

    observeEvent(input[[NS("lineControl")("buttonDraw")]], {
        dataLine <- dataContainer$data

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

        dataScatter <- dataContainer$data
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
        dataScatter <- dataContainer$data
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
        dataLolli <- dataContainer$data
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
        dataLolli <- dataContainer$data

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


