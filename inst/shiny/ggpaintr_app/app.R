library(shiny)
library(bslib)
require(ggplot2)
require(dplyr)


# Define UI for application that draws a histogram
ui <- fluidPage(

  theme = bs_theme(version = 4, bootswatch = "minty"),

  headerPanel("Hello ggpaintr!"),

  # Sidebar with a slider input for the number of bins

  sidebarPanel(sidebarMenu(
      menuItem("Design", tabName = "menuDesign"),
      menuItem("Draw", tabName = "menuDraw")
    ),

  body <- dashboardBody(
    tabItems(
      tabItem(
        tabName = "menuDesign",
        fileInput(inputId = "fileData",
                  label = "Upload data in csv or rds format",
                  accept = c(".csv", ".rds")),
        DT::dataTableOutput("mytable"),
      ),

      tabItem(
        tabName = "menuDraw",
        p("Plot type"),
        br(),
        fluidRow(
          box(column(3,
                     actionButton(
                       "drawBar",
                       "Bar plot"
                     )),
              column(3,
                     actionButton(
                       "drawLine",
                       "Line plot"
                     )),
              column(3,
                     actionButton(
                       "drawScatter",
                       "Scatter plot"
                     )),
              column(3,
                     actionButton(
                       "drawLolli",
                       "Lollipop plot"
                     ))
          )
        ),

        br(),

        fluidRow(
          column(
            4,
            uiOutput("drawControls")
          ),
          column(
            8,
            plotOutput("mainPlot")
          )
        )
      )
    )
  )

))

# Define server logic required to draw a histogram
server <- function(input, output, session) {


  dataContainer <- reactive({
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

  output$mytable <- DT::renderDT(
    dataContainer(),
    filter = "top",
    options = list(
      pageLength = 5
    )
  )

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

# Bind ui and server together
shinyApp(ui, server)
