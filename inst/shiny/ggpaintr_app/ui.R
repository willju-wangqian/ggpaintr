#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



sidebar <- dashboardSidebar(
  width=300,

  sidebarMenu(
    HTML(paste0(
      "<br>",
      "<a href='https://github.com/willju-wangqian/ggpaintr' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='img/ggpaintr.png' width = '186'></a>",
      "<br>"
    )),
    menuItem("Home", tabName = "home", icon = icon("home")),
    menuItem("Paintr", tabName = "menuPaint", icon = icon("image"))
    # menuItem("Design", tabName = "menuDesign",icon = icon("table")),
    # menuItem("Draw", tabName = "menuDraw",icon = icon("stats", lib = "glyphicon")),
    # menuItem("Code", tabName = "code", icon = icon("code"))
  )
)



body <- dashboardBody(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),

  tabItems(

    tabItem(
      tabName = "home",
      includeMarkdown("www/home.md"),
      img(src = "img/image.png", height = 500, width = 1000)
    ),

    tabItem(
      tabName = "menuPaint",

      tabsetPanel(

        tabPanel(
          "Data", icon = icon("table"),
          fileInput(inputId = "fileData",
                    label = "Upload data in csv or rds format",
                    accept = c(".csv", ".rds")),
          DT::dataTableOutput("mytable"),
        ),

        tabPanel(
          "Paint", icon =  icon("palette"),
          fluidRow(
            box(column(
              3, actionBttn(
                "drawBox",
                label = "boxplot",
                icon = div(
                  img(src = "img_button/boxplot.png")
                  # p("box plot")
                ),
                # "style"="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                style = "fill"
              ))
            )
            # box(column(3,
            #            actionButton(
            #              "drawBox",
            #              label = div(
            #                img(src = "img_button/boxplot.png"),
            #                p("box plot")
            #              ),
            #              style = "color: #fff; background-color: #337ab7; border-color: #2e6da4",
            #            )
            # ) )
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
          )),

        tabPanel(
          "Code", icon =  icon("code"),
        )
      )
    )

    # tabItem(
    #   tabName = "menuDesign",
    # ),

    # tabItem(
    #
    #   tabName = "menuDraw",
    #   strong("Plot type"),
    #   br(),
    #   fluidRow(
    #     box(column(3,
    #                actionButton(
    #                  "drawBar",
    #                  "Bar plot"
    #                )),
    #         column(3,
    #                actionButton(
    #                  "drawLine",
    #                  "Line plot"
    #                )),
    #         column(3,
    #                actionButton(
    #                  "drawScatter",
    #                  "Scatter plot"
    #                )),
    #         column(3,
    #                actionButton(
    #                  "drawLolli",
    #                  "Lollipop plot"
    #                ))
    #     )
    #   ),
    # )
  )
)

# Put them together into a dashboardPage


dashboardPage(
  skin="purple",
  dashboardHeader(title = "Welcome to ggpaintr!"),
  sidebar,
  body
)

