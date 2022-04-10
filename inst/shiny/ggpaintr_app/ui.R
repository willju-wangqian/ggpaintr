#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



sidebar <- dashboardSidebar(width=300,

    sidebarMenu(
      HTML(paste0(
        "<br>",
        "<a href='https://github.com/willju-wangqian/ggpaintr' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='ggpaintr.png' width = '186'></a>",
        "<br>"
      )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Design", tabName = "menuDesign",icon = icon("table")),
        menuItem("Draw", tabName = "menuDraw",icon = icon("stats", lib = "glyphicon")),
        menuItem("Code", tabName = "code", icon = icon("code"))
    )
)



body <- dashboardBody(
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.min.css")),

    tabItems(


        tabItem(
              tabName = "home",
              includeMarkdown("www/home.md")
        ),

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

# Put them together into a dashboardPage


dashboardPage(
    skin="purple",
    dashboardHeader(title = "Welcome to ggpaintr!"),
    sidebar,
    body
)

