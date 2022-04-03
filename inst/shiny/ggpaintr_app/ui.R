#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

sidebar <- dashboardSidebar(
    sidebarMenu(
        menuItem("Design", tabName = "menuDesign"),
        menuItem("Draw", tabName = "menuDraw")
    )
)

body <- dashboardBody(
    tabItems(
        tabItem(
            tabName = "menuDesign",
            fileInput(inputId = "fileData",
                      label = "Upload data in csv or rds format",
                      accept = c(".csv")),
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
    dashboardHeader(title = "Simple tabs"),
    sidebar,
    body
)

