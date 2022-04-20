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
            "<a href='https://github.com/willju-wangqian/ggpaintr' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='img/ggpaintr.png' width = '170'></a>",
            "<br>"
        )),
        menuItem("Home", tabName = "home", icon = icon("home")),
        menuItem("Paintr", tabName = "menuPaint", icon = icon("image"))
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
                    pickerInput("defaultData", "select a default dataset:",
                                choices = c("iris", "mtcars"),
                                selected = "",
                                multiple = TRUE,
                                options = pickerOptions(maxOptions = 1)),
                    DT::dataTableOutput("mytable"),
                ),

                tabPanel(
                    "Paint", icon =  icon("palette"),
                    fluidRow(
                        box(width = 12,
                            column(
                                3, actionBttn(
                                    "drawBox",
                                    label = "boxplot",
                                    icon = div(
                                        img(src = "img_button/boxplot.png")
                                        # p("box plot")
                                    ),
                                    # "style"="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                    style = "fill"
                                )),
                            column(
                                3, actionBttn(
                                    "drawBar",
                                    label = "bar chart",
                                    icon = div(
                                        img(src = "img_button/boxplot.png")
                                        # p("box plot")
                                    ),
                                    # "style"="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                    style = "fill"
                                )),
                            column(
                              3, actionBttn(
                                "drawScatter",
                                label = "scatter plot",
                                icon = div(
                                  img(src = "img_button/scatter.png")
                                  # p("scatter plot")
                                ),
                                # "style"="color: #fff; background-color: #337ab7; border-color: #2e6da4",
                                style = "fill"
                              )),
                            column(
                              3, actionBttn(
                                "drawLine",
                                label = "line plot",
                                icon = div(
                                  img(src = "img_button/line.png")
                                  # p("scatter plot")
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
                    box(width = 12, verbatimTextOutput('mycode'))
                )
            )
        )

    )
)

# Put them together into a dashboardPage


ui <- dashboardPage(
    skin="green",
    dashboardHeader(title = "Welcome to ggpaintr!"),
    sidebar,
    body
)

