
sidebar <- dashboardSidebar(
    width=230,

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
                                choices = c("iris", "mtcars","penguins", "txhousing", "faithfuld",
                                            "diamonds", "economics", "economics_long", "CO2"),
                                selected = "",
                                multiple = TRUE,
                                options = pickerOptions(maxOptions = 1)),
                    DT::DTOutput("mytable"),
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
                                        img(src = "img_button/boxplot.png", width = "100%")
                                    ),
                                    style = "fill"
                                )),
                            column(
                                3, actionBttn(
                                    "drawBar",
                                    label = "bar chart",
                                    icon = div(
                                        img(src = "img_button/barplot.png", width = "100%")
                                    ),
                                    style = "fill"
                                )),
                            column(
                              3, actionBttn(
                                "drawScatter",
                                label = "scatter plot",
                                icon = div(
                                  img(src = "img_button/scatter.png", width = "100%")
                                ),
                                style = "fill"
                              )),
                            column(
                              3, actionBttn(
                                "drawLine",
                                label = "line plot",
                                icon = div(
                                  img(src = "img_button/line.png", width = "100%")
                                ),
                                style = "fill"
                              ))
                        )

                    ),
                    br(),
                    fluidRow(
                        column(
                            4,
                            actionButton("buttonDraw", "Draw the plot"),
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

