server <- function(input, output) {
}
ui <- fluidPage(
  box(Width = 12,
      div(style="display: inline-block;vertical-align:moddle; width: 175px;",
          p("Choose a threshold that is ")),
      div(style="display: inline-block;vertical-align:middle; width: 100px;",
          selectInput("var2","",c("inches","cm"))),
      div(style="display: inline-block;vertical-align:moddle; width: 175px;",
          p("Choose a threshold that is ")),
      div(style="display: inline-block;vertical-align:middle; width: 100px;",
          selectInput("var2","",c("inches","cm"))),
      div(style="display: inline-block;vertical-align:moddle; width: 175px;",
          p("Choose a threshold that is "))
  )
)
shinyApp(ui, server)
