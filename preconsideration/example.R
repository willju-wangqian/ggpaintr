library(shiny)
library(shinyBS)

shinyApp(
  ui =
    fluidPage(
      sidebarLayout(
        sidebarPanel(HTML("This button will open Panel 1 using updateCollapse."),
                     actionButton("p1Button", "Push Me!"),
                     selectInput("styleSelect", "Select style for Panel 1",
                                 c("default", "primary", "danger", "warning", "info", "success"))
        ),
        mainPanel(
          bsCollapse(id = "collapseExample", open = "Panel 2",
                     bsCollapsePanel("Panel 1", "This is a panel with just text ",
                                     "and has the default style. You can change the style in ",
                                     "the sidebar.", style = "info"),
                     bsCollapsePanel("Panel 2", "This panel has a generic plot. ",
                                     "and a 'success' style.", plotOutput("genericPlot"), style = "success")
          )
        )
      )
    ),
  server =
    function(input, output, session) {
      output$genericPlot <- renderPlot(plot(rnorm(100)))
      observeEvent(input$p1Button, ({
        updateCollapse(session, "collapseExample", open = "Panel 1")
      }))
      observeEvent(input$styleSelect, ({
        updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
      }))
    }
)