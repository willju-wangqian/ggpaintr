library(shiny)
library(colourpicker)
library(dplyr)
library(ggplot2)
data = iris

ui = fluidPage(
  uiOutput('myPanel'),
  plotOutput("plot")
)

server = function(input, output, session){
  # Create colour template for each group
  groups = levels(factor(data$Species))
  ngroups = length(groups)
  
  cols <- reactive({
    lapply(1:ngroups, function(i) { # for each group
      colourpicker::colourInput(inputId = paste("col", i, sep="_"), # DO NOT change
                                label = paste("Colour for group ", groups[i], ':', sep = ''), # Text shown on template
                                value = "black") # Initial colour
    })
  })
  
  output$myPanel <- renderUI({cols()})
  
  # Put all the input in a vector
  colors <- reactive({
    lapply(1:ngroups, function(i) {
      input[[paste("col", i, sep="_")]]
    })
  })
  
  output$plot <- renderPlot({
    if (is.null(input$col_1)) {
      cols <- rep("#000000", length(levels(factor(data$Species))))
    } else {
      cols <- unlist(colors())
    }
    
    ggplot(data) + 
      geom_point(aes(x = Sepal.Length, y = Sepal.Width, col = Species)) + 
      scale_color_manual(values = cols)
  })
}

shinyApp(ui, server)

