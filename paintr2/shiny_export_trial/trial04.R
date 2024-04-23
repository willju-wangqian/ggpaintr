library(shiny)

ui <- fluidPage(
  titlePanel("Download with Save-As Prompt"),

  sidebarLayout(
    sidebarPanel(
      # Create a download button
      downloadButton("downloadData", "Download Data")
    ),

    mainPanel(
      # Outputs or other UI elements can be placed here
    )
  )
)

server <- function(input, output) {

  # Define the download handler
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-download-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Generate data or get the result data
      data <- mtcars  # This can be replaced by any data frame

      # Write the data to a file
      write.csv(data, file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
