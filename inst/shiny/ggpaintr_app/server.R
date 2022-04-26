#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#

server <- function(input, output, session) {

  result_container <- reactiveValues()

  # reactive to fileData
  dataInput <- reactive({
    # req(input$fileData)

    if(!is.null(input$defaultData)) {

      result_container[['data']] <- paste0("get('", input$defaultData,  "')")
      result_container[['filename']] <- input$defaultData

      get(input$defaultData)


    } else if (!is.null(input$fileData)) {

      path <- input$fileData$datapath

      ext <- tools::file_ext(path) # [TODO] add Imports tools

      validate(
        need(ext %in% c("csv", "rds"), "Please select either csv or rds file")
      )

      read_fun <- NULL

      if (ext == "csv") {
        inputData <- read.csv(path)
        read_fun <- "read.csv"
      } else if (ext == "rds") {
        inputData <- readRDS(path)
        read_fun <- "readRDS"
      }

      result_container[['data']] <- paste0(read_fun, '("', input$fileData$name, '")')
      result_container[['filename']] <- input$fileData$name

      inputData

    } else {
      NULL
    }

  })

  observeEvent(input$fileData, {
    updatePickerInput(session, "defaultData", selected = "")
  })

  # reactive to mytable_rows_all:
  #   apply the filters
  dataContainer <- reactive({
    req(input$mytable_rows_all, dataInput())
    dataInput()[input$mytable_rows_all, ]
  })

  # render the table
  output$mytable <- DT::renderDataTable({
    req(dataInput())
    datatable(dataInput(), filter = 'top',
              options = list(
                pageLength = 10,
                scrollX = TRUE,
                stateSave = TRUE
              ))
  })

  #############################################################
  # source("inst/R/box_server.R", local = TRUE)

  source("inst/R/box_server_new.R", local = TRUE)

  #############################################################
  # server part for the bar chart
  source("inst/R/bar_server.R", local = TRUE)

  #############################################################
  # server part for the scatter plot
  source("inst/R/scatter_server.R", local = TRUE)

  #############################################################
  # server part for the line plot
  source("inst/R/line_server.R", local = TRUE)

}


