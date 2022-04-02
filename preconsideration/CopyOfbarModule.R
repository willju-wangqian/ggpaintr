
# For the bar chart
barControlUI <- function(id, data) {
  ns <- NS(id)
  
  column(12,
         box(
           br(),
           h4("mappings"),
           pickerInput(ns("mapX"), "x:",
                       choices = names(data),
                       selected = "",
                       multiple = TRUE,
                       options = pickerOptions(maxOptions = 1)),
           pickerInput(ns("mapY"), "y:",
                       choices = names(data),
                       selected = "",
                       multiple = TRUE,
                       options = pickerOptions(maxOptions = 1)),
           pickerInput(ns("mapColor"), "color:",
                       choices = names(data),
                       selected = "",
                       multiple = TRUE,
                       options = pickerOptions(maxOptions = 1)),
           h4("bar settings"),
           pickerInput(ns("settingStat"), "stat:",
                       choices = c("bin", "identity", "count"),
                       selected = "",
                       multiple = TRUE,
                       options = pickerOptions(maxOptions = 1)),
           pickerInput(ns("settingPosition"), "position:",
                       choices = c("dodge" = "position_dodge", "fill" = "position_fill"),
                       selected = "",
                       multiple = TRUE,
                       options = pickerOptions(maxOptions = 1)),
           numericInput(ns("settingPositionWidth"), "position width:",
                        NULL,
                        min = 0, max = 1),
           numericInput(ns("settingWidth"), "bar width (between 0 and 1):",
                        NULL,
                        min = 0, max = 1),
           
           h4("misc"),
           pickerInput(ns("miscFacet"), "choose variables for facet (max 2):",
                       choices = names(data),
                       selected = "",
                       multiple = TRUE,
                       options = pickerOptions(maxOptions = 2)),
           checkboxInput(ns("miscFlip"), "Flip the coordinate", value = FALSE, width = NULL)
         ),
         themeUI(ns)
         
  )
  
}

barControlServer <- function(id, data) {
  moduleServer(
    id,
    function(input, output, session) {
      
      # generate the basic plot
      ggPlotObject <- reactive({
        
        barArgList <- list()
        
        aesList <- list()
        
        if(!is.null(input$mapX)) aesList$x <- input$mapX
        if(!is.null(input$mapY)) aesList$y <- input$mapY
        if(!is.null(input$mapColor)) {
          aesList$color <- input$mapColor
          aesList$fill <- input$mapColor
          aesList$group <- input$mapColor
        }
        if(!is.null(input$settingStat)) {
          barArgList$stat = input$settingStat
          # if(input$mapStat == "bin" & has_name(aesList, y)) {
          #   cat("check1\n")
          #   aesList <- aesList[- which(names(aesList) == "y")]
          #   cat("good\n")
          # }
        }
        
        if(!is.null(input$settingPosition)) 
          barArgList$position = 
            getFromNamespace(input$settingPosition, "ggplot2")(
              width = if_else(
                !is.na(input$settingPositionWidth), 
                input$settingPositionWidth,
                NULL
              )
            )
        
        barArgList$width <- 
          if_else(
            !is.na(input$settingWidth),
            input$settingWidth,
            NULL
          )
        
        barArgList$mapping = do.call(aes_string, aesList)
        
        do.call(geom_bar, barArgList)
        
      })
      
      ggPlotObject
      

    }
  )
}
#################