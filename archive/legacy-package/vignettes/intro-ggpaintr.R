## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "figures/vignette-",
  echo=TRUE,
  warning = FALSE,
  message = FALSE,
  out.width = "100%"
)

## ----shinyiris1, echo=FALSE, fig.cap="Some Caption"---------------------------
knitr::include_graphics("images/ggpaintr_iris_1.png")

## ---- include = TRUE, eval = FALSE, attr.source='.numberLines'----------------
#  library(ggpaintr)
#  library(shiny)
#  library(tidyverse)
#  library(shinyWidgets)
#  library(palmerpenguins)
#  
#  # Define UI for application that draws a histogram
#  ui <- fluidPage(
#  
#    # Application title
#    titlePanel("ggpaintr demo"),
#  
#    # Sidebar with a slider input for number of bins
#    sidebarLayout(
#      sidebarPanel(
#        pickerInput("defaultData", "select a default dataset:",
#                    choices = c("iris", "mtcars","penguins", "faithfuld"),
#                    selected = "",
#                    multiple = TRUE,
#                    options = pickerOptions(maxOptions = 1)),
#        uiOutput("controlPanel"),
#        actionButton("draw", "click to draw the plot"),
#      ),
#  
#      # Show a plot of the generated distribution
#      mainPanel(
#        plotOutput("outputPlot"),
#        verbatimTextOutput('outputCode')
#      )
#    )
#  )
#  
#  # Define server logic required to draw a histogram
#  server <- function(input, output) {
#  
#    control_id <- "control_id"
#  
#    # data
#    data_container <- reactive({
#      req(input$defaultData)
#      get(input$defaultData)
#    })
#  
#    # construct paintr object
#    paintr_rctv <- reactive({
#  
#      req(data_container())
#  
#      paintr(control_id,
#             data_container(), data_path = input$defaultData,
#             geom_boxplot(aes(x, y))
#      )
#  
#    })
#  
#    # place ui
#    output$controlPanel <- renderUI({
#      req(paintr_rctv())
#  
#      column(
#        12,
#        paintr_get_ui(paintr_rctv(), "x"),
#        paintr_get_ui(paintr_rctv(), "y"),
#      )
#    })
#  
#    # take results and plot
#    observe({
#      req(paintr_rctv(), data_container(), input$defaultData)
#      data <- data_container()
#  
#      paintr_results <- paintr_plot_code(paintr_rctv())
#  
#      # Plot output
#      output$outputPlot <- renderPlot({
#  
#        paintr_results[['plot']]
#  
#      })
#  
#      # Code output
#      output$outputCode <- renderText({
#  
#        paintr_results[['code']]
#  
#      })
#  
#    }) %>% bindEvent(input$draw)
#  
#  
#  }
#  
#  # Run the application
#  shinyApp(ui = ui, server = server)

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  # add mapping for fill
#  geom_boxplot(aes(x, y, fill)) +
#    coord_flip +        # allow users to flip coordinate
#    labs(x, y, title)   # allow users to add labels for x-axis, y-axis, and title

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  # place ui
#  output$controlPanel <- renderUI({
#    req(paintr_rctv())
#  
#    column(
#      12,
#      paintr_get_ui(paintr_rctv(), "x"),
#      paintr_get_ui(paintr_rctv(), "y"),
#      paintr_get_ui(paintr_rctv(), "fill"),
#      paintr_get_ui(paintr_rctv(), "coord_flip"), # add UI for coord_flip()
#      paintr_get_ui(paintr_rctv(), "labs"),       # add UI for labs()
#    )
#  })

## ----shinyiris2, echo=FALSE, fig.cap="Some Caption"---------------------------
knitr::include_graphics("images/ggpaintr_iris_2.png")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  paintr(
#    id,
#    data,
#    expr,
#    extra_ui = NULL,
#    extra_ui_args = NULL,
#    data_path = "data"
#  )

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  my_paintr <- paintr("id", iris,
#                      geom_boxplot(aes(x, y, fill)))

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  paintr_get_ui(
#    paintr_obj,
#    selected_ui_name,
#    type = "ui",
#    scope = NULL,
#    verbose = FALSE
#  )

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  paintr_plot_code(
#    paintr_obj,
#    selected_color_rctv = NULL,
#    selected_fill_rctv = NULL,
#    color_fill = FALSE,
#    color_group = FALSE,
#    userFUN = NULL,
#    ...
#  )

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  geom_<chart>(aes(<mapping_1>, <mapping_2>, ...), <geom_args_1>, <geom_args_2>, ...) +
#    <plot_settings_1> +
#    <plot_settings_2> +
#    ...

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  scaleColor_build_reactivity(
#    id,
#    paintr_obj,
#    color_or_fill
#  )

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  paintr_rctv <- reactive({
#  
#      req(data_container())
#  
#      paintr(control_id,
#             data_container(), data_path = input$defaultData,
#             geom_boxplot(aes(x, y, fill)) +
#                 coord_flip +
#                 labs(x, y, title) +
#                 scale_fill
#      )
#  })
#  
#  scale_fill_rctv <- scaleColor_build_reactivity(control_id, paintr_rctv, "fill")

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  output$controlPanel <- renderUI({
#      req(paintr_rctv())
#  
#      column(
#          12,
#          paintr_get_ui(paintr_rctv(), "x"),
#          paintr_get_ui(paintr_rctv(), "y"),
#          paintr_get_ui(paintr_rctv(), "fill"),
#          paintr_get_ui(paintr_rctv(), "coord_flip"),
#          paintr_get_ui(paintr_rctv(), "labs"),
#          paintr_get_ui(paintr_rctv(), "scale_fill") # added for scale_fill
#      )
#  })

## ---- echo=TRUE, eval=FALSE---------------------------------------------------
#  paintr_results <- paintr_plot_code(paintr_rctv(),
#                                     selected_fill_rctv = scale_fill_rctv)

