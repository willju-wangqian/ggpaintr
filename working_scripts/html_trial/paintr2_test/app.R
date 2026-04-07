library(ggpaintr)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(palmerpenguins)
library(rlang)
library(assertthat)

library(lobstr)

source("../paintr2_func.R")
source("../ui_function.R")

i <- 1
var_ui_list <- list()

paintr_get_ui_all <- function(paintr_obj, type = "ui", verbose = FALSE) {


  stopifnot(class(paintr_obj) == "paintr_obj")

  type <- match.arg(type, c("ui", "id"))

  ui_list_all <- unname(unlist(
    paintr_obj[['shiny_components']][[type]], recursive = FALSE
  ))

  browser()

  return(ui_list_all)
}

# uis <- paintr_get_ui_all(paintr_obj)
# do.call(column, c(12, uis))



# Define UI for application that draws a histogram
ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textAreaInput("formula", label = "Enter the paintr formula",
                    value = "ggplot(data = upload, aes(x = var + 1, y = var)) +
   geom_point(aes(color = var), size = num, color = text) +
   labs(x = text, y = text, title = text, tags = text)", rows = 5, placeholder = "Input the formula"),
   actionButton("enter", "click to enter the formula"),
   # pickerInput("defaultData", "select a default dataset:",
   #             choices = c("iris", "mtcars","penguins", "faithfuld"),
   #             selected = "",
   #             multiple = TRUE,
   #             options = pickerOptions(maxOptions = 1)),
   uiOutput("controlPanel"),
   # uiOutput("paintr2_main_ui"),
   actionButton("draw", "click to draw the plot"),
   actionButton("uiExport", "export the UI"),
    ),

   # Show a plot of the generated distribution
   mainPanel(
     plotOutput("outputPlot"),
     verbatimTextOutput('outputCode')
   )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  paintr_obj <- eventReactive({
    input$enter
  }, {
    paintr_formula(input$formula)
  })

  output$controlPanel <- renderUI({
    req(paintr_obj())

    column(12, paintr_get_tab_ui(paintr_obj()))

    # browser()

  })

  observe({
    req(paintr_obj())

    updated_ui_list <- var_ui_replacement(paintr_obj()$ui_list, var_ui_list)
    tab_ui <- tab_wrap_ui(updated_ui_list)


    browser()

  }) %>% bindEvent(input$uiExport)

  observe({
    req(paintr_obj(), input$enter)

    var_ui_list <<- register_var_ui_outputs(input, output, paintr_obj())
  })

  observe({
    req(paintr_obj())

    paintr_processed_expr_list <- paintr_obj()[['expr_list']]
    unfolded_id_list <- unlist(paintr_obj()[['id_list']])
    keywords_list <- paintr_obj()[['keywords_list']]
    index_path_list <- paintr_obj()[['index_path_list']]

    for (id in unfolded_id_list) {

      id_domain <- unlist(strsplit(id, "\\+"))[1]
      paintr_processed_expr_list[[id_domain]] <-
        expr_replace_keywords(paintr_processed_expr_list[[id_domain]],
                              keywords_list[[id_domain]][[id]],
                              index_path_list[[id_domain]][[id]],
                              input[[id]])
    }


    paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_null)
    paintr_processed_expr_list <- lapply(paintr_processed_expr_list, expr_remove_emptycall)

    code_text_list <- lapply(paintr_processed_expr_list, expr_text)
    code_text <- do.call(paste, c(unname(code_text_list), sep = ' +\n  '))

    paintr_plot_list <- lapply(paintr_processed_expr_list, eval)
    p <- paintr_plot_list[[1]]
    for (i in 2:length(paintr_plot_list)) p <- p + paintr_plot_list[[i]]

    output$outputPlot <- renderPlot({

      p


    })

    output$outputCode <- renderText({

      code_text

    })

  }) %>% bindEvent(input$draw)

}

# Run the application
shinyApp(ui = ui, server = server)


# expr <- expr(ggplot(data = input$data, aes(x = input$var)))



