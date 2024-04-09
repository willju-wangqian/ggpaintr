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
    paintr_expr <- parse_expr(input$formula)
    paintr_expr_list <- unlist(break_sum(paintr_expr))
    paintr_expr_names <- sapply(paintr_expr_list, get_fun_names)
    paintr_expr_names <- handle_duplicate_names(paintr_expr_names)
    paintr_expr_list <- set_names(paintr_expr_list, paintr_expr_names)

    index_path_list <- lapply(paintr_expr_list, get_index_path)
    id_list <- lapply(names(index_path_list), function(.nn) {
      lapply(index_path_list[[.nn]], encode_id, .nn)
    })
    index_path_list <- purrr::map2(index_path_list, id_list, set_names)

    keywords_list <- purrr::map2(
      index_path_list, paintr_expr_list, function(.path, .expr) {
        lapply(.path, function(.x, .exprr) expr_pluck(.exprr, .x),
               .exprr = .expr)
      })

    paintr_expr_param_list <- purrr::map2(
      paintr_expr_list, index_path_list, function(.expr, .path_list) {
        lapply(.path_list, function(.path) {
          get_expr_param(.expr, .path)
        })
      }
    )

    paintr_ui_list <- purrr::pmap(
      list(keywords_list, id_list, paintr_expr_param_list),
      function(k_l, id_l, p_l) {
        purrr::pmap(list(k_l, id_l, p_l), generate_ui_individual)
      }
    )

    list(
      param_list = paintr_expr_param_list,
      keywords_list = keywords_list,
      index_path_list = index_path_list,
      id_list = id_list,
      expr_list = paintr_expr_list,
      ui_list = paintr_ui_list
    )
  })

  output$controlPanel <- renderUI({
    req(paintr_obj())

    ui_list <- paintr_obj()$ui_list

    tab_list <- unname(purrr::map2(ui_list, names(ui_list),
                                   function(ui, nn) do.call(tabPanel, c(nn, unname(ui)))))
    tab_ui <- do.call(tabsetPanel, tab_list)

    # ui_piece <- unname(unlist(paintr_obj()$ui_list, recursive = FALSE))

    # do.call(column, c(12, ui_piece))
    column(12, tab_ui)

  })

  observe({
    req(paintr_obj())

    output_embed_var(input, output, paintr_obj())
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

    # browser()

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


  # observe({
  #   req(paintr_obj(), input$defaultData)
  #
  #   foo()
  # })




  # control_id <- "control_id"
  #
  # # data
  # data_container <- reactive({
  #   req(input$defaultData)
  #   get(input$defaultData)
  # })
  #
  # paintr_rctv <- eventReactive({
  #   input$enter
  #   data_container()
  # }, {
  #   req(data_container())
  #
  #   paintr_expr <- parse_expr(input$formula)
  #
  #   paintr(control_id,
  #          data_container(), data_path = input$defaultData,
  #          !!paintr_expr
  #   )
  # })
  #
  #
  # # place ui
  # output$controlPanel <- renderUI({
  #   req(paintr_rctv())
  #
  #   # browser()
  #
  #   do.call(column,
  #           c(12, paintr_get_ui_all(paintr_rctv())))
  # })
  #
  #
  # output$paintr2_main_ui <- renderUI({
  #   column(12,
  #          )
  #
  # })
  #
  #
  # # take results and plot
  # observe({
  #   req(paintr_rctv(), data_container(), input$defaultData)
  #   data <- data_container()
  #
  #   paintr_results <- paintr_plot_code(paintr_rctv())
  #
  #   # Plot output
  #   output$outputPlot <- renderPlot({
  #
  #     paintr_results[['plot']]
  #
  #   })
  #
  #   # Code output
  #   output$outputCode <- renderText({
  #
  #     paintr_results[['code']]
  #
  #   })
  #
  #   browser()
  #
  # }) %>% bindEvent(input$draw)


}

# Run the application
shinyApp(ui = ui, server = server)







