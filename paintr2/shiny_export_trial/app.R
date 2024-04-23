library(ggpaintr)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(palmerpenguins)
library(rlang)
library(assertthat)

library(lobstr)

library(here)
source(here("paintr2", "paintr2_func.R"))
source(here("paintr2", "ui_function.R"))

library(ggsci)


ui <- fluidPage(

  # Application title
  titlePanel("ggpaintr demo"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      textAreaInput("formula", label = "Enter the paintr formula",
                    value =
                      "ggplot(data = mpg, aes(x = var, y = var)) +
                      geom_point() +
                      labs(x = text, y = text)",
                    rows = 5, placeholder = "Input the formula"),
      actionButton("enter", "use above formula to get UI"),
      uiOutput("controlPanel"),
      actionButton("draw", "click to draw the plot"),
      downloadButton("shinyExport", "export the shiny app"),
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("outputPlot"),
      verbatimTextOutput('outputCode')
    )
  )
)

server <- function(input, output, session) {

  session$userData$paintr <- reactiveValues(obj = list(NULL))

  observeEvent(input$enter, {
    session$userData$paintr$obj <- paintr_formula(input$formula)
  })

  observe({
    req(session$userData$paintr$obj, input$enter)

    session$userData$paintr$var_ui_list <-
      output_embed_var(input, output, session$userData$paintr$obj)
  })

  output$controlPanel <- renderUI({
    req(session$userData$paintr)

    column(12, paintr_get_tab_ui(session$userData$paintr$obj))

  })

  output$shinyExport <- downloadHandler(
    filename = "trial_0.R",
    content = function(file) {

      req(session$userData$paintr$var_ui_list)

      generate_shiny(session$userData$paintr$obj,
                     session$userData$paintr$var_ui_list,
                     file,
                     style = TRUE)

    }
  )

  observe({
    req(session$userData$paintr$obj)

    complete_expr_code <-
      paintr_complete_expr(session$userData$paintr$obj, input)

    output$outputPlot <- renderPlot({

      paintr_get_plot(complete_expr_code[['complete_expr_list']])

    })

    output$outputCode <- renderText({

      complete_expr_code[['code_text']]

    })

  }) %>% bindEvent(input$draw)
}

shinyApp(ui, server)



