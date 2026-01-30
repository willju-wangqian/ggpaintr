library(ggpaintr)
library(shiny)
library(tidyverse)
library(shinyWidgets)
library(palmerpenguins)
library(rlang)
library(assertthat)

library(lobstr)

library(here)
source(here("paintr2/paintr2_func.R"))
source(here("paintr2/ui_function.R"))

ggpaintr_basic <- function(input_formula) {

  ui <- fluidPage(

    # Application title
    titlePanel("ggpaintr demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        textAreaInput("formula", label = "Enter the paintr formula",
                      value = input_formula, rows = 5, placeholder = "Input the formula"),
        actionButton("enter", "click to enter the formula"),
        uiOutput("controlPanel"),
        actionButton("draw", "click to draw the plot"),
        # actionButton("uiExport", "export the UI"),
      ),

      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("outputPlot"),
        verbatimTextOutput('outputCode')
      )
    )
  )

  server <- function(input, output) {

    paintr_obj <- eventReactive({
      input$enter
    }, {
      paintr_formula(input$formula)
    })

    output$controlPanel <- renderUI({
      req(paintr_obj())

      column(12, paintr_get_tab_ui(paintr_obj()))

    })

    # observe({
    #   req(paintr_obj())
    #
    #   updated_ui_list <- var_ui_replacement(paintr_obj()$ui_list, var_ui_list)
    #   tab_ui <- tab_wrap_ui(updated_ui_list)
    #
    # }) %>% bindEvent(input$uiExport)

    observe({
      req(paintr_obj(), input$enter)

      output_embed_var(input, output, paintr_obj())
    })

    observe({
      req(paintr_obj())

      complete_expr <- paintr_complete_expr(paintr_obj(), input)

      output$outputPlot <- renderPlot({

        paintr_get_plot(complete_expr[['complete_expr_list']])

      })

      output$outputCode <- renderText({

        complete_expr[['code_text']]

      })

    }) %>% bindEvent(input$draw)
  }

  shinyApp(ui, server)
}

ggpaintr_basic2 <- function(input_formula) {

  ui <- fluidPage(

    # Application title
    titlePanel("ggpaintr demo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
      sidebarPanel(
        # textAreaInput("formula", label = "Enter the paintr formula",
        #               value = input_formula, rows = 5, placeholder = "Input the formula"),
        # actionButton("enter", "click to enter the formula"),
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
    session$userData$paintr$obj <- paintr_formula(input_formula)

    observe({
      req(session$userData$paintr$obj)

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
}

world_map <- map_data("world")

# Some EU Contries
some.eu.countries <- c(
  "Portugal", "Spain", "France", "Switzerland", "Germany",
  "Austria", "Belgium", "UK", "Netherlands",
  "Denmark", "Poland", "Italy",
  "Croatia", "Slovenia", "Hungary", "Slovakia",
  "Czech republic"
)
# Retrievethe map data
some.eu.maps <- map_data("world", region = some.eu.countries)

# Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region.lab.data <- some.eu.maps %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

# ggplot(some.eu.maps, aes(x = long, y = lat)) +
#   geom_polygon(aes( group = group, fill = region))+
#   geom_text(aes(label = region), data = region.lab.data,  size = 3, hjust = 0.5)+
#   scale_fill_viridis_d()+
#   theme_void()+
#   theme(legend.position = "none")

source(here("paintr2/paintr2_func.R"))
source(here("paintr2/ui_function.R"))
ggpaintr_basic2(
  "ggplot(data = some.eu.maps, aes(x = var + 1, y = var)) +
    geom_polygon(aes(group = var, fill = var)) +
    geom_polygon(aes(group = var, fill = var)) +
    geom_text(data = region.lab.data, aes(x = var, y = var, label = var),  size = num, hjust = num)+
    scale_fill_viridis_d()+
    theme_void()+
    theme(legend.position = text)"
)






capitalize <- function(string) {
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  string
}

df2 <- msleep

ggpaintr_basic2(
  "
  ggplot(data = df2, aes(x = var, y = var)) +
   geom_point() +
   labs(x = text, y = text, title = text) +
   facet_grid(var ~ var, labeller = expr)
  "
)

ggpaintr_basic2(
  "ggplot(data = iris, aes(x = var, y = var)) +
    geom_point(aes(color = var), size = num) +
    labs(x = text) +
    facet_wrap(expr)"
)

#









data("mpg")

df <- data.frame(
  cat = c('a', 'a', 'b'),
  num = c(1, 2, 3)
)

ggpaintr_basic2(
  "
  ggplot(data = df, aes(x = var, y = var)) +
   geom_col() +
   labs(x = text, y = text, title = text)
  "
)

ggpaintr_basic2(
  "ggplot(data = iris, aes(x = var + 1, y = var)) +
   geom_point() +
   labs(x = text, y = text, title = text)"
)

processed_data <- iris %>% mutate(
  Sepal_Length_modified = Sepal.Length + 1
)

ggpaintr_basic2(
  "ggplot(data = upload, aes(x = var, y = var)) +
   geom_point() +
   labs(x = text, y = text, title = text)"
)


f1 <- function(x) enexpr(x)
f1(a + b + c)



paintr2_basic(library$scatterplot, data)

foo <- function() {
  print("environment of foo. foo2() is called here.")
  print(environment())

  foo2()
}

foo2 <- function() {
  print("the environment of foo2:")
  print(environment())
  print("the environment where foo2 is called:")
  print(parent.frame())
}

foo()


# Define the division function
safe_division <- function(x, y) {
  tryCatch(
    {
      # Expression that might cause an error
      result <- x / y
      if (is.infinite(result)) {
        stop("Attempt to divide by zero.")
      }
      result
    },
    error = function(e) {
      # Error handler: what to do if an error occurs
      cat("Error occurred: ", e$message, "\n")
      NA  # You might return NA or another appropriate value
    },
    finally = {
      # Code that executes no matter what
      cat("Attempted division of", x, "by", y, "\n")
    }
  )
}

# Test the function
safe_division(10, 2)  # Should work
safe_division(10, 0)  # Should trigger the error handler

tt <- expr(geom_point(aes(foo1()), foo2(), foo3(foo4())))
tt <- expr(labs(aes(foo1()), foo2(), foo3(foo4())))
expr_remove_emptycall2(tt)

