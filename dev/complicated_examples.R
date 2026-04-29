library(ggplot2)
library(ggpaintr)
ui <- shiny::fluidPage(ptr_module_ui("plot1"))
server <- function(input, output, session) {
  ptr_module_server(
    "plot1",
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() +
    facet_wrap(~ var) +
    labs(title = text, x = text, y = text)"
  )
}
shiny::shinyApp(ui, server)

ui <- fluidPage(
  titlePanel("Embedded ggpaintr"),
  sidebarLayout(
    sidebarPanel(
      ptr_input_ui()
    ),
    mainPanel(
      ptr_output_ui()
    )
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) +
       geom_point() +
       labs(title = text)"
  )

  ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_plot(output, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)
}

shinyApp(ui, server)


ns_a <- shiny::NS("plot_a")
ns_b <- shiny::NS("plot_b")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("A",
             fluidRow(
               column(4, ptr_input_ui(ns = ns_a)),
               column(8, ptr_output_ui(ns = ns_a))
             )),
    tabPanel("B",
             fluidRow(
               column(4, ptr_input_ui(ns = ns_b)),
               column(8, ptr_output_ui(ns = ns_b))
             ))
  )
)

checkbox_defaults1 <- list(geom_point = FALSE)
checkbox_defaults2 <- list(geom_point = FALSE)

server <- function(input, output, session) {
  ptr_server(
    input, output, session,
    formula = "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    ns      = ns_a,
    checkbox_defaults = checkbox_defaults1
  )
  ptr_server(
    input, output, session,
    formula = "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
    ns      = ns_b,
    checkbox_defaults = checkbox_defaults2
  )
}

shinyApp(ui, server)


library(shiny)
library(ggpaintr)
library(palmerpenguins)

formula <- "ggplot(data = penguins,
                   aes(x = var, y = var, color = sex)) +
              geom_point() +
              labs(title = text)"

ui <- navbarPage(
  title = "Penguin morphology",
  # original app functionality
  tabPanel(
    "Sex-by-species summary",
    tableOutput("dimorphism_table")
  ),
  # embed ggpaintr
  tabPanel(
    "Interactive plot",
    sidebarLayout(
      sidebarPanel(ptr_input_ui(ns = NS("plot"))),
      mainPanel(ptr_output_ui(ns = NS("plot")))
    )
  )
)

server <- function(input, output, session) {
  ptr_server(
    input, output, session,
    formula = formula,
    ns      = NS("plot")
  )

  output$dimorphism_table <- renderTable({
    aggregate(body_mass_g ~ species + sex, data = penguins,
              FUN = mean, na.rm = TRUE)
  })
}

shinyApp(ui, server)


library(shiny)
library(ggpaintr)
library(ggiraph)
library(palmerpenguins)
library(colourpicker)

color_placeholder <- ptr_define_placeholder(
  keyword       = "color",
  build_ui      = function(id, copy, meta, context) {
    colourpicker::colourInput(id, copy$label, value = "#e63946")
  },
  resolve_expr  = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }
    rlang::expr(!!as.character(value))
  },
  copy_defaults = list(label = "Highlight color for {param}")
)

boundary <- subset(penguins, species %in% c("Adelie", "Chinstrap"))

formula <- "ggplot(data = boundary, ) +
      geom_point_interactive(aes(
        x = bill_length_mm, y = bill_depth_mm,
        tooltip = var), size = 3, color = color)"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(ptr_input_ui()),
    mainPanel(ggiraph::girafeOutput("interactive_plot"),
              verbatimTextOutput(ptr_build_ids()$code_output))
  )
)

server <- function(input, output, session) {
  state <- ptr_server(
    input, output, session,
    formula      = formula,
    placeholders = list(color_placeholder)
  )

  output$interactive_plot <- ggiraph::renderGirafe({
    base <- ptr_extract_plot(state$runtime())
    if (is.null(base)) return(NULL)
    x <- girafe(
      code = print(base),
    )
    x

  })
}

shinyApp(ui, server)

library(shiny)
library(ggpaintr)
library(plotly)
library(palmerpenguins)

boundary <- subset(penguins, species %in% c("Adelie", "Chinstrap"))

formula <- "ggplot(data = boundary,
                     aes(x = bill_length_mm, y = bill_depth_mm,
                         text = paste(species, sex, island, sep = ' / '))) +
                geom_point()+
                geom_point(data = filter(boundary, bill_depth_mm > num, bill_length_mm > num),
                           color = color)"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(ptr_input_ui()),
    mainPanel(plotly::plotlyOutput("interactive_plot"),
              verbatimTextOutput(ptr_build_ids()$code_output))
  )
)

server <- function(input, output, session) {
  state <- ptr_server(
    input, output, session,
    formula      = formula,
    placeholders = list(color_placeholder)
  )

  output$interactive_plot <- plotly::renderPlotly({
    p <- ptr_extract_plot(state$runtime())
    if (is.null(p)) return(NULL)
    plotly::ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)


library(ggpaintr)
library(shiny)

range_placeholder <- ptr_define_placeholder(
  keyword       = "range",
  build_ui      = function(id, copy, meta, context) {
    sliderInput(id, copy$label,
                min = 0, max = 100, value = c(0, 60), step = 0.5)
  },
  resolve_expr  = function(value, meta, context) {
    if (is.null(value)) return(ptr_missing_expr())
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Zoom range for {param}")
)
library(shiny)
library(ggpaintr)
library(plotly)
library(palmerpenguins)

boundary <- subset(penguins, species %in% c("Adelie", "Chinstrap"))

formula <-
  "ggplot(data = boundary,
        aes(x = bill_length_mm, y = bill_depth_mm,
            color = species,
            text  = paste(species, sex, island, sep = ' / '))) +
   geom_point(size = num) +
   coord_cartesian(xlim = range, ylim = range)"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(ptr_input_ui()),
    mainPanel(plotly::plotlyOutput("interactive_plot"),
              verbatimTextOutput(ptr_build_ids()$code_output))
  )
)

server <- function(input, output, session) {
  state <- ptr_server(
    input, output, session,
    formula      = formula,
    placeholders = list(range_placeholder)
  )

  output$interactive_plot <- plotly::renderPlotly({
    p <- ptr_extract_plot(state$runtime())
    if (is.null(p)) return(NULL)
    plotly::ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)


library(ggpaintr)
ptr_ns_id <- ggpaintr:::ptr_ns_id
log_num <- ptr_define_placeholder(
  keyword = "cols",

  # Return a container; fill it in from bind_ui once data is available.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Populate the selectInput with numeric columns drawn from the data
  # symbol referenced in the formula's ggplot() layer.
  bind_ui = function(input, output, metas, context) {
    eval_env <- context$eval_env
    ggplot_expr <- context$ptr_obj$expr_list[["ggplot"]]
    data_expr <- if (is.call(ggplot_expr)) ggplot_expr$data else NULL
    numeric_cols <- character(0)
    if (!is.null(eval_env) && !is.null(data_expr)) {
      d <- tryCatch(eval(data_expr, envir = eval_env), error = function(e) NULL)
      if (is.data.frame(d)) {
        numeric_cols <- names(d)
      }
    }
    for (meta in metas) {
      local({
        m <- meta
        output[[paste0(m$id, "_container")]] <- shiny::renderUI({
          shiny::selectInput(
            m$id,
            label   = paste("Log-transform column for", m$param),
            choices = numeric_cols,
            multiple = TRUE
          )
        })
      })
    }
    invisible(NULL)
  },

  # Generate `.log_safe(<col>)` in the completed expression.
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }
    rlang::expr(.log_safe(!!rlang::sym(value)))
  },

  # Bind the helper the generated call references by name.
  prepare_eval_env = function(input, metas, eval_env, context) {
    eval_env$.log_safe <- function(x) log(pmax(x, .Machine$double.eps))
    eval_env
  },

  copy_defaults = list(label = "Log-transform column for {param}")
)
cols_placeholder <- ptr_define_placeholder(
  keyword = "cols",

  # Render an empty uiOutput; the real widget is filled in by bind_ui at runtime.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(
      ptr_ns_id(context$ui_ns_fn %||% shiny::NS(NULL),
                paste0(id, "_ui"))
    )
  },

  # Fill the uiOutput once the active dataset is known.
  bind_ui = function(input, output, metas, context) {
    eval_env <- context$eval_env  # cached active eval env (knows the data frame)
    for (meta in metas) {
      ui_id  <- ptr_ns_id(context$ui_ns_fn %||% shiny::NS(NULL), meta$id)
      out_id <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL),
                          paste0(meta$id, "_ui"))
      # Find the data frame the layer will plot.
      data_obj <- get0(meta$layer_name, envir = eval_env, inherits = TRUE)
      cols     <- if (is.data.frame(data_obj)) names(data_obj) else character()

      local({
        captured <- shiny::selectInput(ui_id, "Choose columns",
                                       choices = cols, multiple = TRUE)
        output[[out_id]] <- shiny::renderUI(captured)
      })
    }
    invisible(NULL)
  },

  # Substitute the chosen names into the formula as a tidyselect spec.
  resolve_expr = function(value, meta, context) {
    if (length(value) == 0) return(ptr_missing_expr())
    rlang::expr(dplyr::all_of(!!value))
  }
)

registry <- ptr_merge_placeholders(list(cols = log_num))

# Pipe lives inside data = ...; ggplot(...) stays the top-level layer.
ptr_app(
  "ggplot(
       data =
                dplyr::select(iris, Species, cols) |>
                tidyr::pivot_longer(-Species, names_to = 'measurement'),
       aes(x = measurement, y = value, fill = Species)
     ) + geom_boxplot()",
  placeholders = registry
)

# Pipe lives inside data = ...; ggplot(...) stays the top-level layer.
ptr_app(
  "ggplot(
       data = dplyr::select(iris, cols),
       aes(x = measurement, y = value, fill = Species)
     ) + geom_boxplot()",
  placeholders = registry
)

library(ggpaintr)
ptr_ns_id <- ggpaintr:::ptr_ns_id
log_num <- ptr_define_placeholder(
  keyword = "cols",

  # Return a container; fill it in from bind_ui once data is available.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Populate the selectInput with numeric columns drawn from the data
  # symbol referenced in the formula's ggplot() layer.
  bind_ui = function(input, output, metas, context) {
    eval_env <- context$eval_env
    ggplot_expr <- context$ptr_obj$expr_list[["ggplot"]]
    data_expr <- if (is.call(ggplot_expr)) ggplot_expr$data else NULL
    numeric_cols <- character(0)
    if (!is.null(eval_env) && !is.null(data_expr)) {
      d <- tryCatch(eval(data_expr, envir = eval_env), error = function(e) NULL)
      if (is.data.frame(d)) {
        numeric_cols <- names(d)
      }
    }
    for (meta in metas) {
      local({
        m <- meta
        output[[paste0(m$id, "_container")]] <- shiny::renderUI({
          shiny::selectInput(
            m$id,
            label   = paste("Log-transform column for", m$param),
            choices = numeric_cols,
            multiple = TRUE
          )
        })
      })
    }
    invisible(NULL)
  },

  # Generate `.log_safe(<col>)` in the completed expression.
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }
    rlang::expr(.log_safe(!!rlang::sym(value)))
  },

  # Bind the helper the generated call references by name.
  prepare_eval_env = function(input, metas, eval_env, context) {
    eval_env$.log_safe <- function(x) log(pmax(x, .Machine$double.eps))
    eval_env
  },

  copy_defaults = list(label = "Log-transform column for {param}")
)

registry <- ptr_merge_placeholders(list(cols = log_num))

# Pipe lives inside data = ...; ggplot(...) stays the top-level layer.
ptr_app(
  "ggplot(
       data = dplyr::select(iris, cols),
       aes(x = measurement, y = value, fill = Species)
     ) + geom_boxplot()",
  placeholders = registry
)


log_num <- ptr_define_placeholder(
  keyword = "log_num",

  # Return a container; fill it in from bind_ui once data is available.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Populate the selectInput with numeric columns drawn from the data
  # symbol referenced in the formula's ggplot() layer.
  bind_ui = function(input, output, metas, context) {
    eval_env <- context$eval_env
    ggplot_expr <- context$ptr_obj$expr_list[["ggplot"]]
    data_expr <- if (is.call(ggplot_expr)) ggplot_expr$data else NULL
    numeric_cols <- character(0)
    if (!is.null(eval_env) && !is.null(data_expr)) {
      d <- tryCatch(eval(data_expr, envir = eval_env), error = function(e) NULL)
      if (is.data.frame(d)) {
        numeric_cols <- names(d)[vapply(d, is.numeric, logical(1))]
      }
    }
    for (meta in metas) {
      local({
        m <- meta
        output[[paste0(m$id, "_container")]] <- shiny::renderUI({
          shiny::selectInput(
            m$id,
            label   = paste("Log-transform column for", m$param),
            choices = numeric_cols
          )
        })
      })
    }
    invisible(NULL)
  },

  # Generate `.log_safe(<col>)` in the completed expression.
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }
    rlang::expr(.log_safe(!!rlang::sym(value)))
  },

  # Bind the helper the generated call references by name.
  prepare_eval_env = function(input, metas, eval_env, context) {
    eval_env$.log_safe <- function(x) log(pmax(x, .Machine$double.eps))
    eval_env
  },

  copy_defaults = list(label = "Log-transform column for {param}")
)

registry <- ptr_merge_placeholders(list(log_num = log_num))

ptr_app(
  "ggplot(data = mtcars, aes(x = wt, y = log_num)) + geom_point()",
  placeholders = registry
)


cols_ph <- ptr_define_placeholder(
  keyword = "cols",

  # Render an empty container; bind_ui fills it once the data is known.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Walk ggplot()'s `data =` argument to its source data symbol, eval it in
  # the live env, and offer its columns. This handles both
  #   data = iris
  # and chained forms like
  #   data = iris |> dplyr::select(...) |> tidyr::pivot_longer(...)
  bind_ui = function(input, output, metas, context) {
    eval_env  <- context$eval_env
    data_expr <- context$ptr_obj$expr_list[["ggplot"]]$data

    find_source <- function(e) {
      if (is.symbol(e)) return(e)
      if (is.call(e) && length(e) >= 2L) return(find_source(e[[2]]))
      NULL
    }
    src <- find_source(data_expr)

    df <- if (!is.null(src) && !is.null(eval_env)) {
      tryCatch(eval(src, envir = eval_env), error = function(e) NULL)
    }
    choices <- if (is.data.frame(df)) names(df) else character()

    for (meta in metas) {
      local({
        m <- meta
        output[[paste0(m$id, "_container")]] <- shiny::renderUI({
          shiny::selectInput(
            m$id,
            label    = paste("Choose columns for", m$param),
            choices  = choices,
            multiple = TRUE
          )
        })
      })
    }
    invisible(NULL)
  },

  # A vector of column names becomes a tidyselect spec dplyr::select() accepts.
  resolve_expr = function(value, meta, context) {
    if (length(value) == 0) return(ptr_missing_expr())
    rlang::expr(dplyr::all_of(!!value))
  },

  copy_defaults = list(label = "Choose columns for {param}")
)

registry <- ptr_merge_placeholders(list(cols = cols_ph))

# Pipe lives inside data = ...; ggplot(...) stays the top-level layer.
ptr_app(
  "ggplot(
       data = iris |>
                dplyr::select(Species, cols) |>
                tidyr::pivot_longer(-Species, names_to = 'metric'),
       aes(x = metric, y = value, fill = Species)
     ) + geom_boxplot()",
  placeholders = registry
)





flea_pcp <- flea %>%
  pcp_select(species, 2:7, species) %>%
  pcp_scale(method="uniminmax") %>%
  pcp_arrange()
ggplot(data = flea_pcp, aes_pcp()) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species))

ptr_app(
  'ggplot(data = flea |>
  pcp_select(cols) |>
  pcp_scale(method="uniminmax") |>
  pcp_arrange() , aes(x = pcp_x, y = pcp_y, yend = pcp_yend, class = pcp_class,
    group = pcp_id, level = pcp_level, label = pcp_level)) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species))',
  placeholders = registry
)



library(ggpcp )
data(flea, package = "GGally")

cols_ph <- ptr_define_placeholder(
  keyword = "cols",

  # Render an empty container; bind_ui fills it once the data is known.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Walk ggplot()'s `data =` argument to its source data symbol, eval it in
  # the live env, and offer its columns. This handles both
  #   data = iris
  # and chained forms like
  #   data = iris |> dplyr::select(...) |> tidyr::pivot_longer(...)
  bind_ui = function(input, output, metas, context) {
    eval_env  <- context$eval_env
    data_expr <- context$ptr_obj$expr_list[["ggplot"]]$data

    find_source <- function(e) {
      if (is.symbol(e)) return(e)
      if (is.call(e) && length(e) >= 2L) return(find_source(e[[2]]))
      NULL
    }
    src <- find_source(data_expr)

    df <- if (!is.null(src) && !is.null(eval_env)) {
      tryCatch(eval(src, envir = eval_env), error = function(e) NULL)
    }
    choices <- if (is.data.frame(df)) names(df) else character()

    for (meta in metas) {
      local({
        m <- meta
        output[[paste0(m$id, "_container")]] <- shiny::renderUI({
          shiny::selectInput(
            m$id,
            label    = paste("Choose columns for", m$param),
            choices  = choices,
            multiple = TRUE
          )
        })
      })
    }
    invisible(NULL)
  },

  # A vector of column names becomes a tidyselect spec dplyr::select() accepts.
  resolve_expr = function(value, meta, context) {
    if (length(value) == 0) return(ptr_missing_expr())
    rlang::expr(dplyr::all_of(!!value))
  },

  copy_defaults = list(label = "Choose columns for {param}")
)
range_placeholder <- ptr_define_placeholder(
  keyword       = "range",
  build_ui      = function(id, copy, meta, context) {
    sliderInput(id, copy$label,
                min = 0, max = 20, value = c(0, 60), step = 0.001)
  },
  resolve_expr  = function(value, meta, context) {
    if (is.null(value)) return(ptr_missing_expr())
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Zoom range for {param}")
)
registry <- ptr_merge_placeholders(list(cols = cols_ph,
                                        range = range_placeholder))

ptr_app(
  'ggplot(data = flea |>
  pcp_select(cols) |>
  pcp_scale(method="uniminmax") |>
  pcp_arrange(method = "from-right") , mapping = aes_pcp(x = pcp_x)) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species)) +
  coord_cartesian(xlim = range, ylim = range)',
  placeholders = registry
)

aes_pcp_mapping <- aes_pcp()
ptr_app(
  'ggplot(data = flea |>
  pcp_select(cols) |>
  pcp_scale(method="uniminmax") |>
  pcp_arrange(method = "from-right") , mapping = aes_pcp_mapping) +
  geom_pcp_axes() +
  geom_pcp(aes(colour = species)) +
  coord_cartesian(xlim = range, ylim = range)',
  placeholders = registry
)
