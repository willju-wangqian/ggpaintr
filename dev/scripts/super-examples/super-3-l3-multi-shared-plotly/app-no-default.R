# Playable copy. Run from repo root: devtools::load_all(".") then shiny::runApp("<this dir>"). See dev/scripts/super-examples/README.md.
# super-3-l3-multi-shared-plotly (no-default variant): same formula
# structure as app.R but every `pp*(default)` is stripped of its positional
# default. Path-A pressure-test variant per ADR-0016.
library(shiny)

ppRange <- ptr_define_placeholder_value(
  keyword     = "ppRange",
  positional_arg = ptr_arg_numeric_vector(length = 2),
  build_ui    = function(node, label = NULL, selected = NULL, ...) {
    val <- if (!is.null(selected) && length(selected) == 2L) selected else c(0, 100)
    shiny::sliderInput(node$id, label = label %||% "Range",
                       min = -100, max = 100, value = val, step = 0.1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    value <- as.double(value)
    rlang::expr(c(!!value[1], !!value[2]))
  }
)

formula_a <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(), y = ppVar(),
                     color = ppVar(shared = "linked"))) +
    geom_point(size = ppNum(), alpha = ppNum()) +
    scale_x_continuous(limits = ppRange()) +
    labs(title = ppText())
)
formula_b <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(), y = ppVar(),
                     color = ppVar(shared = "linked"))) +
    geom_point(size = ppNum()) +
    geom_smooth(method = ppText(), linewidth = ppNum()) +
    labs(title = ppText())
)
formula_a_str <- paste(deparse(formula_a), collapse = "\n")
formula_b_str <- paste(deparse(formula_b), collapse = "\n")

shared <- ptr_shared(
  formulas  = list(formula_a_str, formula_b_str),
  shared_ui = list(
    linked = function(id) shiny::selectInput(id, "Linked color", names(mtcars))
  )
)

ui <- ptr_ui_page(
  ptr_shared_panel(shared),
  shiny::fluidRow(
    shiny::column(
      6,
      ptr_ui_controls(formula_a_str, "plot1"),
      ptr_ui_plot("plot1"),
      ptr_ui_code("plot1")
    ),
    shiny::column(
      6,
      ptr_ui_controls(formula_b_str, "plot2"),
      plotly::plotlyOutput(shiny::NS("plot2")("custom_plot"), height = "500px") |>
        ptr_ui_toggle_code(ptr_ui_code("plot2"))
    )
  )
)

server <- function(input, output, session) {
  shared_state <- ptr_shared_server(shared)
  state_a <- ptr_server(formula_a, "plot1", shared_state = shared_state)
  state_b <- ptr_server(formula_b, "plot2", shared_state = shared_state)
  output[[shiny::NS("plot2")("custom_plot")]] <- plotly::renderPlotly({
    res <- state_b$runtime()
    shiny::req(isTRUE(res$ok), res$plot)
    plotly::ggplotly(res$plot)
  })
}

shiny::shinyApp(ui, server)
