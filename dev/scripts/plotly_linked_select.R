# plotly_linked_select.R ------------------------------------------------------
# Example: ggpaintr (ptr_ui_* pieces + ptr_server) driving a plotly render,
# with linked selection:
#   * left  — the ggpaintr formula plot, rendered via ggplotly(); box/lasso
#             select points on it
#   * table — the selected rows, read back through plotly::event_data()
#   * right — a second (fixed) plot that highlights the selected points
#
# Why ptr_ui_controls() and not the all-in-one ptr_ui(): ptr_ui() bundles the
# default *static* ggplot output panel. For a plotly render the supported L3
# pattern is: place ptr_ui_controls() + your own plotlyOutput(), wire the one
# public ptr_server(), and read the live ggplot off state$runtime()$plot
# (see ?ptr_server, section "Custom render (L3)").
#
# Row identity: the formula's data is fixed (mtcars), so we tag each row with
# a key (the plotly-documented `key` aesthetic). ggplotly() carries it through,
# and event_data("plotly_selected")$key hands the selected row indices back.
#
# Run from the package root:  source("dev/scripts/plotly_linked_select.R")

# pkgload::load_all(".", quiet = TRUE)  # the dev tree, NOT the stale installed ggpaintr
library(ggpaintr)
library(shiny)
library(ggplot2)
library(plotly)

ptr_options(gate_draw = FALSE)

# --- the ggpaintr formula -----------------------------------------------------
f <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg), color = ppVar(cyl))) +
    geom_point() +
    theme_minimal()
)

PLOTLY_SOURCE <- "ptr_main"

# --- UI -----------------------------------------------------------------------
ui <- ptr_ui_page(
  titlePanel("ggpaintr + plotly: linked selection"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      ptr_ui_controls(formula = f, id = "p")
    ),
    mainPanel(
      width = 9,
      fluidRow(
        column(6,
          h4("Formula plot (box/lasso-select points)"),
          plotly::plotlyOutput("main_plotly", height = "420px")
        ),
        column(6,
          h4("hp vs qsec (selection highlighted)"),
          plotly::plotlyOutput("second_plot", height = "420px")
        )
      ),
      h4("Selected rows"),
      tableOutput("sel_table")
    )
  )
)

# --- SERVER -------------------------------------------------------------------
server <- function(input, output, session) {
  # The single public server entry point; returns the ptr_state handle.
  state <- ptr_server(f, "p")

  # Custom plotly render off the live runtime: re-runs on every Update click.
  output$main_plotly <- plotly::renderPlotly({
    p <- state$runtime()$plot
    req(p)
    # Tag rows with a stable key so event_data() can identify them.
    p$data$.ptr_row <- seq_len(nrow(p$data))
    p <- p + aes(key = .ptr_row)
    plotly::ggplotly(p, source = PLOTLY_SOURCE) |>
      plotly::layout(dragmode = "select") |>
      plotly::event_register("plotly_selected") |>
      plotly::event_register("plotly_deselect")
  })

  # Selected row indices, kept in a reactiveVal so deselect can clear them.
  sel_rows <- reactiveVal(integer(0))

  observeEvent(plotly::event_data("plotly_selected", source = PLOTLY_SOURCE), {
    sel <- plotly::event_data("plotly_selected", source = PLOTLY_SOURCE)
    keys <- if (is.null(sel$key)) integer(0) else as.integer(unlist(sel$key))
    sel_rows(sort(unique(keys)))
  })

  observeEvent(plotly::event_data("plotly_deselect", source = PLOTLY_SOURCE), {
    sel_rows(integer(0))
  })

  # Table of the selected rows.
  output$sel_table <- renderTable(rownames = TRUE, {
    rows <- sel_rows()
    if (length(rows) == 0) {
      return(data.frame(note = "No points selected — drag a box/lasso on the left plot."))
    }
    mtcars[rows, ]
  })

  # Second plot: same rows, different axes, selection highlighted.
  output$second_plot <- plotly::renderPlotly({
    d <- mtcars
    d$selected <- factor(
      ifelse(seq_len(nrow(d)) %in% sel_rows(), "selected", "other"),
      levels = c("other", "selected")
    )
    p2 <- ggplot(d, aes(x = hp, y = qsec, color = selected, size = selected)) +
      geom_point(alpha = 0.8) +
      scale_color_manual(values = c(other = "grey70", selected = "#d62728")) +
      scale_size_manual(values = c(other = 2, selected = 3.5)) +
      guides(size = "none") +
      theme_minimal()
    plotly::ggplotly(p2)
  })
}

shinyApp(ui, server)
