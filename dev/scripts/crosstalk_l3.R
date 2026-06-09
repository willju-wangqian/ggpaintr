# crosstalk_l3.R --------------------------------------------------------------
# An L3-style Shiny app that applies ggpaintr's *pattern* (scan placeholders ->
# build widgets -> substitute -> eval -> embed) to a crosstalk `bscols()`
# expression instead of a ggplot.
#
# NOTE on why this does NOT use ptr_server():
#   ggpaintr's runtime is ggplot-locked. After eval it runs
#   ptr_assemble_plot_safe() (requires a ptr_root with >=1 layer) and
#   ptr_validate_plot_render_safe() (calls ggplot2::ggplot_build()). A crosstalk
#   bscols() bundle is HTML, not a ggplot, so it is rejected by both gates.
#   We therefore reuse only the host-agnostic IDEA: a recursive AST walk for
#   `pp*` tokens, value substitution, then eval + renderUI. (~80 lines below.)

library(shiny)
library(plotly)
library(crosstalk)

# --- the placeholder-bearing expression (the "formula") ----------------------
# ppVar(<default>) -> a column picker (default = the bareword shown).
# ppText(<default>) -> a free-text input (default = the string shown).
ptr_expr <- quote({
  shared <- crosstalk::SharedData$new(mtcars)
  crosstalk::bscols(
    widths = c(3, 9),
    list(
      crosstalk::filter_slider("hp", "Horsepower", shared, ~hp, width = "100%"),
      crosstalk::filter_checkbox("cyl", "Cylinders", shared, ~cyl, inline = TRUE)
    ),
    plotly::plot_ly(
      shared,
      x = ~ppVar(wt), y = ~ppVar(mpg), color = ~factor(ppVar(cyl)),
      type = "scatter", mode = "markers", marker = list(size = 10)
    ) |>
      plotly::layout(title = ppText("Weight vs MPG"), dragmode = "select") |>
      plotly::highlight(on = "plotly_selected", off = "plotly_deselect")
  )
})

DATA_COLS <- names(mtcars)

# --- 1. WALK: collect placeholders -------------------------------------------
# Recursive descent over the language object; records every ppVar()/ppText()
# call with a stable id (kind + default + occurrence index).
ptr_scan_placeholders <- function(expr) {
  found <- list()
  seen <- new.env(parent = emptyenv())
  next_id <- function(kind, default) {
    base <- paste0(kind, "_", default)
    n <- (if (is.null(seen[[base]])) 0L else seen[[base]]) + 1L
    seen[[base]] <- n
    if (n == 1L) base else paste0(base, "_", n)
  }
  walk <- function(node) {
    if (is.call(node)) {
      head <- node[[1]]
      if (is.symbol(head) && as.character(head) %in% c("ppVar", "ppText")) {
        kind <- as.character(head)
        default <- if (length(node) >= 2) {
          d <- node[[2]]
          if (is.symbol(d)) as.character(d) else as.character(d)
        } else ""
        found[[length(found) + 1L]] <<- list(
          kind = kind, default = default, id = next_id(kind, default)
        )
        return(invisible())  # don't descend into the placeholder's own args
      }
      for (i in seq_along(node)) walk(node[[i]])
    }
  }
  walk(expr)
  found
}

# --- 2. WIDGETS: one input per placeholder -----------------------------------
ptr_make_widget <- function(ph, ns) {
  if (ph$kind == "ppVar") {
    selectInput(ns(ph$id), label = ph$default, choices = DATA_COLS,
                selected = ph$default)
  } else {
    textInput(ns(ph$id), label = ph$id, value = ph$default)
  }
}

# --- 3. SUBSTITUTE: replace placeholders with chosen values ------------------
# Mirrors the scan's id assignment so ids line up 1:1 with the widgets.
ptr_substitute <- function(expr, values) {
  seen <- new.env(parent = emptyenv())
  next_id <- function(kind, default) {
    base <- paste0(kind, "_", default)
    n <- (if (is.null(seen[[base]])) 0L else seen[[base]]) + 1L
    seen[[base]] <- n
    if (n == 1L) base else paste0(base, "_", n)
  }
  rewrite <- function(node) {
    if (is.call(node)) {
      head <- node[[1]]
      if (is.symbol(head) && as.character(head) %in% c("ppVar", "ppText")) {
        kind <- as.character(head)
        default <- if (length(node) >= 2) as.character(node[[2]]) else ""
        id <- next_id(kind, default)
        val <- values[[id]]
        if (is.null(val)) val <- default
        return(if (kind == "ppVar") as.symbol(val) else as.character(val))
      }
      as.call(lapply(node, rewrite))
    } else node
  }
  rewrite(expr)
}

# --- UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("L3 crosstalk app (ggpaintr pattern, custom render)"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      uiOutput("ptr_controls"),
      actionButton("draw", "Draw", class = "btn-primary")
    ),
    mainPanel(width = 9, uiOutput("ptr_plot"))
  )
)

# --- SERVER ------------------------------------------------------------------
server <- function(input, output, session) {
  placeholders <- ptr_scan_placeholders(ptr_expr)

  output$ptr_controls <- renderUI({
    lapply(placeholders, ptr_make_widget, ns = identity)
  })

  drawn <- eventReactive(input$draw, ignoreNULL = FALSE, {
    values <- setNames(
      lapply(placeholders, function(ph) input[[ph$id]] %||% ph$default),
      vapply(placeholders, `[[`, "", "id")
    )
    new_expr <- ptr_substitute(ptr_expr, values)
    eval(new_expr, envir = new.env(parent = globalenv()))
  })

  output$ptr_plot <- renderUI({
    htmltools::browsable(drawn())
  })
}

`%||%` <- function(a, b) if (is.null(a)) b else a

shinyApp(ui, server)
