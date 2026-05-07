# Public entry points for the typed-AST core. Each entry point composes the
# UI builder (per-layer panels via `build_ui_for.ptr_layer`) and the server
# wiring (`ptr_server`).

#' Build a Shiny App from a `ggpaintr` Formula
#'
#' Translates `formula` into the typed AST, builds the per-layer panel UI,
#' and wires the server end-to-end. Returns a `shiny.appobj` ready to be run.
#'
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ui_text Optional named list of copy overrides for UI labels and
#'   placeholders.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes.
#' @param expr_check Controls `expr` placeholder validation. `TRUE` (default)
#'   applies the built-in safety walker; `FALSE` disables checking.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Defaults to `character()`.
#' @param ns A namespace function (`character -> character`), typically
#'   `shiny::NS(id)` or `shiny::NS(NULL)`.
#'
#' @return A `shiny.appobj`.
#' @export
ptr_app <- function(formula,
                       envir = parent.frame(),
                       ui_text = NULL,
                       checkbox_defaults = NULL,
                       expr_check = TRUE,
                       safe_to_remove = character(),
                       ns = shiny::NS(NULL)) {
  parts <- ptr_app_components(
    formula,
    envir = envir,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    expr_check = expr_check,
    safe_to_remove = safe_to_remove,
    ns = ns
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

ptr_app_components <- function(formula,
                                  envir = parent.frame(),
                                  ui_text = NULL,
                                  checkbox_defaults = NULL,
                                  expr_check = TRUE,
                                  safe_to_remove = character(),
                                  ns = shiny::NS(NULL)) {
  tree <- ptr_translate(formula, expr_check = expr_check)

  ui <- ptr_build_app_ui(
    tree,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    ns = ns
  )

  server <- function(input, output, session) {
    ptr_server(
      input, output, session, formula,
      envir = envir,
      ui_text = ui_text,
      checkbox_defaults = checkbox_defaults,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove,
      ns = ns
    )
  }
  list(ui = ui, server = server)
}

ptr_build_app_ui <- function(tree, ui_text = NULL,
                                checkbox_defaults = NULL,
                                ns = shiny::NS(NULL)) {
  shell_copy <- layer_panel_default_shell_copy(ui_text)
  layer_names <- vapply(tree$layers, function(l) l$name, character(1))

  panels <- lapply(tree$layers, function(layer) {
    build_ui_for(layer,
                 ui_text = ui_text,
                 ns_fn = ns,
                 checkbox_defaults = checkbox_defaults,
                 shell_copy = shell_copy)
  })

  picker <- shinyWidgets::pickerInput(
    inputId = ns("ptr_layer_select"),
    label = shell_copy$layer_picker_label %||% "Layer",
    choices = layer_names,
    selected = if (length(layer_names)) layer_names[1L] else NULL
  )
  hidden_tabset <- do.call(
    shiny::tabsetPanel,
    c(list(id = ns("ptr_layer_tabset"), type = "hidden"), panels)
  )

  shiny::fluidPage(
    shiny::titlePanel(ptr_resolve_ui_text("title", ui_text = ui_text)$label %||% ""),
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        picker,
        hidden_tabset,
        shiny::actionButton(
          ns("ptr_update_plot"),
          label = shell_copy$update_plot_label %||% "Update plot"
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput(ns("ptr_plot")),
        shiny::uiOutput(ns("ptr_error")),
        shiny::verbatimTextOutput(ns("ptr_code"))
      )
    )
  )
}

# ---- Module variants ----

#' Module UI for a `ggpaintr` Formula
#'
#' Namespaced UI side of a Shiny module wrapping a `ggpaintr` formula. Pair
#' with [ptr_module_server()].
#'
#' @param id Module id; the namespace prefix for inputs and outputs.
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param ui_text Optional named list of copy overrides.
#' @param checkbox_defaults Optional named list of initial checked states.
#' @param expr_check Controls `expr` placeholder validation. Defaults to `TRUE`.
#'
#' @return A Shiny tag list.
#' @export
ptr_module_ui <- function(id, formula, ui_text = NULL,
                             checkbox_defaults = NULL, expr_check = TRUE) {
  tree <- ptr_translate(formula, expr_check = expr_check)
  ptr_build_app_ui(
    tree,
    ui_text = ui_text,
    checkbox_defaults = checkbox_defaults,
    ns = shiny::NS(id)
  )
}

#' Module Server for a `ggpaintr` Formula
#'
#' Namespaced server side of a Shiny module wrapping a `ggpaintr` formula.
#' Pair with [ptr_module_ui()]. Additional arguments are forwarded to
#' [ptr_server_state()] (e.g. `shared`, `draw_trigger`, `expr_check`,
#' `safe_to_remove`, `ui_text`, `checkbox_defaults`).
#'
#' @param id Module id; must match the id passed to [ptr_module_ui()].
#' @param formula A single formula string with `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param ... Forwarded to [ptr_server_state()].
#'
#' @return The `ptr_state` list from [ptr_server_state()] (returned by the
#'   inner module session for advanced wiring; usually consumed for its
#'   side effects only).
#' @export
ptr_module_server <- function(id, formula, envir = parent.frame(), ...) {
  shiny::moduleServer(id, function(input, output, session) {
    ptr_server(input, output, session, formula,
                  envir = envir, ns = session$ns, ...)
  })
}

# ---- Grid app: N plots + top-level shared widgets + draw-all ----

#' Grid App: Multiple `ggpaintr` Plots With Shared Controls
#'
#' Builds a fluid layout of N plot modules with a top-level `wellPanel` for
#' shared input widgets and a "Draw all" button that triggers a redraw across
#' every plot. Each plot's `shared = "..."` placeholders read from the
#' corresponding entry in `shared_ui` instead of rendering local widgets.
#'
#' @param plots A list of formula strings, one per plot.
#' @param shared_ui Named list mapping shared key → `function(id) -> shiny.tag`
#'   builder. Names must match the `shared = "..."` annotations used in
#'   `plots`. Pass `list()` if there are no shared placeholders.
#' @param envir Environment used to resolve local data objects.
#' @param title App title shown in the page header.
#' @param draw_all_label Label for the draw-all action button.
#' @param expr_check Controls `expr` placeholder validation. Defaults to `TRUE`.
#'
#' @return A `shiny.appobj`.
#' @export
ptr_app_grid <- function(plots,
                            shared_ui = list(),
                            envir = parent.frame(),
                            title = "ggpaintr grid",
                            draw_all_label = "Draw all",
                            expr_check = TRUE) {
  parts <- ptr_app_grid_components(
    plots = plots,
    shared_ui = shared_ui,
    envir = envir,
    title = title,
    draw_all_label = draw_all_label,
    expr_check = expr_check
  )
  shiny::shinyApp(ui = parts$ui, server = parts$server)
}

ptr_app_grid_components <- function(plots,
                                       shared_ui = list(),
                                       envir = parent.frame(),
                                       title = "ggpaintr grid",
                                       draw_all_label = "Draw all",
                                       expr_check = TRUE) {
  if (is.character(plots)) plots <- as.list(plots)
  assertthat::assert_that(
    is.list(plots),
    length(plots) >= 1L,
    all(vapply(plots, rlang::is_string, logical(1)))
  )
  assertthat::assert_that(is.list(shared_ui))
  if (length(shared_ui) > 0L) {
    nms <- names(shared_ui)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort(
        "`shared_ui` must have unique non-empty names matching the `shared` annotations in `plots`."
      )
    }
    is_fn <- vapply(shared_ui, is.function, logical(1))
    if (!all(is_fn)) {
      rlang::abort(
        "Every entry of `shared_ui` must be a function `function(id) -> shiny.tag`."
      )
    }
  }

  shared_names <- names(shared_ui)
  plot_module_ids <- paste0("plot_", seq_along(plots))
  n_plots <- length(plots)
  col_width <- max(1L, 12L %/% n_plots)
  draw_all_id <- "ptr_grid_draw_all"

  shared_panel <- if (length(shared_ui) > 0L) {
    shiny::wellPanel(
      do.call(
        shiny::tagList,
        c(
          lapply(shared_names, function(nm) shared_ui[[nm]](nm)),
          list(shiny::actionButton(draw_all_id, draw_all_label))
        )
      )
    )
  } else {
    shiny::wellPanel(shiny::actionButton(draw_all_id, draw_all_label))
  }

  plot_columns <- do.call(
    shiny::fluidRow,
    lapply(seq_along(plots), function(i) {
      shiny::column(
        width = col_width,
        ptr_module_ui(plot_module_ids[[i]], plots[[i]], expr_check = expr_check)
      )
    })
  )

  ui <- shiny::fluidPage(
    shiny::titlePanel(title),
    shared_panel,
    plot_columns
  )

  force(plots)
  force(envir)
  force(expr_check)

  server <- function(input, output, session) {
    shared_reactives <- if (length(shared_names) > 0L) {
      stats::setNames(
        lapply(shared_names, function(nm) shiny::reactive(input[[nm]])),
        shared_names
      )
    } else {
      list()
    }
    draw_all_trigger <- shiny::reactive(input[[draw_all_id]])

    for (i in seq_along(plots)) {
      local({
        idx <- i
        ptr_module_server(
          plot_module_ids[[idx]],
          formula = plots[[idx]],
          envir = envir,
          expr_check = expr_check,
          shared = shared_reactives,
          draw_trigger = draw_all_trigger
        )
      })
    }
  }

  list(ui = ui, server = server)
}
