#' Build a ggpaintr Shiny App with a bslib Theme
#'
#' A themed variant of `ptr_app()` that lays out the generated controls and
#' outputs inside a [bslib::page_sidebar()] shell with [bslib::card()]
#' containers. Builds per-layer panels via the typed-AST UI dispatch and
#' wires the new `ptr_server` end-to-end.
#'
#' Requires the `bslib` package. Install it with `install.packages("bslib")`.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building
#'   the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes.
#' @param expr_check Controls `expr` placeholder validation.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Defaults to `character()`.
#' @param theme A `bslib` theme object. Defaults to a Bootstrap 5 Flatly
#'   bootswatch. Pass any [bslib::bs_theme()] result to customize.
#' @param title App title shown in the page header.
#' @param ns An optional namespace function (`character -> character`).
#'
#' @return A `shiny.appobj`.
#' @examples
#' if (interactive()) {
#' ptr_app_bslib(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' }
#' @export
ptr_app_bslib <- function(formula,
                          envir = parent.frame(),
                          ui_text = NULL,
                          checkbox_defaults = NULL,
                          expr_check = TRUE,
                          safe_to_remove = character(),
                          theme = NULL,
                          title = "ggpaintr",
                          ns = shiny::NS(NULL)) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    rlang::abort(
      "Package 'bslib' is required for ptr_app_bslib(). Install it with install.packages(\"bslib\")."
    )
  }
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)

  if (is.null(theme)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")
  }
  if (!is.null(ui_text$shell$title$label)) {
    title <- ui_text$shell$title$label
  }

  tree <- ptr_translate(formula, expr_check = expr_check)
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

  ui <- bslib::page_sidebar(
    ptr_layer_assets(),
    title = title,
    theme = theme,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = 340,
      picker,
      hidden_tabset,
      shiny::actionButton(
        ns("ptr_update_plot"),
        label = shell_copy$update_plot_label %||% "Update plot"
      )
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Plot"),
      bslib::card_body(
        shiny::plotOutput(ns("ptr_plot")),
        shiny::uiOutput(ns("ptr_error"))
      )
    ),
    bslib::card(
      bslib::card_header("Generated code"),
      bslib::card_body(
        shiny::verbatimTextOutput(ns("ptr_code"))
      )
    )
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
    invisible(NULL)
  }

  shiny::shinyApp(ui = ui, server = server)
}
