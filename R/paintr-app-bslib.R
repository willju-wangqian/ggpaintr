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
#' @param title App title shown in the page header. When `NULL`
#'   (the default), the title is taken from `ui_text$shell$title$label`
#'   if supplied, otherwise from a hardcoded fallback.
#'
#' @section Precedence:
#' If both `title` and `ui_text$shell$title$label` are supplied, the
#' explicit `title` argument wins. Pass `title = NULL` (or omit it) to
#' have the bslib navbar brand follow the same `ui_text` overrides the
#' non-bslib `ptr_app()` shell uses.
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
                          title = NULL) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    rlang::abort(
      "Package 'bslib' is required for ptr_app_bslib(). Install it with install.packages(\"bslib\")."
    )
  }
  ns <- shiny::NS(NULL)
  safe_to_remove <- validate_safe_to_remove(safe_to_remove)

  if (is.null(theme)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")
  }
  # Precedence: an explicit `title =` wins (this arg only exists on the
  # bslib path -- users who pass it have explicitly asked for that string
  # in the navbar brand). When absent, fall through to
  # `ui_text$shell$title$label` (the same source the non-bslib path
  # respects), then to a hardcoded default. Documented under
  # `@section Precedence` in the man page.
  if (is.null(title)) {
    title <- ui_text$shell$title$label %||% "ggpaintr"
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

  # Shared widgets (`var(shared = ...)`, `num(shared = ...)`, ...) live once
  # at the top of the sidebar, above the per-layer picker -- same contract as
  # `ptr_app()`'s shared section. Consumer placeholders emit a `uiOutput`
  # container here; their picker is rendered server-side by
  # `ptr_bind_shared_consumer_uis()` (wired via `ptr_make_app_server()`).
  shared_widgets <- drop_null(lapply(
    collect_shared_placeholders(tree),
    function(e) build_ui_for(e$node, ui_text = ui_text, ns_fn = ns,
                             label_override = e$label_override)
  ))
  shared_section <- if (length(shared_widgets) > 0L) {
    shiny::tagList(
      shiny::tags$p(class = "ptr-shared-panel__title fw-bold mb-1",
                    "Shared controls"),
      shiny::tags$p(class = "ptr-shared-panel__hint text-muted small mb-2",
                    "One value here is reused everywhere it is referenced."),
      do.call(shiny::tagList, shared_widgets),
      shiny::tags$hr()
    )
  } else {
    NULL
  }

  sidebar <- do.call(bslib::sidebar, c(
    list(title = "Controls", width = 340),
    drop_null(list(
      shared_section,
      picker,
      hidden_tabset,
      shiny::actionButton(
        ns("ptr_update_plot"),
        label = shell_copy$update_plot_label %||% "Update plot"
      )
    ))
  ))

  ui <- bslib::page_sidebar(
    ptr_layer_assets(),
    title = title,
    theme = theme,
    sidebar = sidebar,
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

  server <- ptr_make_app_server(
    formula, tree,
    envir = envir, ui_text = ui_text,
    checkbox_defaults = checkbox_defaults, expr_check = expr_check,
    safe_to_remove = safe_to_remove, ns = ns
  )

  shiny::shinyApp(ui = ui, server = server)
}
