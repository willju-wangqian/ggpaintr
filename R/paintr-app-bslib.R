#' Build a ggpaintr Shiny App with a bslib Theme
#'
#' A themed variant of [ptr_app()] that lays out the generated controls and
#' outputs inside a [bslib::page_sidebar()] shell with [bslib::card()]
#' containers. Intended as a worked example of how to reskin `ggpaintr` using
#' only its public API: the wrapper calls [ptr_build_ids()], [ptr_input_ui()],
#' and [ptr_server()] and composes outputs from the id contract with plain
#' `shiny` primitives — no internal helpers are touched.
#'
#' Requires the `bslib` package. Install it with `install.packages("bslib")`.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building
#'   the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders.
#' @param placeholders Optional custom placeholder definitions or an existing
#'   placeholder registry.
#' @param expr_check Controls `expr` placeholder validation. See [ptr_app()].
#' @param theme A `bslib` theme object. Defaults to a Bootstrap 5 Flatly
#'   bootswatch. Pass any [bslib::bs_theme()] result to customize.
#' @param title App title shown in the page header.
#'
#' @return A `shiny.appobj`.
#' @examples
#' \dontrun{
#' ptr_app_bslib(
#'   "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
#' )
#' }
#' @export
ptr_app_bslib <- function(formula,
                          envir = parent.frame(),
                          ui_text = NULL,
                          placeholders = NULL,
                          expr_check = TRUE,
                          theme = NULL,
                          title = "ggpaintr") {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    rlang::abort(
      "Package 'bslib' is required for ptr_app_bslib(). Install it with install.packages(\"bslib\")."
    )
  }

  ids <- ptr_build_ids()
  if (is.null(theme)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")
  }

  if (!is.null(ui_text$shell$title$label)) {
    title <- ui_text$shell$title$label
  }

  ui <- bslib::page_sidebar(
    title = title,
    theme = theme,
    sidebar = bslib::sidebar(
      title = "Controls",
      width = 340,
      ptr_input_ui(ids = ids, ui_text = ui_text)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header("Plot"),
      bslib::card_body(
        shiny::plotOutput(ids$plot_output),
        shiny::uiOutput(ids$error_output)
      )
    ),
    bslib::card(
      bslib::card_header("Generated code"),
      bslib::card_body(
        shiny::verbatimTextOutput(ids$code_output)
      )
    )
  )

  server <- function(input, output, session) {
    ptr_server(
      input,
      output,
      session,
      formula,
      envir = envir,
      ui_text = ui_text,
      placeholders = placeholders,
      ids = ids,
      expr_check = expr_check
    )
    invisible(NULL)
  }

  shiny::shinyApp(ui = ui, server = server)
}
