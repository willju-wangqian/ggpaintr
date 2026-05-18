#' Bslib-Themed App: A Demonstration Wrapper
#'
#' A small wrapper that composes the public ggpaintr primitives
#' ([ptr_ui_controls()], [ptr_ui_plot()], [ptr_ui_toggle_code()],
#' [ptr_module_server()]) inside a [bslib::page_sidebar()] shell. Exported
#' so users who want a quick
#' `bslib`-themed app can call it directly, but its primary purpose is to
#' illustrate the wrapper pattern documented in
#' `vignette("ggpaintr-customization")` § "Writing your own wrapper" — the
#' entire source is short enough to copy and adapt for any other layout or
#' theme.
#'
#' For the recommended primary entry points, see [ptr_app()] and
#' [ptr_app_grid()]. Requires the `bslib` package
#' (`install.packages("bslib")`).
#'
#' Single-formula `var(shared = "...")` coordination is not supported on this
#' wrapper — the auto-built shared widgets `ptr_app()` provides require an
#' internal helper that is not part of the public API today. Use [ptr_app()]
#' for that case, or [ptr_app_grid()] (with explicit `shared_ui =`
#' builders) for multi-formula shared coordination.
#'
#' @param formula A single formula string using `ggpaintr` placeholders.
#' @param envir Environment used to resolve local data objects when building
#'   the app.
#' @param ui_text Optional named list of copy overrides for UI labels, helper
#'   text, and placeholders. The app title is read from
#'   `ui_text$shell$title$label`; defaults to `"ggpaintr"`.
#' @param checkbox_defaults Optional named list of initial checked states for
#'   layer checkboxes.
#' @param expr_check Controls `expr` placeholder validation: `TRUE` (default)
#'   applies the built-in denylist + AST walker; `FALSE` disables all
#'   validation; a `list` with `deny_list`/`allow_list` entries customises
#'   the policy. See `vignette("ggpaintr-safety")`.
#' @param safe_to_remove Character vector of additional function names whose
#'   zero-argument calls should be dropped after placeholder substitution
#'   leaves them empty. Defaults to `character()`.
#' @param theme A `bslib` theme object. Defaults to a Bootstrap 5 Flatly
#'   bootswatch. Pass any [bslib::bs_theme()] result to customize. This is a
#'   `bslib` passthrough and is not part of the ggpaintr public surface the
#'   wrapper demonstrates — wrappers are free to expose downstream-library
#'   args like this in addition to whatever ggpaintr primitives they compose.
#'
#' For the formula grammar (placeholder keywords, shared annotation,
#' empty-call cleanup), see [ptr_app()].
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
                          theme = NULL) {
  if (!requireNamespace("bslib", quietly = TRUE)) {
    rlang::abort(
      "Package 'bslib' is required for ptr_app_bslib(). Install it with install.packages(\"bslib\")."
    )
  }
  if (is.null(theme)) {
    theme <- bslib::bs_theme(version = 5, bootswatch = "flatly")
  }
  id <- "ptr"
  title <- ptr_resolve_ui_text("title", ui_text = ui_text)$label %||%
    "ggpaintr"

  ui <- bslib::page_sidebar(
    title = title,
    theme = theme,
    sidebar = bslib::sidebar(
      title = "Controls",
      # ptr_controls_ui()/ptr_outputs_ui() were removed (ADR 0005 §5); the
      # wrapper now composes the public bare pieces + combinators directly.
      # The `.ptr-app` div restores the themed scope the bslib page chrome
      # does not provide, and ptr_ui_assets() ships the bundle (deduped
      # page-wide by htmlDependency).
      shiny::tags$div(
        class = "ptr-app",
        ptr_ui_assets(),
        ptr_ui_controls(
          id = id, formula = formula,
          ui_text = ui_text,
          checkbox_defaults = checkbox_defaults,
          expr_check = expr_check,
          shared = NULL
        )
      )
    ),
    bslib::card(
      full_screen = TRUE,
      shiny::tags$div(
        class = "ptr-app",
        ptr_ui_assets(),
        ptr_ui_toggle_code(
          ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)),
          ptr_ui_code(id)
        )
      )
    )
  )

  server <- function(input, output, session) {
    ptr_module_server(
      id,
      formula = formula,
      envir = envir,
      ui_text = ui_text,
      checkbox_defaults = checkbox_defaults,
      expr_check = expr_check,
      safe_to_remove = safe_to_remove
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
