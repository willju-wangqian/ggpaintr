pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-4-user-css-safety-adversarial (no-default variant): same formula
# structure as app.R but every `pp*(default)` is stripped of its positional
# default. Path-A pressure-test variant per ADR-0016. Notably exercises the
# `ppColor()` no-arg build_ui NULL-arrival path (the build_ui's fallback
# "#3366FF" surfaces because `selected` arrives NULL).

ppColor <- ptr_define_placeholder_value(
  keyword = "ppColor",
  build_ui = function(node, label = "Color", selected = NULL, ...) {
    v <- if (is.character(selected) && length(selected) == 1L && nzchar(selected)) {
      selected
    } else "#3366FF"
    shiny::tags$div(
      class = "ptr-super4-colorpicker",
      shiny::textInput(node$id, label, value = v, placeholder = "#RRGGBB")
    )
  },
  resolve_expr = function(value, ...) value,
  parse_positional_arg = ptr_arg_string(),
  validate_session_input = function(value, ctx) {
    if (is.character(value) && length(value) == 1L &&
        grepl("^#[0-9A-Fa-f]{6}$", value)) {
      TRUE
    } else {
      "must be #RRGGBB hex"
    }
  }
)

y_arg <- "ppVar()"
title_segment <- sprintf("Speed vs %s", "Weight")

ptr_app(
  paste0(
    "ggplot(head(mtcars), aes(x = ppVar(), y = ", y_arg,
    ", color = ppVar())) + ",
    "geom_point(size = ppNum(), alpha = ppNum()) + ",
    "geom_smooth(method = ppText(), color = ppColor(), ",
    "linewidth = ppNum(), span = ppNum()) + ",
    "labs(title = ppText(\"", title_segment,
    "\"), subtitle = ppExpr())"
  ),
  css = "user.css"
)
