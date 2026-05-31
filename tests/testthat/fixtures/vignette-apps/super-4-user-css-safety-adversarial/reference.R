# Path-B reference (ADR-0016): the naked-ggplot expression that app.R's
# paste0/sprintf string-builder assembles, evaluable as plain R outside
# `ptr_app()`. The string-builder's literal substitutions (`y_arg`,
# `title_segment`) are resolved at authoring time; ppColor's positional
# default ("#3366FF") feeds the identity runtime so the canonical formula
# is Path-B clean.
library(ggpaintr)
library(ggplot2)
library(shiny)

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
  positional_arg = ptr_arg_string(),
  validate_input = function(value, ctx) {
    if (is.character(value) && length(value) == 1L &&
        grepl("^#[0-9A-Fa-f]{6}$", value)) TRUE else "must be #RRGGBB hex"
  }
)

y_arg <- "ppVar(wt)"
title_segment <- sprintf("Speed vs %s", "Weight")
formula_text <- paste0(
  "ggplot(head(mtcars), aes(x = ppVar(mpg), y = ", y_arg,
  ", color = ppVar(cyl))) + ",
  "geom_point(size = ppNum(2), alpha = ppNum(0.7)) + ",
  "geom_smooth(method = ppText(\"lm\"), color = ppColor(\"#3366FF\"), ",
  "linewidth = ppNum(1), span = ppNum(0.75)) + ",
  "labs(title = ppText(\"", title_segment,
  "\"), subtitle = ppExpr(nrow(mtcars)))"
)

eval(parse(text = formula_text))
