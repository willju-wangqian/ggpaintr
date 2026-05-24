pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-4-user-css-safety-adversarial: ADR 0013 §App-4 user_css + safety
# + adversarial pressure. Slot picks (per ADR §App-4 + PLAN-06):
#   ptr_app() + css = "user.css" · literal head(mtcars) · G5 string-builder
#   force-eval (paste0 + sprintf, both inside the closed whitelist at
#   R/paintr-app.R:260-262) · window code panel.
#
# Roster (per ADR §App-4 / PLAN-06):
#   ppVar  × 3  (aes x / y / color)
#   ppNum  × 4  (size / alpha / linewidth / span)
#   ppText × 2  (geom_smooth method / labs title)
#   ppExpr × 1  (labs subtitle — the adversarial probe target)
#   ppColor× 1  (custom value placeholder registered locally, validate_input
#                 rejects non-hex strings; wrapped in
#                 <div class="ptr-super4-colorpicker">)
# Total widgets ≈ 11. The user.css sibling declares two CSS rules: a
# colorpicker border-color rule and a sentinel rule whose presence on the
# page proves user_css reached the DOM (asserted on `head` innerHTML).
#
# Registering ppColor *in the child process* keeps the test process clean
# of cross-test contamination — see project memory `shinytest2-appdir-pkgload`.

ppColor <- ptr_define_placeholder_value(
  keyword = "ppColor",
  # `selected` is the canonical injection slot for the formula's positional
  # default (invoke_build_ui at R/paintr-build-ui.R:744-765); the formula
  # uses positionless ppColor() so `selected` arrives NULL and the build_ui
  # fallback "#3366FF" wins. The wrapping `<div class="ptr-super4-colorpicker">`
  # is the styling hook the user.css rule targets.
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
  default_arg = ptr_default_string(),
  validate_input = function(value, ctx) {
    if (is.character(value) && length(value) == 1L &&
        grepl("^#[0-9A-Fa-f]{6}$", value)) {
      TRUE
    } else {
      "must be #RRGGBB hex"
    }
  }
)

# G5 row: paste0 + sprintf are both inside the closed force-eval whitelist
# at R/paintr-app.R:260-262. `y_arg` is the LITERAL STRING "ppVar(wt)" — when
# paste0 assembles it, the resulting formula text contains `y = ppVar(wt)`
# which the parser turns into a real ppVar placeholder (this is the G5
# propagation demonstration: a string fragment becomes a widget).
y_arg <- "ppVar(wt)"
title_segment <- sprintf("Speed vs %s", "Weight")

ptr_app(
  paste0(
    "ggplot(head(mtcars), aes(x = ppVar(mpg), y = ", y_arg,
    ", color = ppVar(cyl))) + ",
    "geom_point(size = ppNum(2), alpha = ppNum(0.7)) + ",
    "geom_smooth(method = ppText(\"lm\"), color = ppColor(\"#3366FF\"), ",
    "linewidth = ppNum(1), span = ppNum(0.75)) + ",
    "labs(title = ppText(\"", title_segment,
    "\"), subtitle = ppExpr(nrow(mtcars)))"
  ),
  css = "user.css"
)
