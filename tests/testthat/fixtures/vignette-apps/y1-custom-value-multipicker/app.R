# Boot scaffolding (NOT vignette code): Y1 regression fixture.
#
# The consumer-path deselect bug surfaces visibly only because ppVar
# uses pickerInput(multiple = TRUE), whose deselect transport sends an
# empty value that Shiny converts to NULL on the server (verified via
# probe — see test-consumer-deselect-stays-empty.R). Built-in value
# placeholders (ppText/ppNum/ppExpr) use widgets that emit "" / NA on
# clear, length 1, so the bug never surfaced on the value path in
# practice -- but a *custom* value placeholder backed by the same
# pickerInput shape WOULD hit the same bug. This fixture registers
# such a placeholder ("ppMultiPick") and the test confirms the value-
# path fix (closure-flag at R/paintr-server.R:1475) covers it.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_define_placeholder_value(
  keyword = "ppMultiPick",
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    choices <- c("a", "b", "c", "d")
    retained <- intersect(selected %||% character(0), choices)
    shinyWidgets::pickerInput(
      inputId  = node$id,
      label    = label %||% "Pick",
      choices  = choices,
      selected = retained,
      multiple = TRUE,
      options  = shinyWidgets::pickerOptions(maxOptions = 1L)
    )
  },
  resolve_expr = function(value, ...) {
    if (length(value) == 0L) return(NULL)
    value[1L]
  },
  parse_positional_arg = ptr_arg_string()
)

ptr_app(
  'ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(cyl))) +
     geom_smooth(method = ppMultiPick("a"))'
)
