# Edge case — `data = unknown_object` (no such symbol in the formula's envir).
# Mirrors the manual checklist's "Missing-object checks" item:
# `ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()`.
# No `var` placeholders — so the missing object only matters when the plot
# actually tries to construct, not during UI build.
#
# Expected behavior:
#   - app launches successfully
#   - clicking Render plot surfaces an inline error mentioning `unknown_object`
#   - the plot pane stays empty
#   - generated code panel still renders (resolution succeeded; only construction failed)
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app("ggplot(data = unknown_object, aes(x = mpg, y = disp)) + geom_point()"),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
