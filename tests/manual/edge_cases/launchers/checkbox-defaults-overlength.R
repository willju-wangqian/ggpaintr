# Edge case — `checkbox_defaults` value vector longer than the number of
# matching layers. Manual checklist "checkbox_defaults initial-state checks"
# item 5: with two geom_point() layers but a length-3 default, expect a
# console warning about the extra value and the recognized prefix (first
# checked, second unchecked) applied at startup.
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + geom_point(color = 'red')",
    checkbox_defaults = list(geom_point = c(TRUE, FALSE, TRUE))
  ),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
