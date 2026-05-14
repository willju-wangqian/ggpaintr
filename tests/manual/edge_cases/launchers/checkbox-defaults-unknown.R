# Edge case — `checkbox_defaults` names a layer the formula doesn't contain.
# Manual checklist "checkbox_defaults initial-state checks" item 4:
# expect a console warning listing `geom_typo` as unknown, and the app
# launches with all real layers checked. The test reads the launcher's stderr.
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
          checkbox_defaults = list(geom_typo = FALSE)),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
