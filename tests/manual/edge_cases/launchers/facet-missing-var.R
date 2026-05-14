# Edge case — facet_wrap(expr) typed with a missing column.
# Manual checklist "Core Shiny checks" item 6:
# Enter `~ Speciesasdf` in the facet input, click Render; expect the inline
# error channel to surface ggplot2's faceting error (not raw Shiny `[object Object]`).
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app("ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point() + facet_wrap(expr)"),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
