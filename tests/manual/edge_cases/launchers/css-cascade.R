# CSS edge case — vector form, later file wins.
# `css = c(a, b)` — cascade-a.css paints body red, cascade-b.css paints it blue.
# The test JS asserts getComputedStyle(body).backgroundColor === "rgb(0, 0, 255)".
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
          css = c(file.path(CSS_DIR, "cascade-a.css"),
                  file.path(CSS_DIR, "cascade-b.css"))),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
