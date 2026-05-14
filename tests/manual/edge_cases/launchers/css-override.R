# CSS edge case — user override wins, relative url() resolves.
# Passes `css = "<override.css>"`; sentinel rules paint body crimson with
# `background-image: url(./mark.svg)`. The test JS asserts:
#   (1) <link> for the user stylesheet sits AFTER the bundled ggpaintr stylesheet.
#   (2) getComputedStyle(body).backgroundColor === "rgb(220, 20, 60)"  (crimson).
#   (3) getComputedStyle(body).backgroundImage includes the served path to mark.svg.
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
          css = file.path(CSS_DIR, "override.css")),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
