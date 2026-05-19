# Custom integration — top-level ids via the module namespace.
# The post-rewrite custom-id mechanism is the module ns: pass a custom string
# into ptr_ui()/ptr_server() and every internal id is prefixed.
# (The pre-rewrite `ptr_build_ids()` helper is no longer exported; the
# manual checklist's wording predates the rewrite.)
#
# This launcher uses a deliberately non-default prefix ("myprefix_42") so the
# test can assert (a) the app boots, (b) every Shiny input id seen in the DOM
# starts with "myprefix_42-", and (c) the plot renders normally.
source("tests/manual/edge_cases/launchers/_setup.R")

NS_PREFIX <- "myprefix_42"
ui <- fluidPage(
  ptr_ui(NS_PREFIX, "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(size = num)")
)
server <- function(input, output, session) {
  ptr_server(NS_PREFIX, "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(size = num)")
}
shiny::runApp(shinyApp(ui, server),
              port = PORT, host = "127.0.0.1", launch.browser = FALSE)
