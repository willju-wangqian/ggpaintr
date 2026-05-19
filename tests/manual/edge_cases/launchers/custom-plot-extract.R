# Custom integration — host-owned renderPlot that uses ptr_extract_plot().
# Manual checklist "Shiny integration checks" items 5–7.
#
# `ptr_extract_plot()` wraps `shiny::isolate()` and is therefore a
# non-reactive snapshot helper. To use it inside a host `renderPlot()`,
# take the reactive dependency on `state$runtime()` first, then extract.
#
# Test asserts:
#   - #user_plot img is present and rendered with width >0 after Update plot.
#   - The default code binder still updates.
source("tests/manual/edge_cases/launchers/_setup.R")

formula <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point(size = num)"

ui <- fluidPage(
  ptr_module_ui(formula, "m"),
  plotOutput("user_plot", height = "300px")
)
server <- function(input, output, session) {
  state <- ptr_module_server(formula, "m")
  output$user_plot <- renderPlot({
    res <- state$runtime()        # take the reactive dependency
    req(isTRUE(res$ok))
    p <- ptr_extract_plot(state)  # non-reactive snapshot extraction
    req(!is.null(p))
    p + ggplot2::theme_classic() + ggplot2::labs(caption = "host-owned renderer")
  })
}
shiny::runApp(shinyApp(ui, server),
              port = PORT, host = "127.0.0.1", launch.browser = FALSE)
