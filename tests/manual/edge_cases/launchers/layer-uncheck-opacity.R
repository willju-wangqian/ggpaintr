# Layer-switcher edge case — disabled-layer visual state.
# Manual checklist "Layer switcher checks" item 7: when a layer's `Include
# this layer in the plot` checkbox is unchecked, the layer panel content
# visually grays out (computed opacity ≈ 0.5) while inputs stay clickable.
# The layer drops from the rendered plot and from the generated code.
#
# Formula has two layers, both initially included. The test:
#   1. Switch layer picker to geom_smooth.
#   2. Uncheck geom_smooth_checkbox.
#   3. Read getComputedStyle(<panel>).opacity → expect "0.5" or similar.
#   4. Click Render plot; assert outputCode no longer contains "geom_smooth".
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + geom_smooth(se = FALSE)"
  ),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
