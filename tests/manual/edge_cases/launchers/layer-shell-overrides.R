# Layer-switcher edge case — three-key shell `ui_text` overrides.
# Manual checklist "Layer switcher checks" item 8: confirm that overriding
# `shell$layer_picker$label`, `shell$data_subtab$label`, and
# `shell$controls_subtab$label` rebrands those three controls while every
# OTHER copy slot (Update plot, layer-checkbox label) keeps the default.
#
# Test asserts (via DOM):
#   - The layer-picker label reads "Choose layer".
#   - The inner Data sub-tab nav reads "Pipeline".
#   - The inner Controls sub-tab nav reads "Aesthetics".
#   - The Render button still reads the default ("Update plot").
#   - The layer-include checkbox still reads its default text.
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app(
    "mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()",
    ui_text = list(
      shell = list(
        layer_picker    = list(label = "Choose layer"),
        data_subtab     = list(label = "Pipeline"),
        controls_subtab = list(label = "Aesthetics")
      )
    )
  ),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
