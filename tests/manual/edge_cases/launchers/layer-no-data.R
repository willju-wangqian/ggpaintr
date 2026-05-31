# Layer-switcher edge case — no-data layers render flat (no inner Data/Controls
# sub-tabs). Manual checklist "Layer switcher checks" item 4.
#
# Formula has a pipeline-bearing ggplot layer (its panel SHOULD show inner
# Data + Controls sub-tabs) and three flat layers (geom_point, labs, coord_*)
# whose panels should NOT have inner sub-tabs.
#
# Test asserts, per layer selected:
#   ggplot           → inner sub-tabs Data, Controls exist
#   geom_point       → flat (no inner sub-tabs)
#   labs             → flat
#   coord_cartesian  → flat
source("tests/manual/edge_cases/launchers/_setup.R")
shiny::runApp(
  ptr_app(
    "mtcars |> head(ppNum) |> ggplot(aes(x = ppVar, y = ppVar)) +
       geom_point(size = ppNum) + labs(title = ppText) + coord_cartesian()"
  ),
  port = PORT, host = "127.0.0.1", launch.browser = FALSE
)
