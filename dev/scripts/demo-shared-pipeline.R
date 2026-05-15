# Demo: ptr_app_grid() with a `shared` placeholder INSIDE a data pipeline.
# Two plots share one slider that controls how many rows of `mtcars` each
# plot draws, via `head(num(shared = 'n_rows'))` in the data pipeline.
#
# How to run:
#   setwd("/Users/willju/Research/ggpaintr")
#   devtools::load_all(".")
#   source("/tmp/demo-shared-pipeline.R")
#   shiny::runApp(app)            # opens the demo in a browser
#
# What to try (manual verification):
#   1. In each plot's control panel, click "Data" tab (in the layer picker).
#      You'll see a num input bound to the shared slider.
#   2. Move the top-level "Rows to show" slider.
#      → Both plots' "Update data" buttons should highlight as stale.
#   3. Click "Update data" on plot 1 only.
#      → Plot 1's stale highlight clears; plot 2 stays stale.
#   4. Click "Draw all" at the top.
#      → Plot 1 redraws with the NEW row count; plot 2 redraws with its
#        OLD cached row count (because its data cache wasn't refreshed).
#   5. Click "Update data" on plot 2, then "Draw all".
#      → Both plots now reflect the slider value.

devtools::load_all("/Users/willju/Research/ggpaintr")

app <- ptr_app_grid(
  plots = list(
    "mtcars |> head(num(shared = 'n_rows')) |>
       ggplot(aes(x = wt, y = mpg)) + geom_point()",
    "mtcars |> head(num(shared = 'n_rows')) |>
       ggplot(aes(x = hp, y = mpg)) + geom_point()"
  ),
  shared_ui = list(
    n_rows = function(id) {
      shiny::sliderInput(id, "Rows to show", min = 2, max = 32, value = 6, step = 1)
    }
  ),
  ui_text = list(shell = list(title = list(label = "ggpaintr grid: shared × data pipeline")))
)
