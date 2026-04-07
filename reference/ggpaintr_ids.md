# Build Standard Output Ids for ggpaintr Integration

Create a validated set of top-level output and control ids for embedding
the `ggpaintr` runtime inside a larger Shiny app.

## Usage

``` r
ggpaintr_ids(
  control_panel = "controlPanel",
  draw_button = "draw",
  export_button = "shinyExport",
  plot_output = "outputPlot",
  error_output = "outputError",
  code_output = "outputCode"
)
```

## Arguments

- control_panel:

  Output id used for the generated control panel.

- draw_button:

  Input id used for the draw button.

- export_button:

  Output id used for the export download button.

- plot_output:

  Output id used for the plot output.

- error_output:

  Output id used for the inline error UI.

- code_output:

  Output id used for the generated code output.

## Value

An object of class `ggpaintr_ids`.

## Examples

``` r
ids <- ggpaintr_ids(
  control_panel = "custom_controls",
  draw_button = "run_plot"
)
ids$control_panel
#> [1] "custom_controls"
```
