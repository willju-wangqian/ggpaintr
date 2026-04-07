# Bind the Generated Control Panel into a Shiny App

Register the dynamic `var` controls and render the standard tabbed
control panel into the target
[`uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html).

## Usage

``` r
ggpaintr_bind_control_panel(
  input,
  output,
  paintr_state,
  ids = paintr_state$ids
)
```

## Arguments

- input:

  A Shiny `input` object.

- output:

  A Shiny `output` object.

- paintr_state:

  A `ggpaintr_state` object.

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

## Value

Invisibly returns `paintr_state`.
