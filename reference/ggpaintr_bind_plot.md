# Bind Default Plot Rendering into a Shiny App

Bind the same default
[`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) behavior
used by
[`ggpaintr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_server.md).
Advanced users who want to transform the plot should instead write their
own [`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) and
call
[`ggpaintr_plot_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_plot_value.md).

## Usage

``` r
ggpaintr_bind_plot(output, paintr_state, ids = paintr_state$ids)
```

## Arguments

- output:

  A Shiny `output` object.

- paintr_state:

  A `ggpaintr_state` object.

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

## Value

Invisibly returns `paintr_state`.
