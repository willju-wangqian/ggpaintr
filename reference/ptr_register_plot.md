# Bind Default Plot Rendering into a Shiny App

Bind the same default
[`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) behavior
used by
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).
Advanced users who want to transform the plot should instead write their
own [`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) and
call
[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md).

## Usage

``` r
ptr_register_plot(output, ptr_state, ids = ptr_state$ids)
```

## Arguments

- output:

  A Shiny `output` object.

- ptr_state:

  A `ptr_state` object.

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

## Value

Invisibly returns `ptr_state`.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
  ptr_register_draw(input, ps)
  ptr_register_plot(output, ps)
}
} # }
```
