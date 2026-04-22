# Bind Default Plot Rendering into a Shiny App

Bind the same default `renderPlot()` behavior used by
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).
Advanced users who want to transform the plot should instead write their
own `renderPlot()` and call
[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md).

## Usage

``` r
ptr_register_plot(output, ptr_state)
```

## Arguments

- output:

  A Shiny `output` object.

- ptr_state:

  A `ptr_state` object.

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
