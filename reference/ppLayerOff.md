# Off-by-default layer wrapper

ADR-0020 *structural* keyword that marks a layer as off-by-default in
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).
Inside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
the parser sees the wrapper and unwraps it at translate time, stamping
the boot-state metadata on the resulting node:
`ppLayerOff(layer_expr, hide = TRUE)` becomes a `ptr_layer` with
`default_active = FALSE`. The wrapper itself never appears in the typed
tree.

## Usage

``` r
ppLayerOff(layer_expr, hide = TRUE)
```

## Arguments

- layer_expr:

  A ggplot2 layer expression (e.g.
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html),
  `facet_wrap(~ cyl)`). Evaluated only when `hide = FALSE`.

- hide:

  A length-1 logical literal (`TRUE` or `FALSE`). In
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  formulas this MUST be a literal — the translator aborts on a
  non-literal so the formula remains the single source of truth for the
  app's boot state. Defaults to `TRUE`.

## Value

Outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md):
`NULL` when `hide = TRUE`, otherwise the evaluated `layer_expr`.

## Details

Outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
it behaves per its R semantics so naked-ggplot scripts still render:
`ppLayerOff(geom_point(), TRUE)` returns `NULL` (so
`ggplot(mtcars, aes(x = mpg, y = wt)) + ppLayerOff(geom_point(), TRUE)`
renders without the hidden layer); `ppLayerOff(geom_point(), FALSE)`
returns the layer.

For the pipeline-stage sibling that exposes a user-toggleable checkbox
(ADR-0021), see
[`ppVerbSwitch()`](https://willju-wangqian.github.io/ggpaintr/reference/ppVerbSwitch.md).

## Examples

``` r
library(ggplot2)
# Naked-R semantics: hide = TRUE drops the layer to NULL.
p1 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  ppLayerOff(geom_point(), TRUE)        # equivalent to no geom_point
p2 <- ggplot(mtcars, aes(x = mpg, y = wt)) +
  ppLayerOff(geom_point(), FALSE)       # the layer is added

# Inside ptr_app(), the wrapper becomes a node-level default and a
# boot-state-off checkbox:
# ptr_app(ggplot() + ppLayerOff(geom_point(aes(x = mpg, y = wt)), TRUE))
```
