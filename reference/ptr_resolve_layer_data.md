# Resolve the Dataset for a ggpaintr Layer

Return the active data frame for a parsed layer (e.g. `"ggplot"`,
`"geom_point"`). Designed to be called from a custom placeholder's
`bind_ui()` callback when the widget needs to know columns or values
from the dataset that the user is currently plotting.

## Usage

``` r
ptr_resolve_layer_data(ptr_obj, layer_name, input, context, eval_env)
```

## Arguments

- ptr_obj:

  A `ptr_obj` (available as `context$ptr_obj` inside a `bind_ui()`
  callback).

- layer_name:

  Layer name as a string. Use `meta$layer_name` to scope to the meta
  currently being bound, or `"ggplot"` for the base layer.

- input:

  A Shiny `input` reactive values object (the first argument of
  `bind_ui()`).

- context:

  The placeholder context (the fourth argument of `bind_ui()`).

- eval_env:

  An evaluation environment, typically `context$eval_env`.

## Value

A named list with components `has_data` (logical scalar) and `data` (the
resolved data frame, or `NULL`).

## Details

Resolution order:

1.  Look up the layer's `data` argument by parameter name. If the layer
    has a `data = <placeholder>` argument (e.g. `data = upload`),
    evaluate that placeholder to get the data frame.

2.  If the `data` argument is an unbound symbol, evaluate it in
    `eval_env`.

3.  Otherwise fall back to the first positional argument when it is a
    bare symbol (e.g. `ggplot(mtcars, aes(...))`). This heuristic does
    **not** handle non-data-first call shapes such as `merge(x, y)`; in
    those cases supply `data = ` explicitly in the formula.

Returns `list(has_data = FALSE, data = NULL)` if no dataset is
resolvable, so callers can early-return without rendering an empty
widget.

## See also

[`ptr_define_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder.md),
[`ptr_ns_id()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ns_id.md).
