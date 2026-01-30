# Module server that sets colors through `scale_fill_gradient`, `scale_fill_manual`, `scale_color_gradient`, `scale_color_manual`

Module server that sets colors through `scale_fill_gradient`,
`scale_fill_manual`, `scale_color_gradient`, `scale_color_manual`

## Usage

``` r
scaleColorFillHandler(id, selected_color_fill_rctv, color_fill)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- selected_color_fill_rctv:

  the reactive values returned by
  [`scaleColor_build_reactivity()`](https://willju-wangqian.github.io/ggpaintr/reference/scaleColor_build_reactivity.md)

- color_fill:

  string. either `color` or `fill`. Specifies whether it's `scale_color`
  or `scale_fill`

## Value

the return of `scale_<fill/color>_<gradient/manual>` and its code or
`NULL`

## Note

this function handles the `error` when `selected_color_fill_rctv()`
returns error
