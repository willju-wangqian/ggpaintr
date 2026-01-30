# The inner module server that sets colors through `scale_fill_gradient`, `scale_fill_manual`, `scale_color_gradient`, `scale_color_manual`

The inner module server that sets colors through `scale_fill_gradient`,
`scale_fill_manual`, `scale_color_gradient`, `scale_color_manual`

## Usage

``` r
scaleColorHandler_inner(id, selected_colors, color_fill)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- selected_colors:

  the selected colors from input

- color_fill:

  string. either `color` or `fill`. Specifies whether it's `scale_color`
  or `scale_fill`

## Value

the return of `scale_<fill/color>_<gradient/manual>` and its code or
`NULL`
