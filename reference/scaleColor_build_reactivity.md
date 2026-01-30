# Build reactivity for `scale_color` or `scale_fill`

Build reactivity for `scale_color` or `scale_fill`

## Usage

``` r
scaleColor_build_reactivity(id, paintr_rctv, color_or_fill)
```

## Arguments

- id:

  An ID string that corresponds with the ID used for all component of
  this `paintr_obj`

- paintr_rctv:

  a reactive value of a `paintr_obj` object created by
  [`paintr()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr.md)

- color_or_fill:

  string. either `color` or `fill`. Specifies whether it's `scale_color`
  or `scale_fill`

## Value

a reactive object that is passed into
[`paintr_plot_code()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_plot_code.md)
