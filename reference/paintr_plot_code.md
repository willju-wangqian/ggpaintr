# Generate plot and the corresponding code of `ggplot2` from a `paintr_obj`

Generate plot and the corresponding code of `ggplot2` from a
`paintr_obj`

## Usage

``` r
paintr_plot_code(
  paintr_obj,
  data_filter = "",
  selected_color_rctv = NULL,
  selected_fill_rctv = NULL,
  color_fill = FALSE,
  color_group = FALSE,
  userFUN = NULL,
  ...
)
```

## Arguments

- paintr_obj:

  a `paintr_obj`

- data_filter:

  data filtering code

- selected_color_rctv:

  reactive value returned by `scaleColor_build_reactivity` for color

- selected_fill_rctv:

  reactive value returned by `scaleColor_build_reactivity` for fill

- color_fill:

  bool; optional. Whether or not to use the same variable for both color
  and fill

- color_group:

  bool; optional. Whether or not to use the same variable for both color
  and group

- userFUN:

  a function that returns a named list, where the names of this named
  list are parameters (except for `mapping`) of `geom_<chart>`, and the
  elements of this list are arguments of the corresponding parameters

- ...:

  arguments that go into `userFUN`

## Value

a named list of two elements; plot and code

## Note

this function should be called inside
[`observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html)
since the [`isolate()`](https://rdrr.io/pkg/shiny/man/isolate.html)
scope provided by
[`observeEvent()`](https://rdrr.io/pkg/shiny/man/observeEvent.html) is
essential.
