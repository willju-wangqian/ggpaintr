# Add `ggplot2` Layers Programmatically

Evaluate one or more `ggplot2` expressions and attach the results as
"extras" on the state. Extras are folded into the plot during the next
runtime cycle when `state$runtime()$ok` is `TRUE`. Eval failures leave
the existing extras untouched.

## Usage

``` r
ptr_gg_extra(state, ...)
```

## Arguments

- state:

  A `ptr_state` from
  [`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md).

- ...:

  `ggplot2` layer expressions (e.g.
  `ptr_gg_extra(state, ggplot2::scale_x_log10(), theme_minimal())`).
  Captured unevaluated and stored as quosures, then evaluated in
  `state$eval_env`. Eval errors propagate and leave the existing extras
  untouched (atomic update).

## Value

`state`, invisibly.

## Examples

``` r
shiny::isolate({
  state <- ptr_init_state(
    "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"
  )
  state <- ptr_gg_extra(state, ggplot2::theme_minimal())
  ptr_extract_code(state)
})
#> [1] ""
```
