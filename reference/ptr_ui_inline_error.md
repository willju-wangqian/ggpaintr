# Nest an Inline Error Slot in a Plot Piece

Output combinator: takes an already-built bare plot piece
([`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md))
and an already-built bare error piece
([`ptr_ui_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_error.md))
and returns the plot card with the error slot rendered **inline in the
card body** — the layout the bundled apps use. Pure DOM structure; no
server coupling (the server registers `ptr_plot` / `ptr_error`
regardless). Nestable inside
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md).

## Usage

``` r
ptr_ui_inline_error(plot, error)
```

## Arguments

- plot:

  A plot piece, typically `ptr_ui_plot(id)`. Must be the
  `.ptr-card--plot` card so the error can be appended to its body.

- error:

  An error piece, typically `ptr_ui_error(id)` built with the same `id`
  as `plot`.

## Value

A [shiny::tag](https://rdrr.io/pkg/shiny/man/reexports.html) — the plot
card with `error` nested in its body.

## Details

This combinator does **not** add the `.ptr-output` toggle scope (it has
no toggle, so it needs none);
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md)
owns that wrapper.

## See also

[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_error.md),
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md)

## Examples

``` r
ptr_ui_inline_error(ptr_ui_plot("p"), ptr_ui_error("p"))
#> <div class="ptr-card ptr-card--plot">
#>   <div class="ptr-card__head">
#>     <h3 class="ptr-card__title">Plot</h3>
#>   </div>
#>   <div class="ptr-card__body">
#>     <div class="shiny-plot-output html-fill-item" id="p-ptr_plot" style="width:100%;height:400px;"></div>
#>     <div id="p-ptr_error" class="shiny-html-output"></div>
#>   </div>
#> </div>
```
