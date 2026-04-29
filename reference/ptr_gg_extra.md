# Capture Out-of-Runtime ggplot Additions for the Code Output

Advanced helper for custom `renderPlot({...})` blocks inside embedded
ggpaintr apps. When an advanced developer adds ggplot components
(themes, scales, coords, guides, labs, ...) to the plot returned by
[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md),
those additions render visually but never reach the generated code
output. `ptr_gg_extra()` solves that by capturing the expressions it
receives and storing them on the `ptr_state` so the default code binder
([`ptr_register_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_code.md))
can surface them alongside the formula-driven code.

## Usage

``` r
ptr_gg_extra(ptr_state, ...)
```

## Arguments

- ptr_state:

  A `ptr_state` object created by
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md).

- ...:

  ggplot components (theme, scale, coord, guides, labs, ...). Captured
  with [`rlang::enquos()`](https://rlang.r-lib.org/reference/enquo.html)
  for code output, evaluated for the returned list.

## Value

A list of evaluated ggplot components, suitable for
`plot_obj + ptr_gg_extra(ptr_state, ...)`.

## Details

The helper is intentionally **not** exposed through
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
or
[`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md).
It is only meaningful when the caller owns their own `renderPlot({...})`
block built on
[`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
and the `ptr_register_*()` helpers.

Semantics:

- Replace-per-call. Each call overwrites the previously captured extras.
  Pass every component you want in a single call, e.g.
  `ptr_gg_extra(ps, theme_minimal(), scale_x_log10())`.

- Suppression on failure. When the underlying runtime reports
  `ok = FALSE`, the code binder ignores extras so stale values from a
  prior successful draw never surface during a failed draw.

- No new S3 class. The return value is a plain list, so
  `plot_obj + ptr_gg_extra(ps, ...)` dispatches through ggplot2's
  built-in `ggplot_add.list` method.

## Examples

``` r
if (interactive()) {
server <- function(input, output, session) {
  ps <- ptr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )
  ptr_setup_controls(input, output, ps)
  ptr_register_draw(input, ps)
  ptr_register_error(output, ps)
  ptr_register_code(output, ps)

  output$outputPlot <- shiny::renderPlot({
    plot_obj <- ptr_extract_plot(ps$runtime())
    if (is.null(plot_obj)) {
      graphics::plot.new()
      return(invisible(NULL))
    }
    plot_obj + ptr_gg_extra(ps, ggplot2::theme_minimal(base_size = 16))
  })
}
}
```
