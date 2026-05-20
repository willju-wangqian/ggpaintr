# Grid App: Multiple `ggpaintr` Plots With Shared Controls

Builds a fluid layout of N plot modules with a top-level `wellPanel` for
shared input widgets and a "Draw all" button that triggers a redraw
across every plot. Each plot's `shared = "..."` placeholders read from
the corresponding entry in `shared_ui` instead of rendering local
widgets.

## Usage

``` r
ptr_app_grid(
  plots,
  shared_ui = list(),
  envir = parent.frame(),
  ui_text = NULL,
  draw_all_label = "Draw all",
  expr_check = TRUE,
  css = NULL,
  ncol = NULL,
  nrow = NULL
)
```

## Arguments

- plots:

  A list of formula strings, one per plot.

- shared_ui:

  Named list mapping shared key → `function(id) -> shiny.tag` builder.
  Names must match the `shared = "..."` annotations used in `plots`.
  Pass [`list()`](https://rdrr.io/r/base/list.html) if there are no
  shared placeholders.

- envir:

  Environment used to resolve local data objects.

- ui_text:

  Optional named list of copy overrides. The page header reads
  `ui_text$shell$title$label`; defaults to `"ggpaintr grid"`. See
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  for the full schema.

- draw_all_label:

  Label for the draw-all action button.

- expr_check:

  Controls `expr` placeholder validation: `TRUE` (default) applies the
  built-in denylist + AST walker; `FALSE` disables all validation; a
  `list` with `deny_list`/`allow_list` entries customises the policy.
  See
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md).

- css:

  Optional character vector of paths to additional CSS files, linked
  once at the page level after `ggpaintr`'s bundled stylesheet so its
  rules win. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  for the full semantics. Defaults to `NULL`.

- ncol:

  Number of plot columns. Default `NULL` auto-computes from `nrow` or
  places all plots in one row.

- nrow:

  Number of plot rows. Default `NULL` auto-computes from `ncol`.

## Value

A `shiny.appobj`.

## Details

For the formula grammar (placeholder keywords, `shared = "<id>"`
annotation, empty-call cleanup), see
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).

## See also

[`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md)
for the `css =` argument and themable CSS custom properties.

## Examples

``` r
if (interactive()) {
  ptr_app_grid(
    plots = list(
      "ggplot(mtcars, aes(x = var, y = var)) + geom_point()",
      "ggplot(mtcars, aes(x = var)) + geom_histogram()"
    )
  )
}
```
