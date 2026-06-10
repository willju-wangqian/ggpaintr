# Grid App: Multiple `ggpaintr` Plots With Shared Controls

Builds a fluid layout of N plot modules with a top-level `wellPanel` for
shared input widgets and a "Draw all" button that triggers a redraw
across every plot. Each plot's `shared = "..."` placeholders collapse to
one widget in the top panel, rendered from the placeholder's own
`build_ui`.

## Usage

``` r
ptr_app_grid(
  plots,
  envir = parent.frame(),
  ui_text = NULL,
  draw_all_label = "Draw all",
  expr_check = TRUE,
  css = NULL,
  ncol = NULL,
  nrow = NULL,
  spec = NULL
)
```

## Arguments

- plots:

  A list of formula strings, one per plot.

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

  Controls formula-level `ppExpr` validation: `TRUE` (default) applies
  the built-in denylist + AST walker; `FALSE` disables formula-level
  validation; a `list` with `deny_list`/`allow_list` entries customises
  the formula-level policy. Runtime-typed `ppExpr` input is always
  screened against the built-in denylist regardless. See
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

- spec:

  An optional named list of fully-qualified Shiny input id -\> value,
  used to override widget defaults at session boot. The same flat spec
  is passed to every per-plot engine; each instance filters by its own
  namespace prefix. See [ADR
  0012](https://willju-wangqian.github.io/ggpaintr/reference/dev/adr/0012-role-based-tree-and-ptr-spec.md).

## Value

A `shiny.appobj`.

## Details

For the formula grammar (placeholder keywords, `shared = "<id>"`
annotation, empty-call cleanup), see
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).

## Removed `shared_ui`

The `shared_ui` argument is no longer supported (see
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
for the full rationale). To customise the widget a shared key renders,
define a custom placeholder with the `build_ui` you want
(`ptr_define_placeholder_*`) and use it in the formula — the shared
widget auto-renders from that `build_ui`.

## See also

[`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md)
for the `css =` argument and themable CSS custom properties.

## Examples

``` r
if (interactive()) {
  ptr_app_grid(
    plots = list(
      "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()",
      "ggplot(mtcars, aes(x = ppVar)) + geom_histogram()"
    )
  )
}
```
