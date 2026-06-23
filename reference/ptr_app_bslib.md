# Bslib-Themed App: A Demonstration Wrapper

A small wrapper that composes the public ggpaintr primitives
([`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md),
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md))
inside a
[`bslib::page_sidebar()`](https://rstudio.github.io/bslib/reference/page_sidebar.html)
shell. Exported so users who want a quick `bslib`-themed app can call it
directly, but its primary purpose is to illustrate the wrapper pattern:
the entire source is short enough to copy and adapt for any other layout
or theme.

## Usage

``` r
ptr_app_bslib(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  theme = NULL,
  spec = NULL
)
```

## Arguments

- formula:

  A single formula string using `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects when building the app.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders. The app title is read from `ui_text$shell$title$label`;
  defaults to `"ggpaintr"`.

- expr_check:

  Controls formula-level `ppExpr` validation: `TRUE` (default) applies
  the built-in denylist + AST walker; `FALSE` disables formula-level
  validation; a `list` with `deny_list`/`allow_list` entries customises
  the formula-level policy. Runtime-typed `ppExpr` input is always
  screened against the built-in denylist regardless. See the safety
  chapter of the ggpaintr book (development-version docs):
  <https://willju-wangqian.github.io/ggpaintr-book/safety.html>.

- safe_to_remove:

  Character vector of additional function names whose zero-argument
  calls should be dropped after placeholder substitution leaves them
  empty. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html).

- theme:

  A `bslib` theme object. Defaults to a Bootstrap 5 Flatly bootswatch.
  Pass any
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
  result to customize. This is a `bslib` passthrough and is not part of
  the ggpaintr public surface the wrapper demonstrates — wrappers are
  free to expose downstream-library args like this in addition to
  whatever ggpaintr primitives they compose.

- spec:

  An optional named list of fully-qualified Shiny input id -\> value,
  used to override widget defaults at session boot.

  For the formula grammar (placeholder keywords, shared annotation,
  empty-call cleanup), see
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).

## Value

A `shiny.appobj`.

## Details

For the recommended primary entry points, see
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
and
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md).
Requires the `bslib` package (`install.packages("bslib")`).

Single-formula `ppVar(shared = "...")` coordination is not supported on
this wrapper — the auto-built shared widgets
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
provides require an internal helper that is not part of the public API
today. Use
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
for that case, or
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
for multi-formula shared coordination (the shared widgets auto-render
from each placeholder's own `build_ui`).

## Examples

``` r
if (interactive()) {
ptr_app_bslib(
  "ggplot(data = mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
)
}
```
