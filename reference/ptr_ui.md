# Self-contained UI for a `ggpaintr` Formula

The L2 default-layout UI bundle for a `ggpaintr` formula: owns its own
`.ptr-app` theme scope + asset bundle (nothing else to remember) and is
namespaced by `id`. Pair with the single public
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).
For a hand-composed L3 layout, use the bare `ptr_ui_*` pieces instead
and pair them with the same
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

## Usage

``` r
ptr_ui(
  formula,
  id = NULL,
  ui_text = NULL,
  checkbox_defaults = NULL,
  expr_check = TRUE,
  css = NULL,
  shared = NULL
)
```

## Arguments

- formula:

  A single formula string with `ggpaintr` placeholders.

- id:

  Optional module id; the namespace prefix for inputs and outputs.
  Defaults to `NULL` (identity namespace, single-instance use).

- ui_text:

  Optional named list of copy overrides; see
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  for the full schema and current defaults.

- checkbox_defaults:

  Optional named list of initial checked states.

- expr_check:

  Controls `expr` placeholder validation: `TRUE` (default) applies the
  built-in denylist + AST walker; `FALSE` disables all validation; a
  `list` with `deny_list`/`allow_list` entries customises the policy.
  See
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md).

- css:

  Optional character vector of paths to additional CSS files; linked
  after `ggpaintr`'s bundled stylesheet so its rules win. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  for the full semantics. Defaults to `NULL`.

- shared:

  Optional coordinator object from
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  for the multi-instance embedding. Forwarded verbatim to
  [`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md).
  When `NULL` (the single-instance default) the inline "Shared controls"
  section renders **every** `shared = "..."` placeholder in `formula`.
  When a `ptr_shared_spec` is supplied, its cross-formula keys
  (`shared$panel_keys`) are excluded here because they belong to the one
  standalone
  [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md);
  only this formula's formula-local shared keys render inline. Defaults
  to `NULL`.

## Value

A `shiny.tag` — a `fluidPage` shell containing the controls panel, plot
output, and asset bundle.

## See also

[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
[`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md)
for the `css =` argument and themable CSS custom properties.

## Examples

``` r
ui <- ptr_ui(
  "ggplot(mtcars, aes(x = var, y = var)) + geom_point()",
  "plot1"
)
```
