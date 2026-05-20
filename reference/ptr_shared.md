# Build the Shared Coordinator for a Multi-Instance Embedding

Constructs a single pure, non-reactive coordinator object from the full
set of plot formulas. The coordinator computes the cross-formula
**partition** – a shared key used by exactly one formula renders in that
formula's inline shared section; a key used by two or more formulas
renders in the one standalone
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md).
Because every consumer derives its view from this one object, the UI and
server can never disagree about the partition.

## Usage

``` r
ptr_shared(
  formulas,
  shared_ui = list(),
  ui_text = NULL,
  expr_check = TRUE,
  draw_all_label = "Draw all"
)
```

## Arguments

- formulas:

  A character vector or list of formula strings, one per embedded
  [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
  instance.

- shared_ui:

  Named list of `function(id) -> shiny.tag` builders, one per shared key
  the embedder wants to customise. Unsupplied keys are auto-rendered
  from the first formula that mentions them.

- ui_text:

  Optional copy overrides forwarded to the auto-built widgets (see
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)'s
  `ui_text` argument).

- expr_check:

  Whether to validate `expr` placeholders during formula translation.
  Default `TRUE`.

- draw_all_label:

  Label for the "Draw all" button rendered in the panel when two or more
  formulas are supplied.

## Value

A `ptr_shared_spec` S3 object. Public fields: `panel_keys`
(cross-formula keys), `local_keys_by_formula` (per-formula inline keys).
Deterministic and idempotent for a given `formulas`.

## Details

This is strictly multi-instance API: with a single ggpaintr instance all
shared widgets auto-render inline and no coordinator is needed. The
coordinator is consumed by exactly three functions, each taking only the
object:
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
[`ptr_ui_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_shared_panel.md),
and
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md).

Errors when no formula declares a `shared = "..."` annotation – building
the coordinator is a declaration of intent.

## See also

[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
[`ptr_ui_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_shared_panel.md),
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md),
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md).

## Examples

``` r
obj <- ptr_shared(c(
  "ggplot(mtcars, aes(x = var(shared='x'), y = var)) + geom_point()",
  "ggplot(mtcars, aes(x = var(shared='x'), y = var)) + geom_bar()"
))
obj$panel_keys   # "x" — shared by both formulas
#> [1] "x"
```
