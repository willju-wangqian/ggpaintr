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
  id = NULL,
  ui_text = NULL,
  expr_check = TRUE,
  draw_all_label = "Draw all"
)
```

## Arguments

- formulas:

  A non-empty character vector or list, one element per embedded
  [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
  instance. Each element is either a formula **string** or a **quoted
  ggplot expression** (`rlang::expr(...)` / `quote(...)`); quoted
  expressions are deparsed to their source string, and the two forms are
  interchangeable (mixed lists are allowed). A built ggplot object (e.g.
  `g <- ggplot(...) + geom_point()` then passing `g`) is **not**
  accepted – its source is unrecoverable, so quote it with
  [`expr()`](https://rlang.r-lib.org/reference/expr.html) instead. Note:
  a native pipe `|>` inside a quoted expression is desugared by R's
  parser before capture, so it does not survive into the generated code
  panel (use `%>%` or a formula string to preserve it).

- id:

  Optional character scalar that namespaces every id this coordinator
  emits, so two or more coordinators can coexist in one Shiny session
  without colliding on shared `input` slots. When non-`NULL`, every id
  (`shared_<key>`, `shared_<key>_stage_enabled`, `ptr_shared_draw_all`,
  `ptr_shared_errors`, the stage-block DOM id, and each consumer
  `renderUI` container) is prefixed `<id>-` using Shiny
  [`NS()`](https://rdrr.io/pkg/shiny/man/NS.html)'s separator. The
  default `NULL` preserves today's flat ids byte-for-byte – single-panel
  apps need no change. Must be `NULL` or a non-empty string matching
  `^[A-Za-z][A-Za-z0-9_]*$`;
  [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
  [`ptr_ui_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_shared_panel.md),
  and
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
  all read it off `obj$id` – their signatures do not change.

- ui_text:

  Optional copy overrides forwarded to the auto-built widgets (see
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)'s
  `ui_text` argument).

- expr_check:

  Whether to validate `ppExpr` placeholders during formula translation.
  Default `TRUE`. Runtime-typed `ppExpr` input is always screened
  against the built-in denylist regardless.

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

## Removed `shared_ui`

`shared_ui` (a named list of `function(id) -> shiny.tag` builders) is no
longer supported. Its original intent was to let two placeholders that
share a key share the same UI settings. But each placeholder already
carries its own `build_ui` rule, and two `shared=` placeholders simply
reuse that one rule when their widget collapses to the panel. The
customisation therefore comes from the placeholder's `build_ui`
(`ptr_define_placeholder_*(build_ui = ...)`), not from a separate
`shared_ui` override — so the override was redundant, and for `var`
consumers it actively dropped the formula default and froze the column
list. To customise a shared widget, define a custom placeholder with the
`build_ui` you want and use it in the formula. Label-only divergence for
the shared widget still has its own channel (`node$shared_label`).

## See also

[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
[`ptr_ui_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_shared_panel.md),
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md),
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md).

## Examples

``` r
obj <- ptr_shared(c(
  "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
  "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
))
obj$panel_keys   # "x" — shared by both formulas
#> [1] "x"

# Quoted expressions work too, and may be mixed with strings:
obj2 <- ptr_shared(list(
  rlang::expr(ggplot(mtcars, aes(x = ppVar(shared = "x"), y = mpg)) + geom_point()),
  "ggplot(mtcars, aes(x = ppVar(shared = 'x'), y = hp)) + geom_line()"
))
obj2$panel_keys  # "x"
#> [1] "x"
```
