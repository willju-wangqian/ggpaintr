# Construct the ggpaintr runtime state container

Builds the `ptr_state` object — the translated typed AST (as a
`reactiveVal`), the runtime result, the per-layer resolved-data caches,
the eval environment, the input-snapshot machinery, and the shared
bindings / draw trigger — used by
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
and the advanced-embedder accessors
([`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
/
[`ptr_extract_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
/
[`ptr_extract_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md),
[`ptr_gg_extra()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_gg_extra.md)).

## Usage

``` r
ptr_init_state(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  shared = list(),
  draw_trigger = NULL,
  producer_debounce_ms = NULL,
  ns = shiny::NS(NULL),
  server_ns = ns,
  auto_bind_shared = FALSE,
  shared_resolutions = list(),
  shared_stage_enabled = list(),
  panel_sources = list(),
  plots = NULL
)
```

## Arguments

- formula:

  A single formula string with `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects.

- ui_text:

  Optional named list of copy overrides; see
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  for the full schema and current defaults.

- expr_check:

  Controls formula-level `ppExpr` validation: `TRUE` (default) applies
  the built-in denylist + AST walker; `FALSE` disables formula-level
  validation; a `list` with `deny_list`/`allow_list` entries customises
  the formula-level policy. Runtime-typed `ppExpr` input is always
  screened against the built-in denylist regardless. See
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md).

- safe_to_remove:

  Character vector of additional function names whose zero-argument
  calls should be dropped after substitution.

- shared:

  Named list of reactives (one per shared key) supplied by an outer
  wrapper such as
  [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md).
  Defaults to [`list()`](https://rdrr.io/r/base/list.html).

- draw_trigger:

  Optional reactive carrying a click counter — a numeric scalar that is
  `>= 1` once its button has been clicked (e.g. the grid app's "Draw
  all" `input$...` value). A redraw fires only when the carried value
  looks clicked; a reactive carrying any other value (a data frame, a
  timestamp) invalidates the runtime observer but never triggers a draw.
  To redraw on arbitrary reactive changes (e.g. a reactive pipeline head
  in the formula), use `ptr_options(gate_draw = FALSE)` live mode
  instead. Defaults to `NULL`.

- producer_debounce_ms:

  Optional. Controls the debounce window applied to producer-style
  placeholder inputs (`ppText`, `ppNum`, `ppExpr`) before they
  invalidate downstream consumer caches. `NULL` (default) enables auto
  mode: window starts at 0 ms and the runtime flips to 300 ms after
  three consecutive upstream resolutions exceed 150 ms (and back to 0
  after five consecutive resolutions under 80 ms). Pass `0` to force off
  forever, or a positive integer to pin a manual window.

- ns:

  A namespace function used for rendered ids (UI side).

- server_ns:

  A namespace function used for server-side input lookups. Defaults to
  `ns`.

- auto_bind_shared:

  If `TRUE`, the host (single-plot
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  or
  [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
  auto-render path) binds shared widgets at host scope. Relaxes the
  "missing-from-bindings" check in `ptr_validate_shared_bindings()` (the
  host auto-binds instead).

- shared_resolutions:

  Named list (keyed by raw shared key) of host-computed resolutions for
  shared data-consumer (`ppVar`) widgets, as returned by
  `ptr_resolve_shared_consumers()`. When an entry is present, the
  runtime validates that key's selection against the host-resolved
  upstream (the same data the host picker was built from) instead of the
  per-layer `node$upstream`, so a value valid in the host picker is
  never rejected by one layer's narrower upstream. Defaults to
  [`list()`](https://rdrr.io/r/base/list.html) (no host resolutions;
  per-layer behaviour).

- shared_stage_enabled:

  Named list (keyed by raw shared key) of reactives, each returning a
  logical, that toggle the orphan pipeline stages owned by that shared
  key (as carried in a
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
  bundle). A missing or unset entry leaves the stage enabled. Defaults
  to [`list()`](https://rdrr.io/r/base/list.html).

- panel_sources:

  Named list (keyed by source-node id) of reactives, each returning the
  host-loaded data for a panel-owned shared source (ADR 0023). Populated
  by the host's `ptr_setup_panel_sources()` and threaded through a
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
  bundle so per-instance binders read the host's primed data
  (`state$panel_sources[[node$id]]`) instead of re-wiring their own
  source UI. Defaults to [`list()`](https://rdrr.io/r/base/list.html)
  (single-plot / per-instance context — no panel-owned sources).

- plots:

  Optional list of formula strings for grid contexts. When supplied
  (typically by
  [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)),
  the validator for `shared` bindings cross-checks shared-key references
  against every plot's placeholder set, so a `shared = "..."` annotation
  that exists in plot 2 but not plot 1 still validates. Defaults to
  `NULL` (single- plot context — only `formula` is checked).

## Value

A `ptr_state` list (S3 class `c("ptr_state", "list")`).

## Details

This is a *state container*, not a from-scratch reactive-app builder: it
allocates the reactives but does not attach the pipeline / runtime
observers (those live in internal `ptr_setup_*` helpers wired by
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).
Reach for `ptr_init_state()` directly when you want to drive the typed
tree programmatically or exercise ggpaintr under
[`shiny::testServer()`](https://rdrr.io/pkg/shiny/man/testServer.html);
for a fully wired app use
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

## Examples

``` r
shiny::isolate({
  state <- ptr_init_state(
    "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  )
  is.list(state)
})
#> [1] TRUE
```
