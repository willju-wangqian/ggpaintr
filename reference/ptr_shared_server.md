# Server-Side Counterpart to the Shared Coordinator

Builds the shared input reactives and binds the host-level
`ppVar(shared = "...")` consumer pickers for the
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md).
Returns a `ptr_shared_state` that the embedder threads into each
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
via the `shared_state` argument.

## Usage

``` r
ptr_shared_server(
  obj,
  envir = parent.frame(),
  shared = list(),
  draw_trigger = NULL,
  spec = NULL
)
```

## Arguments

- obj:

  A `ptr_shared_spec` from
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  – the single source of truth for the cross-formula partition. Replaces
  the old `formulas`/`expr_check` arguments, which are now baked into
  `obj`.

- envir:

  Environment used to resolve symbols in the shared
  [`ppVar()`](https://willju-wangqian.github.io/ggpaintr/reference/pp_placeholders.md)
  upstream chains. Default
  [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html) picks up
  the embedder's caller scope so `mtcars` etc. resolve naturally.

- shared:

  Optional named list overriding the auto-derived reactives. Each named
  entry replaces the reactive for that key; unsupplied keys keep the
  default that reads `input[[canonical_shared_id(key)]]`.

- draw_trigger:

  Optional reactive overriding the auto-derived "Draw all" trigger
  (`input$ptr_shared_draw_all`). Useful when an embedder wants to drive
  the cross-module redraw from their own button.

- spec:

  Optional named list of fully-qualified Shiny input id -\> value, used
  to override widget defaults at session boot for host-owned
  panel-shared widgets (`shared_<k>` for consumer / value placeholders,
  `shared_<k>_name` for panel-owned source companion textInputs).
  Multi-instance entrypoints like
  [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
  forward the same flat spec to every per-plot
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  AND to this host-scope server; per-plot servers prefix-filter by their
  own module namespace and drop un-namespaced ids, leaving this host
  apply path to claim them. See [ADR
  0012](https://willju-wangqian.github.io/ggpaintr/reference/dev/adr/0012-role-based-tree-and-ptr-spec.md).

## Value

A `ptr_shared_state` S3 object with public fields `shared`,
`draw_trigger`, `shared_resolutions`, and `shared_stage_enabled` (a
named list of reactives, one per shared key, indicating whether that
key's shared stage is active for each embedded module).

## Details

Namespacing is inherited from `obj$id`; supply it to
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md).

Reads its session via
[`shiny::getDefaultReactiveDomain()`](https://rdrr.io/pkg/shiny/man/domains.html)
– call it inside the top-level Shiny server function (or any reactive
context that inherits the session). Errors when called outside any
reactive domain.

## See also

[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md),
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).

## Examples

``` r
if (interactive()) {
  obj <- ptr_shared(c(
    "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
    "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
  ))
  shiny::shinyApp(
    ui = shiny::fluidPage(ptr_shared_panel(obj)),
    server = function(input, output, session) {
      ptr_shared_server(obj)
    }
  )
}
```
