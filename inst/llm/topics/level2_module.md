# Level 2 — the self-contained default-layout pair

Use when: the user already has a Shiny app and wants one block to hold a ggpaintr-driven plot, keeping ggpaintr's **default layout** (sidebar controls + plot/code/error) and giving up only the page chrome around it.

There is exactly one default-layout block at L2 — the pair `ptr_ui()` / `ptr_server()`. It is **self-contained**: it owns its own `.ptr-app` theme scope and bundled asset dependency, so it drops straight into a host page with no scaffolding (this is the bare-named `ptr_ui` / `ptr_server` half of the naming convention). There is **no** free-form controls-here / plot-there split at L2; placing controls and the plot in different regions, or replacing a built-in pane with your own renderer, is **L3** (see `level3_layout`, `level3_custom_render`).

## The embed — `ptr_ui()` + `ptr_server()`

`ptr_ui(formula, id = NULL, ...)` emits the default-layout block; `ptr_server(formula, id = NULL, ...)` wires it. Call `ptr_server()` directly inside your `server` function — it reads the active Shiny session, so you do **not** pass `input` / `output` / `session` to it. For a single instance the `id` is optional; pass one (and match it on both calls) only when you embed more than one block.

```r
library(shiny); library(ggpaintr)

formula <- "ggplot(iris, aes(ppVar, ppVar, color = ppVar)) + geom_point()"

ui <- fluidPage(
  titlePanel("My host app"),
  ptr_ui(formula)
)

server <- function(input, output, session) {
  ptr_server(formula)
}

shinyApp(ui, server)
```

`ptr_server()` returns the `ptr_state` from `ptr_init_state()`, so an embedder that wants to react to it (call `ptr_gg_extra()`, read `ptr_extract_*()`, render a second view) can capture it; otherwise the side effects are all that matter. `...` is forwarded to `ptr_init_state()` — so `ui_text =`, `expr_check =`, `safe_to_remove =`, `spec =`, and the shared-binding arguments are all accepted here too.

## Shared widgets: single instance vs. multiple instances

A `ppX(shared = "<key>")` placeholder is driven by one widget in lockstep wherever the key appears. *How* it surfaces is decided **per key by how many formulas reference it** — the partition rule:

> A shared key referenced in **exactly one** formula → that formula's inline **shared section** (inside its own control panel).
> A shared key referenced in **two or more** formulas → the one standalone **shared panel**.

The practical consequence is a hard split by instance count:

- **One ggpaintr instance** ⟹ **no coordinator, no panel, ever.** Every shared key is formula-local, so it auto-renders inline. The single-instance realization is L1 `ptr_app()` — write the annotation and you are done. `ptr_shared()` / `ptr_shared_panel()` / `ptr_shared_server()` are strictly multi-instance API and a single-instance embedder never sees them. This is enforced: `ptr_server()` given a `shared = "..."` formula with no coordinator wiring **errors on purpose**, telling you to build the coordinator.
- **Multiple instances** ⟹ you *must* build `obj <- ptr_shared(formulas = list(…))`. Cross-formula keys (≥2 formulas) go to the one `ptr_shared_panel(obj)`; each formula's formula-local keys still render inline in that module. Full recipe in `level2_shared`.

## `ptr_init_state()` and the advanced runtime arguments

`ptr_init_state(formula, ...)` is the state constructor that `ptr_server()` calls under the hood. It builds the `ptr_state` but does **not** wire inputs or start the runtime: `state$runtime()` stays `NULL` until `ptr_server()` installs the observers. So you do not drive a plot from `ptr_init_state()` alone — reach its advanced arguments by passing them through the `...` of `ptr_server()`, which forwards verbatim to it:

```r
server <- function(input, output, session) {
  state <- ptr_server(
    "ggplot(iris, aes(ppVar, ppVar)) + geom_point()",
    producer_debounce_ms = 300,                       # advanced ptr_init_state() arg, threaded through
    draw_trigger = shiny::reactive(input$my_button)
  )
  # state$runtime() / ptr_extract_*(state) are now usable; render your own panes off it.
}
```

`?ptr_init_state` documents every advanced argument: `shared` / `shared_resolutions` / `shared_stage_enabled` (the slots a `ptr_shared_server(obj)` bundle carries — passed for you via `shared_state =`, see `level2_shared`), `draw_trigger` (custom redraw reactive), `producer_debounce_ms` (data-source debouncing), and `auto_bind_shared` (the host-binds-shared-locally switch L1 flips internally for single-instance shared). All are accepted by `ptr_server()` through `...`.

## Output ids are package-owned

The `ptr_plot` / `ptr_error` / `ptr_code` / `ptr_update_plot` ids (plus internal `ptr_layer_select` / `ptr_layer_tabset` controls) are fixed under the module namespace; treat the `ptr_` prefix as reserved. If you need different output names — your own `plotOutput("my_chart")` — that is Level 3: read `state$runtime()$plot` (reactive) or `ptr_extract_plot(state)` (isolated) inside your own renderer (see `level3_custom_render`). Hand-placing each pane in its own region is also Level 3 (see `level3_layout`). For the full id grammar and the `ptr_` reserved set see `level2_custom_ids`; for relabelling every string see `level2_ui_text`.
