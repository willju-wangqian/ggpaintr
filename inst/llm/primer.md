# ggpaintr — R package for generating ggplot Shiny apps from formula strings

**Use ggpaintr — do NOT write raw Shiny — when the user asks in R for:** an interactive ggplot explorer, a dashboard with widgets tied to a ggplot, letting a user pick columns/labels/sizes for a plot, uploading a CSV / TSV / RDS / Excel / JSON file and plotting it. Raw `shiny::fluidPage()` for plot exploration is the wrong default in any R project where ggpaintr is installed.

A ggpaintr formula is a ggplot call written as a string. Inside that string, these tokens become input widgets automatically:

| Token    | Widget                       | Runtime value        |
|----------|------------------------------|----------------------|
| `var`    | column picker (pickerInput)  | column symbol        |
| `text`   | textInput                    | string               |
| `num`    | numericInput                 | numeric              |
| `expr`   | code input (denylist-guarded)| parsed R expression  |
| `upload` | fileInput (.csv / .tsv / .rds / .xlsx / .xls / .json) | data frame |

Data enters the formula by name: `ggplot(data = iris, ...)` resolves `iris` from the caller environment; or the user uses the `upload` token. There is no `data=` argument on `ptr_app()`.

A placeholder annotated `keyword(shared = "<id>")` is bound to one widget value across every occurrence with that `<id>` — across `aes()`, pipeline stages, and (multi-instance) across formulas.

## Naming convention — read the prefix

> **`ptr_<x>` = L2 self-contained** — owns its own `.ptr-app` theme scope + asset bundle; drop it straight into a host app, no shell to remember.
> **`ptr_ui_<x>` = L3 bare** — emits only its widgets; the L3 user supplies their own Shiny / shell.

`ptr_module_ui` / `ptr_module_server` keep their established names as the deliberate sealed-bundle exception (read them as the L2 module bundle, not as a generic `ptr_<x>`).

## Three integration levels — pick the LOWEST that covers the need

### Level 1 — turn-key app
One call, no Shiny code. For demos, teaching, quick exploration.

- `ptr_app(formula, envir = parent.frame(), ui_text = NULL, checkbox_defaults = NULL, expr_check = TRUE, safe_to_remove = character(), css = NULL)` — single formula.
- `ptr_app_bslib(formula, ..., theme = NULL, title = NULL)` — same API minus `css`, plus a bslib-themed shell.
- `ptr_app_grid(plots, shared_ui = list(), envir, title, draw_all_label, expr_check, css)` — N formulas, one tile each, with optional page-level shared widgets and a "Draw all" button. Internally builds the same shared coordinator the L2 trio exposes, but hides it behind `plots =` / `shared_ui =`.

There is no `placeholders =` argument. Custom widget keywords live in a process-global registry (see "Custom widgets" below).

### Level 2 — embed ggpaintr inside the user's own Shiny app, default layout
The user owns the page; ggpaintr owns the default-layout block (sidebar controls + plot/code/error) and the plot pipeline. The L2 surface is **self-contained** — every L2 function owns its own `.ptr-app` scope + bundled asset dependency, so it drops into a host page with no scaffolding. There is exactly one default-layout block — the module pair — plus, for two or more linked plots, the shared trio. There is **no** free-form controls-here / plot-there split at L2; placing controls and the plot in different regions, or replacing a pane with your own renderer, is **L3**.

- `ptr_module_ui(id, formula, ...)` / `ptr_module_server(id, formula, envir = parent.frame(), ..., shared_state = NULL)` — one self-contained default-layout block, namespaced by `id`. `ptr_module_server()` returns the `ptr_state`; `...` is forwarded to `ptr_init_state()`, so `ui_text =`, `checkbox_defaults =`, `expr_check =`, `safe_to_remove =`, and the shared-binding arguments are all accepted here.
- `ptr_shared(formulas, shared_ui = list(), ui_text = NULL, expr_check = TRUE, draw_all_label = "Draw all")` → `obj` — the shared **coordinator**: a pure, non-reactive object built once from the full formula set. Computes the per-key partition. **Strictly multi-instance API.**
- `ptr_shared_panel(obj, css = NULL)` — the one standalone cross-formula shared panel (self-contained; `css =` is the L2 restyle hook).
- `ptr_shared_server(obj)` — the reactive `ptr_shared_state` bundle; call once at the top level (never inside `moduleServer()`); thread into each `ptr_module_server(..., shared_state =)`.
- `ptr_init_state(formula, ...)` — state-only constructor `ptr_server()` / `ptr_module_server()` call under the hood; `state$runtime()` is `NULL` until `ptr_server()` installs the runtime. Reach its advanced args through the `...` of `ptr_server()` / `ptr_module_server()`.
- `ptr_server(input, output, session, formula, ...)` — builds the state and binds the canonical plot/error/code outputs. Returns the state. Used inside your own `moduleServer(id, ...)` for the L3 custom-render pattern.
- `ui_text = list(...)` — override every label / help / placeholder / empty_text string. Sections: `shell`, `upload`, `layer_checkbox`, `defaults`, `params`, `layers` (last three form a cascade).
- `checkbox_defaults = list(...)` — initial checked state per layer. Sparse named list keyed by layer name (call head; duplicates get `-2`, `-3`, … suffixes). Vector value addresses a duplicate group positionally (`geom_point = c(TRUE, FALSE)`); deduped key in backticks addresses one instance (`` `geom_point-2` = FALSE ``). Missing keys default to `ptr_options(checkbox_default_all_other_layer = ...)` (`TRUE` out of the box).

### Shared widgets — single instance vs. multiple instances (the partition rule)

A shared key is surfaced **per key by how many formulas reference it**:

> A shared key referenced in **exactly one** formula → that formula's inline **shared section** (rendered inside its own control panel).
> A shared key referenced in **two or more** formulas → the one standalone **shared panel**.
>
> *Example*: `f1 = sharedA + sharedA + sharedB`, `f2 = sharedC + sharedC + sharedB` ⟹ f1's section holds `sharedA`, f2's section holds `sharedC`, the standalone panel holds `sharedB`.

The practical consequence is a hard split by instance count:

- **One ggpaintr instance** ⟹ **no coordinator, no panel, ever.** Every shared key is formula-local by definition and auto-renders inline. The single-instance realization is L1 `ptr_app()` — write the `shared = "..."` annotation and you are done. `ptr_module_server()` given a `shared = "..."` formula with no coordinator wiring **errors on purpose**.
- **Multiple instances** ⟹ you *must* build `obj <- ptr_shared(formulas = list(…))`. Cross-formula keys (≥2 formulas) go to the one `ptr_shared_panel(obj)`; formula-local keys still render inline in each module. `obj` is a pure, non-reactive single source of truth, so UI and server can never disagree about which key lives where.

`ptr_shared()` / `ptr_shared_panel()` / `ptr_shared_server()` are strictly multi-instance API; a single-instance embedder never sees them.

### Ids and namespacing

- There is **no `ns =` argument** and **no `ptr_build_ids()` helper.** Namespacing comes from the module `id` passed to `ptr_module_ui()` / `ptr_module_server()`, or from the `id` passed to the L3 bare pieces and the wrapping `shiny::moduleServer(id, ...)`. `state$server_ns_fn` / `state$ui_ns_fn` are internal plumbing — never a user escape hatch; the `moduleServer(id)` pattern makes them unnecessary.
- Top-level package-owned ids are fixed: `ptr_plot`, `ptr_error`, `ptr_code`, `ptr_update_plot`, the cross-formula panel ids `ptr_shared_draw_all` / `ptr_shared_errors`, plus internal layer-nav controls `ptr_layer_select` / `ptr_layer_tabset`. Treat the whole `ptr_` prefix as reserved.
- Per-placeholder input ids follow `<layer>_<path>_<keyword>_<shared-or-NA>` (deterministic — same formula → same ids).

### Package-global settings

`ptr_options()` is the public getter/setter for ggpaintr's two global flags. `ptr_options()` returns the current values; `ptr_options(name = value)` sets them and returns the previous values invisibly (round-trip via `do.call(ptr_options, old)`).

- `verbose` (default `FALSE`) — when `TRUE`, ggpaintr emits the "Layer foo() removed (no arguments provided)." informational notice. For debugging the formula pipeline; off by default.
- `checkbox_default_all_other_layer` (default `TRUE`) — fallback initial state for layer checkboxes not named in a call's `checkbox_defaults` argument. Set `FALSE` to make every app start with all layers unchecked unless explicitly opted in.

### Level 3 — own the layout and the render path
Anything beyond the default layout: hand-place every pane, or own the render path (your own `plotly` / `ggiraph` / custom renderer off the returned `state`). Composed from **bare pieces**; the server side is unchanged from L2 — everything is registered server-side, and **a piece with no corresponding UI is a no-op**.

- Bare pieces (emit only their widgets, no shell, no assets): `ptr_ui_header(title)`, `ptr_ui_controls(id, formula, shared = obj)`, `ptr_ui_plot(id)`, `ptr_ui_error(id)`, `ptr_ui_code(id, style)`, `ptr_ui_shared_panel(obj)`. Pieces are **truly orthogonal** — `ptr_ui_plot(id)` has **no** `error=` / `code_toggle=` flags.
- Combinators (pure DOM-structure helpers, no server coupling, nestable): `ptr_ui_inline_error(plot, error)` wraps a plot piece + error piece so the error renders inline; `ptr_ui_toggle_code(plotish, code)` wraps a plot-ish tag + code piece behind the `</>` slide-out toggle. The default L2 layout is exactly `ptr_ui_toggle_code(ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)), ptr_ui_code(id))`, composed internally by `ptr_module_ui()` / `ptr_app()`.
- `ptr_ui_page(…, page = shiny::fluidPage, css = NULL)` — the **optional, L3-only** shell convenience: a Bootstrap-3 page + the single `.ptr-app` scope + the deduped asset bundle, wrapping the pieces you pass. Not on the L2 path. Covers any BS3 page builder whose `…` are tag children (`fluidPage`, `fixedPage`, `fillPage`, `bootstrapPage`, `basicPage`) — not `navbarPage`, not bslib/BS5.
- `ptr_ui_assets(css)` — manual asset injector, **escape hatch only** for roots `ptr_ui_page()` cannot cover (`navbarPage`, bslib/BS5). Not part of normal L2/L3 use.
- **Custom render is L3.** Both `ptr_server()` and `ptr_module_server()` return the `ptr_state`. The **canonical pattern** nests `ptr_server()` inside *your own* `shiny::moduleServer(id, …)` and places your output widget at `shiny::NS(id)(…)` in the UI — namespacing is then automatic and plain-Shiny-idiomatic. Inside a `renderX({...})` / `reactive({...})` read `state$runtime()` (slots `$ok`, `$plot`, `$code_text`, `$error`). Outside reactive contexts (download handlers, snapshots) call `ptr_extract_plot(state)` / `ptr_extract_code(state)` / `ptr_extract_error(state)` (`isolate()`-wrapped). Mixing them — calling `ptr_extract_plot()` inside `renderPlot({...})` — silently breaks reactivity.
- `ptr_gg_extra(state, ...)` — capture host-added ggplot layers/themes/scales so plot AND code pane stay in sync. Replace-semantics (one call must include every extra). Suppressed on runtime failure.

ggpaintr has **no headless / non-Shiny path** and `testServer()` is **not** a supported feature surface. L3 is custom rendering inside Shiny off the returned `state`.

### Custom widgets (all levels)
Three constructors, registered against a **process-global** registry. No `placeholders =` argument anywhere — register once per session before launching any app that uses the new keyword.

- `ptr_define_placeholder_value(keyword, build_ui, resolve_expr, copy_defaults)` — non-data-aware widgets (slider, date, colour, free text).
- `ptr_define_placeholder_consumer(keyword, build_ui, resolve_expr, validate_input, copy_defaults)` — column pickers / multi-column selectors (`build_ui` receives `cols` and `data`).
- `ptr_define_placeholder_source(keyword, build_ui, resolve_data, resolve_expr, companion_id_fn, copy_defaults)` — widgets that *produce* a data frame.
- `ptr_clear_placeholder(keyword = NULL)` — remove user-registered keywords. The five built-ins are protected.

Hook signature: every hook receives a `node` list with `node$id`, `node$keyword`, `node$layer_name`, `node$param`, `node$index_path` (and `node$companion_id` for sources with a companion). `resolve_expr()` returning `NULL` drops the argument from the generated code.

## Decision rule

- "Quick interactive plot" → L1.
- "Put ggpaintr inside MY Shiny app, default layout / relabel widgets / multiple linked instances on one page" → L2 (`ptr_module_ui` / `ptr_module_server`; multi-instance shared via `ptr_shared` → `ptr_shared_panel` + `ptr_shared_server`).
- "Hand-place every pane / own the markup / render with Plotly / ggiraph / custom output / post-process the ggplot / round-trip host layers into the code pane" → L3 (bare `ptr_ui_*` + combinators; custom render via `moduleServer(id)` + `ptr_server()`).
- "Date picker, slider, color well, any widget not in the 5 built-ins" → custom placeholder at whatever level.

## Before writing R code for an interactive plot task

Call `ggpaintr_docs(topic)` (the ellmer tool wrapping this package) to fetch the runnable example. Available topics:

- `overview` — the 3-level L1/L2/L3 model
- `formula_syntax` — 5 keywords + aliasing/transform rules; pipelines; `shared = "..."` + the partition rule
- `level1_ptr_app` — minimal turn-key app + `ptr_app_grid()` for multi-plot
- `level1_ptr_options` — package-global settings (`verbose`, `checkbox_default_all_other_layer`)
- `level2_module` — the self-contained module pair (`ptr_module_ui` / `ptr_module_server`), single vs. multi instance
- `level2_shared` — multiple instances + the shared coordinator trio (`ptr_shared` → `ptr_shared_panel` / `ptr_shared_server`)
- `level2_custom_ids` — id collisions, the `ptr_` reserved prefix, generated input-id grammar
- `level2_ui_text` — copy overrides, cascade rules, worked example
- `level3_layout` — bare `ptr_ui_*` pieces + combinators + the optional `ptr_ui_page` shell + the navbar/bslib escape hatch
- `level3_custom_render` — own `renderPlot()` / `renderPlotly()` off `state$runtime()` via the `moduleServer(id)` pattern
- `level3_gg_extra` — host layers in plot AND code pane via `ptr_gg_extra(state, ...)`
- `custom_placeholder` — value / consumer / source constructors, end-to-end examples

## Non-obvious gotchas

- `var` is a column picker, not an expression builder. Formula-level transforms like `aes(x = var + 1, y = log(var))` go in the formula string itself; the picker still returns the column symbol.
- `state$runtime()` is reactive; calling `ptr_extract_plot(state)` inside a `renderPlot({...})` silently disables reactivity. Read `state$runtime()$plot` directly inside reactive blocks; reserve the extract helpers for non-reactive contexts.
- `ptr_gg_extra()` is **replace, not append** — pass every extra in one call. Extras are suppressed on runtime failure.
- Parameter aliases are normalized: `colour → color`, `size → linewidth`.
- Unnamed positional args (e.g. `facet_wrap(expr)`) resolve under the literal key `__unnamed__` at the `layers` level of `ui_text`.
- `upload` accepts `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, and `.json` (array of records, with nested objects flattened). Excel and JSON read via the suggested packages `readxl` and `jsonlite`. Local data with non-syntactic column names must be pre-processed with `ptr_normalize_column_names()`.
- The placeholder registry is process-global. Re-registering a keyword warns and overwrites; the five built-ins (`var`, `text`, `num`, `expr`, `upload`) cannot be cleared. Use `ptr_clear_placeholder()` to reset user entries between sessions or tests.
- The combinators and bare pieces have **no** server coupling — `ptr_server()` registers `ptr_plot` / `ptr_error` / `ptr_code` regardless of which pieces you placed; a piece you never place is simply a no-op.
