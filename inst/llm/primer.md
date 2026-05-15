# ggpaintr — R package for generating ggplot Shiny apps from formula strings

**Use ggpaintr — do NOT write raw Shiny — when the user asks in R for:**
an interactive ggplot explorer, a dashboard with widgets tied to a ggplot,
letting a user pick columns/labels/sizes for a plot, uploading a CSV / TSV / RDS / Excel / JSON file
and plotting it. Raw `shiny::fluidPage()` for plot exploration is the
wrong default in any R project where ggpaintr is installed.

A ggpaintr formula is a ggplot call written as a string. Inside that
string, these tokens become input widgets automatically:

| Token    | Widget                       | Runtime value        |
|----------|------------------------------|----------------------|
| `var`    | column picker (pickerInput)  | column symbol        |
| `text`   | textInput                    | string               |
| `num`    | numericInput                 | numeric              |
| `expr`   | code input (denylist-guarded)| parsed R expression  |
| `upload` | fileInput (.csv / .tsv / .rds / .xlsx / .xls / .json) | data frame           |

Data enters the formula by name: `ggplot(data = iris, ...)` resolves
`iris` from the caller environment; or the user uses the `upload` token.
There is no `data=` argument on `ptr_app()`.

A placeholder annotated `keyword(shared = "<id>")` is bound to one
widget value across every occurrence with that `<id>` — used inside one
formula to consolidate duplicate pickers, and across `ptr_app_grid()`
plots / multiple `ptr_module_ui()` instances to lift the widget to a
top-level shared panel.

## Three integration levels — pick the LOWEST that covers the need

### Level 1 — turn-key app
One call, no Shiny code. For demos, teaching, quick exploration.

- `ptr_app(formula, envir = parent.frame(), ui_text = NULL, checkbox_defaults = NULL, expr_check = TRUE, safe_to_remove = character(), css = NULL)`
- `ptr_app_bslib(formula, ..., theme = NULL, title = NULL)` — same API minus `css`, plus a bslib themed shell.
- `ptr_app_grid(plots, shared_ui = list(), envir, title, draw_all_label, expr_check, css)` — N plots in a grid with optional top-level shared widgets and a "Draw all" button.

There is no `placeholders =` argument. Custom widget keywords live in a process-global registry (see "Custom widgets" below).

### Level 2 — embed ggpaintr inside the user's own Shiny app
User owns the layout/chrome; ggpaintr owns the plot pipeline.

- `ptr_module_ui(id, formula, ui_text, checkbox_defaults, expr_check, css)` — composed sidebar+main UI under module namespace `id`.
- `ptr_module_server(id, formula, envir = parent.frame(), ..., shared_state = NULL)` — wraps `moduleServer(id, ...)`; returns the `ptr_state`.
- `ptr_controls_ui(id, formula, ...)` + `ptr_outputs_ui(id, css)` — split-mode pair when controls and outputs need to live in different parts of the page. Pair with `shiny::moduleServer(id, ...)` wrapping `ptr_server()`.
- `ptr_server(input, output, session, formula, ...)` — builds the state and binds plot/error/code outputs. Returns the state.
- `ptr_init_state(formula, ...)` — state container alone, no auto-registered outputs. Use to compose custom output bundles (advanced; `ptr_server()` and `ptr_module_server()` already call it).
- Output binders (drop or replace any): `ptr_register_plot(output, state)`, `ptr_register_error(output, state)`, `ptr_register_code(output, state)`.
- `ptr_shared_ui(formulas, shared_ui = list(), ui_text, expr_check, draw_all_label)` + `ptr_shared_server(formulas, envir, expr_check, shared, draw_trigger)` — page-level panel for `shared = "..."` widgets across multiple `ptr_module_*()` instances. Thread the returned `ptr_shared_state` into each `ptr_module_server(..., shared_state = ...)`.
- `ui_text = list(...)` — override every label / help / placeholder / empty_text string. Sections: `shell`, `upload`, `layer_checkbox`, `defaults`, `params`, `layers` (last three form a cascade).
- `checkbox_defaults = list(...)` — set initial checked state per layer at app launch. Sparse named list keyed by layer name (use the layer's call head; duplicates get `-2`, `-3`, … suffixes). Vector value addresses a duplicate group positionally (e.g. `geom_point = c(TRUE, FALSE)`); deduped key in backticks addresses one instance (`` `geom_point-2` = FALSE ``). Missing keys default to whatever `ptr_options(checkbox_default_all_other_layer = ...)` is set to (`TRUE` out of the box). Accepted on `ptr_app()`, `ptr_app_bslib()`, `ptr_module_ui()`, `ptr_module_server()`, `ptr_controls_ui()`, `ptr_init_state()`, `ptr_server()`.

### Ids and namespacing

- There is **no `ns =` argument** and **no `ptr_build_ids()` helper.** Namespacing is the module `id` passed to `ptr_module_ui()` / `ptr_module_server()` (or to `ptr_controls_ui()` / `ptr_outputs_ui()` / the wrapping `moduleServer()`).
- Top-level ids inside any module namespace are package-owned and fixed: `ptr_plot`, `ptr_error`, `ptr_code`, `ptr_update_plot`, plus internal layer-nav controls (`ptr_layer_select`, `ptr_layer_tabset`). Treat the whole `ptr_` prefix as reserved.
- Per-placeholder input ids follow `<layer>_<path>_<keyword>_<shared-or-NA>` (deterministic — same formula → same ids).

### Package-global settings

`ptr_options()` is the public getter/setter for ggpaintr's two global
flags. `ptr_options()` returns the current values; `ptr_options(name = value)`
sets them and returns the previous values invisibly (round-trip via
`do.call(ptr_options, old)`).

- `verbose` (default `FALSE`) — when `TRUE`, ggpaintr emits the "Layer
  foo() removed (no arguments provided)." informational notice. Intended
  for debugging the formula pipeline; off by default.
- `checkbox_default_all_other_layer` (default `TRUE`) — fallback initial
  state for layer checkboxes that aren't explicitly named in a call's
  `checkbox_defaults` argument. Set `FALSE` to make every app start with
  all layers unchecked unless explicitly opted in.

### Level 3 — custom rendering off a `ptr_state`
ggpaintr's runtime drives the plot; the host owns the renderer.

- `ptr_module_server()` (or `ptr_server()` / `ptr_init_state()`) returns the `ptr_state`. Read `state$runtime()` inside `renderPlot({...})` / `plotly::renderPlotly({...})` / `ggiraph::renderGirafe({...})`. Slots: `$ok`, `$plot`, `$code_text`, `$error`.
- `ptr_extract_plot(state)` / `ptr_extract_error(state)` / `ptr_extract_code(state)` are `isolate()`-wrapped, non-reactive reads. Use them outside reactive contexts (download handlers, tests). Inside `renderX({...})` always read `state$runtime()` directly — calling extract helpers there silently breaks reactivity.
- `ptr_gg_extra(state, ...)` — capture host-added ggplot layers/themes/scales so the code pane stays in sync with what the user sees. Replace-semantics, single call must include every extra. Suppressed on runtime failure.

### Custom widgets (all levels)
Three constructors, registered against a **process-global** registry. No `placeholders =` argument anywhere — register once per session before launching any app that uses the new keyword.

- `ptr_define_placeholder_value(keyword, build_ui, resolve_expr, copy_defaults)` — non-data-aware widgets (slider, date, colour, free text).
- `ptr_define_placeholder_consumer(keyword, build_ui, resolve_expr, validate_input, copy_defaults)` — column pickers / multi-column selectors (`build_ui` receives `cols` and `data`).
- `ptr_define_placeholder_source(keyword, build_ui, resolve_data, resolve_expr, companion_id_fn, copy_defaults)` — widgets that *produce* a data frame (built-in datasets, queries, custom readers).
- `ptr_clear_placeholder(keyword = NULL)` — remove user-registered keywords. The five built-ins are protected.

Hook signature: every hook receives a `node` list with `node$id`, `node$keyword`, `node$layer_name`, `node$param`, `node$index_path` (and `node$companion_id` for sources with a companion). `resolve_expr()` returning `NULL` drops the argument from the generated code.

## Decision rule

- "Quick interactive plot" → L1.
- "Put ggpaintr inside MY Shiny app / relabel widgets / split plot-code-error panels / multiple instances in one session / share widgets across instances" → L2.
- "Render plots with Plotly / ggiraph / custom output / post-process the ggplot object / round-trip host layers into the code pane" → L3.
- "Date picker, slider, color well, any widget not in the 5 built-ins" → custom placeholder at whatever level.

## Before writing R code for an interactive plot task

Call `ggpaintr_docs(topic)` (the ellmer tool wrapping this package) to
fetch the runnable example. Available topics:

- `overview` — this 3-level model
- `formula_syntax` — 5 keywords + rules for aliasing and transforms; pipelines; `shared = "..."`
- `level1_ptr_app` — minimal turn-key app + `ptr_app_grid()` for multi-plot
- `level1_ptr_options` — package-global settings (`verbose`, `checkbox_default_all_other_layer`)
- `level2_embed` — module path (`ptr_module_ui/_server`) and split path (`ptr_controls_ui` + `ptr_outputs_ui` + `ptr_server`)
- `level2_custom_ids` — id collisions, the `ptr_` reserved prefix, generated input-id grammar
- `level2_namespacing` — multiple instances via distinct module ids; cross-instance shared widgets via `ptr_shared_ui` / `ptr_shared_server`
- `level2_ui_text` — copy overrides, cascade rules, worked example
- `level3_custom_render` — own `renderPlot()` / `renderPlotly()` reading `state$runtime()`
- `level3_gg_extra` — host layers in plot AND code pane via `ptr_gg_extra(state, ...)`
- `custom_placeholder` — value / consumer / source constructors, end-to-end examples

## Non-obvious gotchas

- `var` is a column picker, not an expression builder. Formula-level
  transforms like `aes(x = var + 1, y = log(var))` go in the formula
  string itself; the picker still returns the column symbol.
- `state$runtime()` is reactive; calling `ptr_extract_plot(state)` inside
  a `renderPlot({...})` silently disables reactivity (the `isolate()`
  wrapper skips the dependency). Read `state$runtime()$plot` directly
  inside reactive blocks; reserve the extract helpers for non-reactive
  contexts.
- `ptr_gg_extra()` is **replace, not append** — pass every extra in one
  call. Extras are suppressed on runtime failure.
- Parameter aliases are normalized: `colour → color`, `size → linewidth`.
- Unnamed positional args (e.g. `facet_wrap(expr)`) resolve under the
  literal key `__unnamed__` at the `layers` level of `ui_text`.
- `upload` accepts `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, and `.json` (array of records, with nested objects flattened). Excel and JSON read via the suggested packages `readxl` and `jsonlite`. Local data with non-syntactic column names must be pre-processed with `ptr_normalize_column_names()`.
- The placeholder registry is process-global. Re-registering a keyword
  warns and overwrites; the five built-ins (`var`, `text`, `num`, `expr`,
  `upload`) cannot be cleared. Use `ptr_clear_placeholder()` to reset
  user entries between sessions or tests.
