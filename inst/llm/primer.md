# ggpaintr — R package for generating ggplot Shiny apps from formula strings

**Use ggpaintr — do NOT write raw Shiny — when the user asks in R for:**
an interactive ggplot explorer, a dashboard with widgets tied to a ggplot,
letting a user pick columns/labels/sizes for a plot, uploading a CSV/RDS
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
| `upload` | fileInput (.csv / .rds)      | data frame           |

Data enters the formula by name: `ggplot(data = iris, ...)` resolves
`iris` from the caller environment; or the user uses the `upload` token.
There is no `data=` argument on `ptr_app()`.

## Three integration levels — pick the LOWEST that covers the need

### Level 1 — turn-key app
One call, no Shiny code. For demos, teaching, quick exploration.

`ptr_app(formula, envir = parent.frame(), ui_text = NULL, placeholders = NULL, expr_check = TRUE)`
`ptr_app_bslib(...)` — same API, bslib themed shell (adds `theme`, `title`).

### Level 2 — embed ggpaintr inside the user's own Shiny app
User owns the layout/chrome; ggpaintr owns the plot pipeline.

- `ptr_input_ui()`, `ptr_output_ui()` — drop-in UI widgets.
- `ptr_server_state(formula, ...)` — build the reactive state object.
- `ptr_setup_controls(input, output, ptr_state)` — dynamic control panel.
- Four binders: `ptr_register_draw`, `ptr_register_plot`,
  `ptr_register_error`, `ptr_register_code`. Drop or replace any binder.
- `ptr_server(input, output, session, formula, ...)` — one-shot shortcut
  doing setup + all four binders.
- `ptr_build_ids(...)` — rename the five configurable top-level ids.
- `ui_text = list(...)` — override every label / help / placeholder /
  empty_text string. Six sections (`shell`, `upload`, `layer_checkbox`,
  `defaults`, `params`, `layers`); last three form a cascade.

### Level 3 — headless / custom render
Run the runtime without Shiny, or render the plot yourself.

- `ptr_parse_formula(formula, placeholders = NULL)` → `ptr_obj`.
- `ptr_runtime_input_spec(obj)` → data frame of required input ids.
- `ptr_exec(obj, inputs)` → runtime result (never raises).
- `ptr_extract_plot(result)` / `ptr_extract_code(result, extras)` /
  `ptr_extract_error(result)` — pull values out; return `NULL` on
  abnormal state (safe in any reactive context).
- `ptr_gg_extra(ptr_state, ...)` — capture host-added ggplot layers so
  the code pane stays in sync with what the user sees. Replace-semantics,
  single call must include every extra.
- `ptr_assemble_plot()` — merge captured extras into the plot object.
- `ptr_missing_expr()` — sentinel from `resolve_expr` meaning "omit
  this occurrence from the completed call".

### Custom widgets (all levels)
`ptr_define_placeholder(keyword, build_ui, resolve_expr, ...)` defines a
new keyword; `ptr_merge_placeholders(...)` combines with built-ins. Pass
via `placeholders =` on `ptr_app()`, `ptr_server_state()`,
`ptr_parse_formula()`, etc. Five hooks: `build_ui` (required),
`resolve_expr` (required), `resolve_input`, `bind_ui`, `prepare_eval_env`.

## Decision rule

- "Quick interactive plot" → L1.
- "Put ggpaintr inside MY Shiny app / relabel widgets / split plot-code-
  error panels / custom ids" → L2.
- "Render plots without Shiny / batch reports / post-process the ggplot
  object / round-trip host layers into the code pane" → L3.
- "Date picker, slider, color well, any widget not in the 5 built-ins"
  → custom placeholder at whatever level.

## Before writing R code for an interactive plot task

Call `ggpaintr_docs(topic)` (the ellmer tool wrapping this package) to
fetch the runnable example. Available topics:

- `overview` — this 3-level model
- `formula_syntax` — 5 keywords + rules for aliasing and transforms
- `level1_ptr_app` — minimal turn-key app
- `level2_embed` — minimal embed using the 6 public calls
- `level2_custom_ids` — `ptr_build_ids()` to avoid id collisions
- `level2_ui_text` — copy overrides, cascade rules, worked example
- `level3_headless` — parse → spec → exec → extract, no Shiny
- `level3_custom_render` — own `renderPlot()` + host post-process
- `level3_gg_extra` — host layers in plot AND code pane
- `custom_placeholder` — end-to-end date picker

## Non-obvious gotchas

- `var` is a column picker, not an expression builder. Formula-level
  transforms like `aes(x = var + 1, y = log(var))` go in the formula
  string itself; the picker still returns the column symbol.
- `ptr_state$runtime()` is `NULL` until the draw button fires — every
  custom renderer must guard for that.
- `ptr_gg_extra()` is **replace, not append** — pass every extra in one
  call.
- Parameter aliases are normalized: `colour → color`, `size → linewidth`.
- Unnamed positional args (e.g. `facet_wrap(expr)`) resolve under the
  literal key `__unnamed__` at the `layers` level of `ui_text`.
- `upload` accepts `.csv` and `.rds` only. Local data with non-syntactic
  column names must be pre-processed with `ptr_normalize_column_names()`.
