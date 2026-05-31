# ggpaintr — 3-level integration model

ggpaintr is organized into three integration levels. Each higher level is strictly built on the one below; pick the lowest that covers your need.

Read the prefix: **`ptr_<x>` is L2 self-contained** (owns its own `.ptr-app` scope + asset bundle — drop it into a host app, no shell to remember); the L2 default-layout pair is the bare-named `ptr_ui` / `ptr_server`. **The suffixed `ptr_ui_<x>` family is L3 bare** (emits only its widgets; you supply the Shiny shell).

```
Level 1 — turn-key app
  ptr_app(formula)            ptr_app_bslib(formula)
  ptr_app_grid(plots)
        ↓ builds on
Level 2 — embed in your own Shiny app (default layout, self-contained)
  ptr_ui(formula, id)         ptr_server(formula, id)
  obj <- ptr_shared(formulas)                   # multi-instance only
  ptr_shared_panel(obj)       ptr_shared_server(obj)
        ↓ builds on
Level 3 — own the layout and the render path (bare pieces)
  ptr_ui_header(title)        ptr_ui_controls(formula, id, shared = obj)
  ptr_ui_plot(id)  ptr_ui_error(id)  ptr_ui_code(id)  ptr_ui_shared_panel(obj)
  ptr_ui_inline_error(plot, error)              # combinator
  ptr_ui_toggle_code(plotish, code)             # combinator
  ptr_ui_page(…, page, css)                     # optional shell
  ptr_ui_assets(css)                            # navbar/bslib escape hatch
  state <- ptr_server(formula, id)              # returns the ptr_state
  state$runtime()  -> $ok $plot $code_text $error        # reactive
  ptr_extract_plot(state) / _error / _code               # isolate() — non-reactive
  ptr_gg_extra(state, ...)                       # round-trip host layers
```

## What each level is

- **Level 1 — turn-key app.** One call, no Shiny code: `ptr_app()` (single formula), `ptr_app_grid()` (multiple formulas, one tile each), `ptr_app_bslib()` (bslib-themed shell). For demos, teaching, quick exploration.
- **Level 2 — embed, default layout.** Drop a ggpaintr-driven block into your own Shiny page and keep ggpaintr's default layout (sidebar controls + plot/code/error). The only default-layout block is the self-contained `ptr_ui(formula, id)` / `ptr_server(formula, id)` pair; for two or more linked plots, add the shared trio (`ptr_shared` → `ptr_shared_panel` + `ptr_shared_server`). There is **no** controls-here / plot-there split at L2 — that, and replacing a pane with your own renderer, is L3.
- **Level 3 — own the layout / render path.** Every UI piece has its own bare exported function (the suffixed `ptr_ui_*`); place each anywhere in your own Shiny, and — because `ptr_server()` returns the `ptr_state` — swap ggpaintr's renderer for your own. Behaviour is added by **combinators**, not flags. The server contract is unchanged from L2: a piece you never place is a no-op.

## The partition rule (shared widgets)

A `ppX(shared = "<key>")` placeholder is driven by one widget in lockstep wherever the key appears. *How* it surfaces is decided per key by how many formulas reference it:

> Referenced in **exactly one** formula → that formula's inline **shared section** (inside its own control panel; no coordinator).
> Referenced in **two or more** formulas → the one standalone **shared panel** (built from the coordinator `obj`).

Hard split by instance count: **one ggpaintr instance ⟹ no coordinator, no panel, ever** (every key is formula-local; this is L1 `ptr_app()`); **multiple instances ⟹ you must build `obj <- ptr_shared(formulas = list(…))`** and the cross-formula keys go to `ptr_shared_panel(obj)`.

## Customization seams shared by every level

- `ui_text =` — override labels, help, placeholder, empty-text strings (build it with `ptr_ui_text()`).
- `expr_check =` — toggle the safety guard on user-supplied `ppExpr`.
- `css =` — link extra stylesheets after ggpaintr's bundled CSS (L1 `ptr_app()` / L2 `ptr_shared_panel()` / L3 `ptr_ui_page()`).
- `spec =` — boot-time override of any widget's default value, keyed by its input id (ADR 0012). Discover ids with `ptr_id_table(formula)`.
- Custom placeholder keywords via `ptr_define_placeholder_value()` / `ptr_define_placeholder_consumer()` / `ptr_define_placeholder_source()`. The registry is **process-global** — register once per session before launching any app that uses the new keyword.

## Decision rule

- "Quick interactive plot" → Level 1.
- "Put ggpaintr inside my Shiny app keeping the default layout / multiple linked instances on one page" → Level 2.
- "Hand-place every pane / own the markup / render the plot myself (Plotly, ggiraph, custom output) / post-process the ggplot / round-trip host layers into the code pane" → Level 3.
- "Date picker, slider, color well, or any widget not in the built-ins" → custom placeholder at whatever level applies.

ggpaintr has **no headless / non-Shiny path**; `testServer()` is not a feature surface. L3 is custom rendering inside Shiny off the returned `state`.
