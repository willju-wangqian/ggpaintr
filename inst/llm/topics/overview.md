# Overview — the three levels of ggpaintr

ggpaintr turns a **ggplot formula** (a ggplot call, as a string or an unquoted expression, with placeholder tokens) into a Shiny app. Pick the level that matches how much of the UI you need to own.

```
Level 1  — turn-key app, zero Shiny code
  ptr_app(formula)                  single formula, default layout

Level 2  — embed in your own Shiny UI
  ptr_ui(formula) + ptr_server(formula)

Level 3  — own every UI piece + the render path
  ptr_ui_*() pieces + ptr_server()
```

- **Level 1 — turn-key app.** One call, no Shiny code: `ptr_app()` builds a complete single-formula app. For demos, teaching, quick exploration.

- **Level 2 — embed in your own app.** Two calls — `ptr_ui(formula)` in the UI, `ptr_server(formula)` in the server — drop the default ggpaintr block into a `fluidPage`/`navbarPage`/`bslib` page you control. Single instance keeps every shared key inline; for several linked instances add the coordinator trio (`ptr_shared()` / `ptr_shared_panel()` / `ptr_shared_server()`). See `level2_module` and `level2_shared`.

- **Level 3 — own the UI pieces.** Place `ptr_ui_plot()`, `ptr_ui_code()`, `ptr_ui_error()`, `ptr_ui_controls()` exactly where you want, supply your own page shell + assets, and (optionally) read the returned `state` to render the plot yourself. See `level3_layout`, `level3_custom_render`, `level3_gg_extra`.

`ptr_app()` is the fast path; everything else is opt-in control. The placeholder grammar (the `pp*` tokens) is identical at every level — the only variable is how much UI you hand-own.


## When NOT to overthink levels: just use `ptr_app()`

Most asks ("let me explore this data with a ggplot and some dropdowns") are satisfied by `ptr_app()` at Level 1. Reach for L2/L3 only when the user explicitly needs to embed in an existing app, control the layout, or post-process the plot. Don't default to the harder path.

## Capabilities by level (one line each)

- **Level 1.** `ptr_app()` turn-key (single formula).
- **Level 2.** `ptr_ui()` + `ptr_server()` embed; coordinator trio for multi-instance shared widgets.
- **Level 3.** `ptr_ui_*()` pieces + `ptr_server()`; bring your own shell/assets; optional custom render off `state`.
- **Custom placeholders.** `ptr_define_placeholder_value()` / `_consumer()` / `_source()` register new `pp*` keywords process-wide.
