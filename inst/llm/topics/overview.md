# ggpaintr — 3-level extensibility model

ggpaintr is organized into three integration levels. Each higher level is
strictly built on the one below; pick the lowest that covers your need.

```
Level 1 — turn-key app
  ptr_app(formula)            ptr_app_bslib(formula)
  ptr_app_grid(plots, shared_ui)
        ↓ builds on
Level 2 — embed in your own Shiny app
  ptr_module_ui(id, formula)  ptr_module_server(id, formula)
  ptr_controls_ui(id, formula) ptr_outputs_ui(id)
  ptr_server(input, output, session, formula)
  ptr_shared_ui(formulas, shared_ui)
  ptr_shared_server(formulas)
        ↓ builds on
Level 3 — custom rendering off a `ptr_state`
  state$runtime()                  # reactive inside renderX({...}):
                                   #   $ok, $plot, $code_text, $error
  ptr_extract_plot(state)          # isolate() wrappers — non-reactive
  ptr_extract_error(state)         #   reads (download handlers, etc.)
  ptr_extract_code(state)
  ptr_register_plot(output, state) ptr_register_error(output, state)
  ptr_register_code(output, state) # drop / replace one output binder
  ptr_gg_extra(state, ...)         # round-trip host layers
  ptr_init_state(formula)          # state-only constructor; advanced
```

Customization seams shared by every level:

- `ui_text =` — override labels, help, placeholder, empty-text strings.
- `expr_check =` — toggle the safety guard on user-supplied `expr`.
- `css =` — link extra stylesheets after ggpaintr's bundled CSS.
- Custom placeholder keywords are added via `ptr_define_placeholder_value()` /
  `ptr_define_placeholder_consumer()` / `ptr_define_placeholder_source()`.
  The registry is **process-global** — register once per session before
  launching any app that uses the new keyword.

Decision rule:

- "Quick interactive plot" → Level 1.
- "Put ggpaintr inside my Shiny app / split controls and outputs across
  panels / multiple ggpaintr instances on one page" → Level 2.
- "Render the plot myself (Plotly, ggiraph, custom output) / post-process
  the ggplot before drawing / round-trip host layers into the code pane
  / drop or replace one of the bundled output panes" → Level 3.
- "Date picker, slider, color well, or any widget not in the built-ins"
  → custom placeholder at whatever level applies.
