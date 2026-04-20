# ggpaintr — 3-level extensibility model

ggpaintr is organized into three integration levels. Each higher level is
strictly built on the one below; pick the lowest that covers your need.

```
Level 1 — turn-key app
  ptr_app(formula)          ptr_app_bslib(formula)
        ↓ builds on
Level 2 — embed in your own Shiny app
  ptr_input_ui()            ptr_output_ui()
  ptr_server_state()        ptr_build_ids()
  ptr_setup_controls()      ptr_register_draw()
  ptr_register_plot()       ptr_register_error()
  ptr_register_code()       ptr_server()
        ↓ builds on
Level 3 — developer API
  ptr_parse_formula()       ptr_runtime_input_spec()
  ptr_exec()                ptr_assemble_plot()
  ptr_extract_plot()        ptr_extract_code()
  ptr_extract_error()       ptr_gg_extra()
  ptr_missing_expr()
```

Customization seams shared by every level:

- `ui_text =` — override labels, help, placeholder, empty-text strings.
- `placeholders =` — register new keywords or replace built-ins.
- `expr_check =` — toggle the safety guard on user-supplied `expr`.

Decision rule:

- "Quick interactive plot" → Level 1.
- "Put ggpaintr inside my Shiny app / relabel widgets / split panels /
  custom ids" → Level 2.
- "Render plots without Shiny / batch reports / post-process the ggplot
  object / round-trip host layers into the code pane" → Level 3.
- "Date picker, slider, color well, or any widget not in the built-ins"
  → custom placeholder at whatever level applies.
