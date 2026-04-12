# Package index

## Start here

- [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  : Build a ggpaintr Shiny App
- [`ptr_generate_shiny()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_generate_shiny.md)
  : Generate a Standalone Shiny App Script

## Shiny integration

Embed ggpaintr inside a larger Shiny app while keeping package-owned
runtime behavior.

- [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  : Register ggpaintr Server Logic
- [`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md)
  : Build Standard Output Ids for ggpaintr Integration
- [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  : Build Reactive Server State for ggpaintr
- [`ptr_setup_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_setup_controls.md)
  : Bind the Generated Control Panel into a Shiny App
- [`ptr_register_draw()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_draw.md)
  : Bind Draw Behavior into a Shiny App
- [`ptr_register_export()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_export.md)
  : Bind Export Behavior into a Shiny App
- [`ptr_register_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_plot.md)
  : Bind Default Plot Rendering into a Shiny App
- [`ptr_register_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_error.md)
  : Bind Default Error Rendering into a Shiny App
- [`ptr_register_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_code.md)
  : Bind Default Code Rendering into a Shiny App
- [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md)
  : Return the Built Plot from a Runtime Result
- [`ptr_extract_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_error.md)
  : Return Default Error UI from a Runtime Result
- [`ptr_extract_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_code.md)
  : Return Generated Code Text from a Runtime Result
- [`ptr_input_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_input_ui.md)
  : Build Default ggpaintr Control Widgets
- [`ptr_output_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_output_ui.md)
  : Build Default ggpaintr Output Widgets

## Placeholder extensibility

Register and reuse supported custom placeholder types without editing
package internals.

- [`ptr_define_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder.md)
  : Construct a Custom ggpaintr Placeholder
- [`ptr_merge_placeholders()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_placeholders.md)
  : Build the Effective Placeholder Registry for ggpaintr
- [`ptr_missing_expr()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_missing_expr.md)
  : Return the Sentinel for Removing a Placeholder Argument

## Data preparation

- [`ptr_normalize_column_names()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_normalize_column_names.md)
  :

  Normalize Dataset Column Names for `ggpaintr`

## Advanced runtime

Low-level helpers intentionally exported for advanced tests, tooling,
and custom runtime workflows.

- [`ptr_parse_formula()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_parse_formula.md)
  : Parse a Paintr Formula
- [`ptr_runtime_input_spec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_runtime_input_spec.md)
  : Describe the Runtime Inputs for a Parsed Formula
- [`ptr_exec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_exec.md)
  : Build the Full Runtime Result for a Paintr App
- [`ptr_assemble_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_assemble_plot.md)
  : Build a Plot from Completed Layer Expressions

## Copy customization

- [`ptr_merge_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_ui_text.md)
  : Build Effective Copy Rules
- [`ptr_resolve_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_ui_text.md)
  : Resolve Copy for One Control or App Element
