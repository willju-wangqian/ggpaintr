# Package index

## L1 — All-in-one

Turn a ggplot-style formula into a ready-to-run Shiny app. One formula →
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md);
several linked plots →
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md).

- [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  :

  Build a Shiny App from a `ggpaintr` Formula

- [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
  :

  Grid App: Multiple `ggpaintr` Plots With Shared Controls

## Formula placeholders

The `pp*` tokens you write inside a formula — the value/consumer/source
controls plus the structural toggles.

- [`ppVar()`](https://willju-wangqian.github.io/ggpaintr/reference/pp_placeholders.md)
  [`ppNum()`](https://willju-wangqian.github.io/ggpaintr/reference/pp_placeholders.md)
  [`ppText()`](https://willju-wangqian.github.io/ggpaintr/reference/pp_placeholders.md)
  [`ppExpr()`](https://willju-wangqian.github.io/ggpaintr/reference/pp_placeholders.md)
  [`ppUpload()`](https://willju-wangqian.github.io/ggpaintr/reference/pp_placeholders.md)
  : Placeholder Identity Helpers
- [`ppLayerOff()`](https://willju-wangqian.github.io/ggpaintr/reference/ppLayerOff.md)
  : Off-by-default layer wrapper
- [`ppVerbSwitch()`](https://willju-wangqian.github.io/ggpaintr/reference/ppVerbSwitch.md)
  : Switchable pipeline-stage wrapper

## Session configuration

Process-wide defaults, column-name normalisation, and UI copy overrides.

- [`ptr_options()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_options.md)
  : Get or Set ggpaintr Package Options

- [`ptr_normalize_column_names()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_normalize_column_names.md)
  :

  Normalize Dataset Column Names for `ggpaintr`

- [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  : Inspect, validate, and pre-merge ggpaintr UI copy

## Wrapper examples

Worked examples of writing custom wrappers on top of the public ggpaintr
primitives. See
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md).

- [`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md)
  : Bslib-Themed App: A Demonstration Wrapper

## L2 — Embed with the default layout

Plug ggpaintr into your own Shiny app with its self-contained default
layout. The module pair is the L2 entry; the shared coordinator
([`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
→
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md) +
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md))
links a cross-formula key across plots. See
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
§ “Multiple plots, and writing your own Shiny app”.

- [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
  :

  Self-contained UI for a `ggpaintr` Formula

- [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  :

  Server for a `ggpaintr` Formula

- [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  : Build the Shared Coordinator for a Multi-Instance Embedding

- [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
  : Render the Standalone Shared Panel (L2, Self-Contained)

- [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
  : Server-Side Counterpart to the Shared Coordinator

## L3 — Own the layout / render path

Own every piece of ggpaintr’s public UI — bare builders (no behaviour
flags) plus nestable combinators. Compose freely under your own Shiny
(optionally
[`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md))
and wire with
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).
See
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md).

- [`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md)
  : Page shell for hand-composed ggpaintr UIs

- [`ptr_ui_header()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_header.md)
  :

  App Header Piece for `ggpaintr`

- [`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md)
  :

  Controls Piece for a `ggpaintr` Formula

- [`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md)
  :

  Plot Pane Piece for a `ggpaintr` Formula

- [`ptr_ui_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_error.md)
  :

  Inline Error Pane Piece for a `ggpaintr` Formula

- [`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md)
  :

  Generated-Code Pane Piece for a `ggpaintr` Formula

- [`ptr_ui_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_shared_panel.md)
  : Render the Standalone Shared Panel (L3, Bare)

- [`ptr_ui_inline_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_inline_error.md)
  : Nest an Inline Error Slot in a Plot Piece

- [`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md)
  :

  Wire a Plot-ish Piece to a Slide-Out Code Window via the `</>` Toggle

- [`ptr_ui_assets()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_assets.md)
  :

  Bundled CSS / JS Assets Piece for `ggpaintr`

## Advanced embedders

Drive the typed tree programmatically or read runtime outputs off the
returned state.

- [`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md)
  : Construct the ggpaintr runtime state container

- [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
  [`ptr_extract_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
  [`ptr_extract_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
  :

  Extract Runtime Outputs From a `ptr_state`

- [`ptr_gg_extra()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_gg_extra.md)
  :

  Add `ggplot2` Layers Programmatically

- [`ptr_resolve_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_ui_text.md)
  : Resolve copy for one ggpaintr control or app element

- [`ptr_id_table()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_id_table.md)
  : Enumerate every Shiny id a ggpaintr formula produces

## Placeholder authors

Register supported custom placeholder types and their UI hooks without
editing package internals.

- [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
  : Define a value placeholder

- [`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md)
  : Define a data-consumer placeholder (e.g. column picker)

- [`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md)
  : Define a data-source placeholder (e.g. upload, database table)

- [`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
  : Remove user-registered placeholders

- [`build_ui_for()`](https://willju-wangqian.github.io/ggpaintr/reference/build_ui_for.md)
  : Build a Shiny UI Widget for a Typed-Tree Node

- [`ptr_arg_symbol_or_string()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md)
  [`ptr_arg_string()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md)
  [`ptr_arg_numeric()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md)
  [`ptr_arg_numeric_vector()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md)
  [`ptr_arg_expression()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md)
  :

  Argument validators for placeholder definitions (`ptr_arg_*`)

- [`ptr_register_constant_fold()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_constant_fold_registry.md)
  [`ptr_clear_constant_fold()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_constant_fold_registry.md)
  [`ptr_constant_fold_keywords()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_constant_fold_registry.md)
  : Constant-fold allowlist registry

- [`ptr_signal_partial()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_signal_partial.md)
  : Signal a transient "partial input" failure from a placeholder hook

## LLM tooling

Expose ggpaintr’s documentation bundle to ellmer-backed coding
assistants.

- [`ptr_llm_primer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_primer.md)
  : Get the ggpaintr LLM system-prompt primer
- [`ptr_llm_topic()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topic.md)
  : Fetch a ggpaintr LLM topic by name
- [`ptr_llm_topics()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_topics.md)
  : List available ggpaintr LLM topic names
- [`ptr_llm_register()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_llm_register.md)
  : Register ggpaintr with an ellmer chat session
