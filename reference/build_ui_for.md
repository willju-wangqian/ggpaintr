# Build a Shiny UI Widget for a Typed-Tree Node

S3 generic dispatched per node class. Resolves the human-readable label
through `ptr_resolve_ui_text`, namespaces the node's id (and any
`shortcut_id` for source nodes) via `ns_fn`, and forwards to the
registered `build_ui` hook for the placeholder's keyword.

## Usage

``` r
build_ui_for(node, ...)
```

## Arguments

- node:

  A typed AST node (e.g. `ptr_ph_value`, `ptr_ph_data_consumer`,
  `ptr_ph_data_source`, `ptr_layer`).

- ...:

  Additional arguments. Recognized by built-in methods: `ui_text`,
  `layer_name`, `ns_fn`, `shell_copy`, `label_override` (force a
  specific widget label, used for shared widgets referenced under
  several params). Consumer placeholders emit only a `uiOutput`
  container at static build time; their picker is rendered server-side
  via `ptr_setup_consumer_uis()`, which calls the registry's
  `build_ui(node, cols, ...)` inside `renderUI` once cols are resolved.

## Value

A `shiny.tag` (or NULL for nodes that emit no UI).
