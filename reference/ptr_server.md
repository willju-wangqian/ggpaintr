# Register ggpaintr Server Logic

Wire the standard `ggpaintr` server behavior into an existing Shiny app.
The returned state object exposes reactive access to the parsed formula,
latest runtime result, and current dynamic `var` UI definitions so
callers can extend the app with additional observers and outputs.

## Usage

``` r
ptr_server(
  input,
  output,
  session,
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  placeholders = NULL,
  ids = ptr_build_ids(),
  expr_check = TRUE
)
```

## Arguments

- input:

  A Shiny `input` object.

- output:

  A Shiny `output` object.

- session:

  A Shiny `session` object.

- formula:

  A single formula string using `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects when building the app.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

- ids:

  A `ptr_build_ids` object controlling the Shiny element IDs used by the
  integration helpers. Defaults to
  [`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md).

- expr_check:

  Controls `expr` placeholder validation. `TRUE` (default) applies the
  built-in denylist of dangerous functions. `FALSE` disables all
  checking. A named list with `deny_list` and/or `allow_list` character
  vectors supplies a custom check; when both are given, denied entries
  are removed from the allowlist.

## Value

A `ptr_state` object containing reactive accessors named `obj`,
`runtime`, and `var_ui_list`, plus shared metadata used by the bind
helpers.
