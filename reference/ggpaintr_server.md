# Register ggpaintr Server Logic

Wire the standard `ggpaintr` server behavior into an existing Shiny app.
The returned state object exposes reactive access to the parsed formula,
latest runtime result, and current dynamic `var` UI definitions so
callers can extend the app with additional observers and outputs.

## Usage

``` r
ggpaintr_server(
  input,
  output,
  session,
  formula,
  envir = parent.frame(),
  copy_rules = NULL
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

- copy_rules:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

## Value

A list containing reactive accessors named `obj`, `runtime`, and
`var_ui_list`.
