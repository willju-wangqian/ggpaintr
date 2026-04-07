# Register Dynamic `var` UI Outputs for a Parsed Formula

Register Dynamic `var` UI Outputs for a Parsed Formula

## Usage

``` r
register_var_ui_outputs(
  input,
  output,
  paintr_obj,
  envir = parent.frame(),
  copy_rules = NULL
)
```

## Arguments

- input:

  A Shiny input object.

- output:

  A Shiny output object.

- paintr_obj:

  A `paintr_obj`.

- envir:

  Environment used to resolve local data objects.

- copy_rules:

  Effective or user-supplied copy rules.

## Value

A named list of generated `var` UI controls registered on `output`.
