# Build a Variable Selector UI

Build a Variable Selector UI

## Usage

``` r
generate_ui_var(data_var, id, param, layer_name = NULL, copy_rules = NULL)
```

## Arguments

- data_var:

  Character vector of candidate variables.

- id:

  Placeholder id.

- param:

  Parameter label.

- layer_name:

  Layer name.

- copy_rules:

  Effective or user-supplied copy rules.

## Value

A `pickerInput()` UI object or `NULL`.
