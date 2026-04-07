# Resolve Copy for One Control or App Element

Resolve Copy for One Control or App Element

## Usage

``` r
paintr_resolve_copy(
  component,
  keyword = NULL,
  layer_name = NULL,
  param = NULL,
  copy_rules = NULL,
  placeholders = NULL
)
```

## Arguments

- component:

  One of `title`, `draw_button`, `export_button`, `upload_file`,
  `upload_name`, `layer_checkbox`, or `control`.

- keyword:

  Optional placeholder keyword.

- layer_name:

  Optional layer name.

- param:

  Optional parameter name.

- copy_rules:

  Effective or user-supplied copy rules.

- placeholders:

  Optional custom placeholder definitions or an effective placeholder
  registry.

## Value

A named list with `label`, `help`, `placeholder`, and `empty_text`.
