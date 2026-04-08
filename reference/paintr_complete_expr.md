# Complete a Parsed Formula with User Inputs

Complete a Parsed Formula with User Inputs

## Usage

``` r
paintr_complete_expr(paintr_obj, input, envir = parent.frame())
```

## Arguments

- paintr_obj:

  A `paintr_obj`.

- input:

  A Shiny input-like object.

- envir:

  The environment used to resolve local data objects.

## Value

A named list with `complete_expr_list`, `code_text`, and `eval_env`.
