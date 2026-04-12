# Build the Effective Placeholder Registry for ggpaintr

Combine the built-in placeholder definitions with optional custom
placeholders. Custom placeholders override built-in placeholders on
keyword collision.

## Usage

``` r
ptr_merge_placeholders(placeholders = NULL)
```

## Arguments

- placeholders:

  Either `NULL`, a named list of `ptr_define_placeholder` objects, or an
  existing `ptr_define_placeholder_registry`.

## Value

An object of class `ptr_define_placeholder_registry`.

## Examples

``` r
registry <- ptr_merge_placeholders()
all(c("var", "text", "num", "expr", "upload") %in% names(registry))
#> [1] TRUE

date_placeholder <- ptr_define_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }

    rlang::expr(as.Date(!!value))
  }
)
custom_registry <- ptr_merge_placeholders(list(date = date_placeholder))
"date" %in% names(custom_registry)
#> [1] TRUE
```
