# Build the Effective Placeholder Registry for ggpaintr

Combine the built-in placeholder definitions with optional custom
placeholders. Custom placeholders override built-in placeholders on
keyword collision.

## Usage

``` r
ggpaintr_effective_placeholders(placeholders = NULL)
```

## Arguments

- placeholders:

  Either `NULL`, a named list of `ggpaintr_placeholder` objects, or an
  existing `ggpaintr_placeholder_registry`.

## Value

An object of class `ggpaintr_placeholder_registry`.

## Examples

``` r
registry <- ggpaintr_effective_placeholders()
all(c("var", "text", "num", "expr", "upload") %in% names(registry))
#> [1] TRUE

date_placeholder <- ggpaintr_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ggpaintr_missing_expr())
    }

    rlang::expr(as.Date(!!value))
  }
)
custom_registry <- ggpaintr_effective_placeholders(list(date = date_placeholder))
"date" %in% names(custom_registry)
#> [1] TRUE
```
