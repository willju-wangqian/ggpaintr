# Return the Sentinel for Removing a Placeholder Argument

Placeholder resolvers should return `ggpaintr_missing_expr()` when the
target argument should be removed from the completed expression.

## Usage

``` r
ggpaintr_missing_expr()
```

## Value

An object of class `ggpaintr_missing_expr`.

## Examples

``` r
inherits(ggpaintr_missing_expr(), "ggpaintr_missing_expr")
#> [1] TRUE
```
