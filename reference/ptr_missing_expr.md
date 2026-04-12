# Return the Sentinel for Removing a Placeholder Argument

Placeholder resolvers should return `ptr_missing_expr()` when the target
argument should be removed from the completed expression.

## Usage

``` r
ptr_missing_expr()
```

## Value

An object of class `ptr_missing_expr`.

## Examples

``` r
inherits(ptr_missing_expr(), "ptr_missing_expr")
#> [1] TRUE
```
