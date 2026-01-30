# Unwrap an expression

Unwrap an expression

## Usage

``` r
unwrap_expr(x)
```

## Arguments

- x:

  an expression

## Value

list of strings converted from the expression

## Examples

``` r
unwrap_expr(x + y + z)
#> [[1]]
#> [1] "+"
#> 
#> [[2]]
#> [[2]][[1]]
#> [1] "+"
#> 
#> [[2]][[2]]
#> [1] "x"
#> 
#> [[2]][[3]]
#> [1] "y"
#> 
#> 
#> [[3]]
#> [1] "z"
#> 
```
