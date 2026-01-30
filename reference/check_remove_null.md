# check if a list has `NULL`; if so, remove it (them)

check if a list has `NULL`; if so, remove it (them)

## Usage

``` r
check_remove_null(x)
```

## Arguments

- x:

  list

## Value

`NULL` or a list

## Examples

``` r
x <- list(a = 1, b = NULL)
check_remove_null(x)
#> $a
#> [1] 1
#> 
```
