# Append a named list

Append a named list

## Usage

``` r
append_list_name(x, name, value)
```

## Arguments

- x:

  a list

- name:

  new name

- value:

  value of the new name

## Value

a list

## Examples

``` r
x <- list(a = 1, b = 2)
append_list_name(x, "c", 3)
#> $a
#> [1] 1
#> 
#> $b
#> [1] 2
#> 
#> $c
#> [1] 3
#> 
```
