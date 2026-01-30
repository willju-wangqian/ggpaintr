# Check if a character vector has name

If this character vector does not have names, then its elements will be
assigned to `names(x)`

## Usage

``` r
check_char_set_names(x)
```

## Arguments

- x:

  a character vector

## Value

a character vector with names

## Examples

``` r
check_char_set_names(c("a", "b"))
#>   a   b 
#> "a" "b" 
```
