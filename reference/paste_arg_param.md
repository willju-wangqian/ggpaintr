# Paste parameter and its argument

Paste parameter and its argument

## Usage

``` r
paste_arg_param(x, add_quo = FALSE)
```

## Arguments

- x:

  a named character vector

- add_quo:

  bool. Whether to add quotation marks on the arguments

## Value

a string

## Examples

``` r
x <- c(param1 = "arg1", param2 = "arg2")
paste_arg_param(x, add_quo = TRUE)
#> [1] "param1 = 'arg1', param2 = 'arg2'"
```
