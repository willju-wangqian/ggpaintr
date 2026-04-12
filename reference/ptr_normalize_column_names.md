# Normalize Dataset Column Names for `ggpaintr`

Normalize incoming column names so `var` placeholders can require exact,
syntactic, unique column-name matches at runtime.

## Usage

``` r
ptr_normalize_column_names(data)
```

## Arguments

- data:

  A data frame or an object coercible with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Value

A tabular object with `ggpaintr`-safe column names. Existing
`data.frame` subclasses keep their class. Names are made syntactic,
unique, and safe against reserved-word collisions. Non-`data.frame`
inputs return the `data.frame` created by
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html).

## Examples

``` r
messy <- data.frame(
  check.names = FALSE,
  "first column" = 1:3,
  "if" = 4:6
)

clean <- ptr_normalize_column_names(messy)
names(clean)
#> [1] "first_column" "if_"         
```
