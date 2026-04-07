# Parse a Paintr Formula

Parse a single ggplot-like formula string into a `paintr_obj` containing
expression metadata, placeholder locations, and generated UI
definitions.

## Usage

``` r
paintr_formula(formula, placeholders = NULL)
```

## Arguments

- formula:

  A single formula string describing a ggplot-like expression.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

## Value

An object of class `paintr_obj`.

## Details

Supported placeholders come from the effective placeholder registry. The
built-in registry includes `var`, `text`, `num`, `expr`, and `upload`.

## Examples

``` r
obj <- paintr_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
names(obj$expr_list)
#> [1] "ggplot"     "geom_point"
```
