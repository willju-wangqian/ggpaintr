# Parse a Paintr Formula

Parse a single ggplot-like formula string into a `ptr_obj` containing
expression metadata, placeholder locations, and generated UI
definitions.

## Usage

``` r
ptr_parse_formula(formula, placeholders = NULL, formula_check = TRUE)
```

## Arguments

- formula:

  A single formula string describing a ggplot-like expression.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

- formula_check:

  Logical or list controlling safety validation of the formula text
  itself. `TRUE` (default) applies the default denylist. `FALSE` skips
  validation, treating the formula as trusted developer input. A list
  with `deny_list` / `allow_list` customises the check. See
  `validate_expr_safety` details.

## Value

An object of class `ptr_obj`.

## Details

Supported placeholders come from the effective placeholder registry. The
built-in registry includes `var`, `text`, `num`, `expr`, and `upload`.

## Note

The `formula` argument is validated by default using the denylist. Set
`formula_check = FALSE` only for trusted developer input that you know
is safe — this is an advanced option for package authors who
programmatically generate formula strings. This check is independent of
the per-placeholder `expr_check` applied at runtime; disabling one does
not disable the other.

## Examples

``` r
obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
names(obj$expr_list)
#> [1] "ggplot"     "geom_point"
```
