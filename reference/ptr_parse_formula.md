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

## Column-name collisions with placeholder keywords

Placeholder detection is purely syntactic: any bare symbol whose name
matches a registered keyword (`var`, `text`, `num`, `expr`, `upload`, or
any custom keyword) is consumed as a placeholder, regardless of whether
the active dataset has a column with the same name. As a consequence, if
your data has a column literally named `"var"`, writing the bare symbol
`var` in the formula will *not* reference that column — it will be
replaced by the user's `var` widget selection. Backtick quoting
(`` `var` ``) parses to the same R symbol and does not escape the
conflict. To reference such a column, use `.data[["var"]]`, which
represents the column name as a string literal (not a symbol) and is
therefore ignored by the placeholder walker. Alternatively, pick the
column from the `var` dropdown, which still lists every column of the
dataset (including one named `"var"`).

## Examples

``` r
obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
names(obj$expr_list)
#> [1] "ggplot"     "geom_point"
```
