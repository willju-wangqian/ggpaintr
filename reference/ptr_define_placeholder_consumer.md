# Define a data-consumer placeholder (e.g. column picker)

A *consumer* placeholder is a value placeholder that additionally
receives the columns of the upstream data frame — typically a column
picker. The built-in example is `var`. See
[`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
§ "Consumer placeholders" for the tutorial.

## Usage

``` r
ptr_define_placeholder_consumer(
  keyword,
  build_ui,
  resolve_expr,
  validate_input = NULL,
  copy_defaults = list(label = "Pick a column for {param}")
)
```

## Arguments

- keyword, copy_defaults:

  See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).

- build_ui:

  `function(node, cols, data, label, ...)` returning a Shiny tag. `cols`
  is a character vector of upstream column names (use as `choices`);
  `character(0)` before upstream resolves. `data` is the upstream data
  frame, or `NULL` while pending — read it only when you need column
  types / levels / ranges.

- resolve_expr:

  `function(value, node, ...)`. For a column picker the typical body is
  `rlang::sym(value)` so the bare column name is spliced as an
  identifier rather than a string literal. See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
  for allowed return types and the `NULL`-prunes-the-argument
  convention.

- validate_input:

  Optional `function(value, upstream_cols)` called before
  `resolve_expr`. Return `TRUE` / `NULL` to accept; return a single
  character string to reject (surfaced inline as the error message,
  layer pruned). Useful when a stale selection no longer matches any
  upstream column after a data swap.

## Value

Invisibly, the registry entry list. Use
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
to remove it.

## See also

[`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
for the tutorial;
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md),
[`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md).

## Examples

``` r
# A consumer that picks a numeric-only column.
ptr_define_placeholder_consumer(
  keyword = "numvar",
  build_ui = function(node, cols, data, label, ...) {
    numeric_cols <- if (is.null(data)) character(0) else
      names(data)[vapply(data, is.numeric, logical(1))]
    shiny::selectInput(node$id, label = label, choices = numeric_cols,
                       selected = character(0))
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) != 1L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  validate_input = function(value, upstream_cols) {
    if (length(value) == 1L && value %in% upstream_cols) TRUE
    else "Pick a column that exists in the upstream data."
  }
)
ptr_clear_placeholder("numvar")
#> ✔ Cleared placeholder: "numvar".
```
