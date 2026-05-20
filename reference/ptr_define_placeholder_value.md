# Define a value placeholder

Register a new keyword (e.g. `pct`, `color`, `date`) that ggpaintr will
recognise as a substitutable token in a formula. The keyword's widget is
built by `build_ui`; the widget's value is turned back into the R code
spliced into the rendered call by `resolve_expr`. See
[`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
§ "Adding a new widget type" for the lifecycle walk-through, signatures
table, and runnable
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
examples — this help page is reference.

## Usage

``` r
ptr_define_placeholder_value(
  keyword,
  build_ui,
  resolve_expr,
  copy_defaults = list(label = "Enter a value for {param}")
)
```

## Arguments

- keyword:

  Single non-empty string. Must be a syntactically valid R name (passes
  [`make.names()`](https://rdrr.io/r/base/make.names.html)) and not an R
  reserved word. This is the token users type in the formula, e.g.
  `geom_point(alpha = pct)`.

- build_ui:

  `function(node, label, ...)` returning a Shiny tag. Pass `node$id` as
  the underlying widget's `inputId`. Read `node$keyword` and
  `node$param` if you need them. The framework also passes any
  `copy_defaults` field you declare by name (`help`, `placeholder`,
  `empty_text`) — or accept a `copy = NULL` list and read them off it.
  Always end the signature with `...`.

- resolve_expr:

  `function(value, node, ...)` returning the R expression spliced into
  the rendered call. `value` is `input[[node$id]]` — whatever Shiny
  stores for that widget. Allowed return types: scalar atomic (numeric /
  character / logical / integer), `name`/`symbol` (build with
  [`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html)),
  `call`/`language` (build with
  [`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html)), or
  `NULL` to **prune the argument** from the rendered call. Use `NULL`
  for empty / not-yet input; throw with
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  malformed input.

- copy_defaults:

  Named list of single non-NA character defaults feeding the `ui_text`
  tree. Allowed names: `label`, `help`, `placeholder`, `empty_text`.
  Strings may contain `{param}`, which is interpolated to the
  surrounding formal-argument name at render time.

## Value

Invisibly, the registry entry list. Called for its side effect of
registering the placeholder in the package-global registry. Use
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
to remove it.

## Details

Three roles. Pick this constructor for a *value* placeholder (a
self-contained widget like a slider, color picker, or numeric input).
Use
[`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md)
when the widget needs the upstream column names (column pickers). Use
[`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md)
when the widget *produces* the data the rest of the formula reads from
(file upload, dataset chooser).

## See also

[`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
for the tutorial;
[`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md),
[`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md),
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md).

## Examples

``` r
# A percentage placeholder: user types a number 0-100; we splice
# the fraction 0-1 into the rendered call.
ptr_define_placeholder_value(
  keyword = "pct",
  build_ui = function(node, label, ...) {
    shiny::numericInput(node$id, label = label, value = 50,
                        min = 0, max = 100, step = 1)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) != 1L || !is.finite(value)) return(NULL)
    value / 100
  },
  copy_defaults = list(label = "Percent for {param}")
)
ptr_clear_placeholder("pct")
#> ✔ Cleared placeholder: "pct".
```
