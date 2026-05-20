# Define a data-source placeholder (e.g. upload, database table)

A *source* placeholder produces a data frame the rest of the formula
reads from. Built-in example: `upload`. Custom examples: database
tables, built-in datasets, URL fetches.

## Usage

``` r
ptr_define_placeholder_source(
  keyword,
  build_ui,
  resolve_data,
  resolve_expr = NULL,
  companion_id_fn = NULL,
  copy_defaults = list(label = "Provide a data source for {param}")
)
```

## Arguments

- keyword, copy_defaults:

  See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).
  See
  [`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
  § "Source placeholders" for the tutorial.

- build_ui:

  `function(node, label, ...)` returning a Shiny tag — same shape as in
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).
  With `companion_id_fn` set, render **two** bound inputs in the same
  tag, one with `inputId = node$id` (data payload) and one with
  `inputId = node$companion_id` (sibling input — typically the
  user-facing dataset name spliced into the rendered code).

- resolve_data:

  `function(value, node, ...)` returning a `data.frame` (the data
  downstream consumers read from), or `NULL` to signal "no data yet".
  Throw via
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  malformed inputs.

- resolve_expr:

  Optional. `function(value, node, ...)` returning the expression
  spliced into the rendered code at the placeholder's position — i.e.
  *how the data is referred to* in the reproducible call, not the data
  itself. Default `rlang::sym(value)` works when the widget's value is
  already the symbol you want. Override to make the rendered code
  re-fetch instead of referencing an in-session object, e.g.
  `function(value, node, ...) rlang::expr(read.csv(!!value$datapath))`.
  With `companion_id_fn` set, `value` here is the *companion* input's
  value (e.g. the typed dataset name), **not** the primary payload — the
  built-in `upload` relies on this so the default splices the typed name
  as a bare symbol.

- companion_id_fn:

  Optional `function(id) -> companion_id_string`. Use this when the
  source widget needs **two** bound Shiny inputs that both participate
  in the runtime substitution cycle: one at `node$id` (the data payload)
  and one at `node$companion_id` (a sibling input, typically a name or
  override). The framework calls this function with the primary id and
  namespaces the returned companion id alongside it, so a single
  `build_ui` can render both widgets and both values reach
  `resolve_data` / `resolve_expr` through `node`. Most sources do not
  need it — one bound input is the common case. The built-in `upload`
  uses it to attach the "Optional dataset name" textbox: the file
  contents bind to `node$id`, the user-typed dataset name binds to
  `node$companion_id`, and the substitution uses the name as the symbol
  inserted into the generated code. Pass `NULL` (default) when one input
  suffices.

## Value

Invisibly, the registry entry list. Use
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
to remove the registration.

## See also

[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md),
[`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md),
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md).

## Examples

``` r
# A minimal in-memory dataset source (picks from pre-loaded data frames).
ptr_define_placeholder_source(
  keyword = "dataset",
  build_ui = function(node, label, ...) {
    shiny::selectInput(node$id, label = label,
                       choices = c("mtcars", "iris"))
  },
  resolve_data = function(value, node, ...) {
    if (length(value) != 1L || !nzchar(value)) return(NULL)
    get(value, envir = as.environment("package:datasets"))
  }
)
ptr_clear_placeholder("dataset")
#> ✔ Cleared placeholder: "dataset".
```
