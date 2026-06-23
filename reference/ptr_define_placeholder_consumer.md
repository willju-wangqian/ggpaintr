# Define a data-consumer placeholder (e.g. column picker)

A *consumer* placeholder is a value placeholder that additionally
receives the columns of the upstream data frame — typically a column
picker. The built-in example is `ppVar`. See
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
§ "Defining your own placeholders" (consumer role).

## Usage

``` r
ptr_define_placeholder_consumer(
  keyword,
  build_ui,
  resolve_expr,
  validate_session_input = NULL,
  parse_positional_arg = NULL,
  parse_named_args = list(),
  embellish_eval = NULL,
  ui_text_defaults = list(label = "Pick a column for {param}")
)
```

## Arguments

- keyword, ui_text_defaults:

  See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).

- build_ui:

  `function(node, cols, data, label = NULL, selected = NULL, ...)`
  returning a Shiny tag. `cols` is a character vector of upstream column
  names (use as `choices`); `character(0)` before upstream resolves.
  `data` is the upstream data frame, or `NULL` while pending — read it
  only when you need column types / levels / ranges.

  **Widget-seeding contract — `selected`.** Same precedence,
  omit-on-no-default, render-empty-when-empty, and never-read-
  `node$default` rules as for value placeholders — see the
  "Widget-seeding contract" block on
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
  for the authoritative description.

  **Consumer-specific rule: filter through
  [`intersect()`](https://rdrr.io/r/base/sets.html).** Always pass
  `selected = intersect(selected %||% character(0), cols)` (or the
  equivalent column filter) to the underlying widget, not bare
  `selected`. Three cases collapse into one line:

  - User-cleared widget (`selected` is empty) →
    [`intersect()`](https://rdrr.io/r/base/sets.html) returns
    `character(0)`, widget renders empty. Honors the
    render-empty-when-empty rule.

  - Stale pick after upstream data swap (`selected` names a column no
    longer in `cols`) →
    [`intersect()`](https://rdrr.io/r/base/sets.html) drops it cleanly.
    Without this, `selectInput` silently falls back to its first choice
    (silent data mutation) and
    [`shinyWidgets::pickerInput`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
    enters a similarly broken state.

  - Valid pick → [`intersect()`](https://rdrr.io/r/base/sets.html) is a
    no-op, value flows through.

- resolve_expr:

  `function(value, node, ...)`. For a column picker the typical body is
  `rlang::sym(value)` so the bare column name is spliced as an
  identifier rather than a string literal. See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
  for allowed return types and the `NULL`-prunes-the-argument
  convention.

- validate_session_input:

  Optional `function(value, ctx)` called before `resolve_expr`. Return
  `TRUE` / `NULL` to accept; return a single character string to reject
  (surfaced inline as the error message, layer pruned). Useful when a
  stale selection no longer matches any upstream column after a data
  swap, or when only certain column types are admissible. `ctx` is a
  plain list with named fields: `node` (the placeholder AST node,
  carries `$id`, `$keyword`, `$args`), `keyword` (convenience alias for
  `node$keyword`), `upstream_cols` (character vector of upstream column
  names — the same value `build_ui` received as `cols`), and `data` (the
  upstream data frame — the same object `build_ui` received as `data`).
  `ctx$upstream_cols` and `ctx$data` may both be `NULL` while upstream
  resolution is pending; the validator is not invoked when upstream has
  not yet resolved (the substitute walker skips the hook in that case).
  ggpaintr invokes this function as `validate_input(value, ctx)` — no
  other positional or named arguments are passed, and `ctx` carries
  exactly the four fields above. The signature does not require `...`.

- parse_positional_arg, parse_named_args:

  See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).
  Consumer placeholders use the same arg-schema slots; the `ppVar`
  built-in passes a column-name validator here when used as
  `ppVar(mpg)`.

- embellish_eval:

  Optional `function(x, ...)` body used when the placeholder is called
  as a plain-R function. `NULL` (default) supplies the identity from
  [`embellish_identity()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)
  (`function(x, ...) x`), matching the legacy `ppVar`-style
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) NSE shape
  (the symbol-passthrough convention). Override to give the consumer a
  non-identity plain-R meaning (e.g.
  [`embellish_symbol_to_string()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)).

## Value

The runtime callable (identity by default; override with
`embellish_eval = ...`). Also called for its registration side effect;
use
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
to remove it.

## See also

[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
§ "Defining your own placeholders";
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md),
[`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md).

## Examples

``` r
# A consumer that picks a numeric-only column.
# Note: `selected = NULL` formal (per the seeding contract) plus
# intersect() filter (per the consumer-specific rule). Empty input
# renders empty; a stale pick after a data swap drops cleanly.
ptr_define_placeholder_consumer(
  keyword = "numvar",
  build_ui = function(node, cols, data, label = NULL,
                      selected = NULL, ...) {
    numeric_cols <- if (is.null(data)) character(0) else
      names(data)[vapply(data, is.numeric, logical(1))]
    retained <- intersect(selected %||% character(0), numeric_cols)
    shiny::selectInput(node$id, label = label %||% "Numeric column",
                       choices = numeric_cols, selected = retained)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) != 1L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  validate_session_input = function(value, ctx) {
    if (length(value) == 1L && value %in% ctx$upstream_cols) TRUE
    else "Pick a column that exists in the upstream data."
  }
)
#> function (x, ...) 
#> x
#> <bytecode: 0x562a2213ca30>
#> <environment: 0x562a22202388>
ptr_clear_placeholder("numvar")
#> ✔ Cleared placeholder: "numvar".
```
