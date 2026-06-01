# Define a value placeholder

Register a new keyword (e.g. `pct`, `color`, `date`) that ggpaintr will
recognise as a substitutable token in a formula. The keyword's widget is
built by `build_ui`; the widget's value is turned back into the R code
spliced into the rendered call by `resolve_expr`. See
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
§ "Defining your own placeholders" for the lifecycle walk-through,
signatures table, and runnable
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
examples — this help page is reference.

## Usage

``` r
ptr_define_placeholder_value(
  keyword,
  build_ui,
  resolve_expr,
  validate_session_input = NULL,
  parse_positional_arg = NULL,
  parse_named_args = list(),
  embellish_eval = NULL,
  ui_text_defaults = list(label = "Enter a value for {param}")
)
```

## Arguments

- keyword:

  Single non-empty string. Must be a syntactically valid R name (passes
  [`make.names()`](https://rdrr.io/r/base/make.names.html)) and not an R
  reserved word. This is the token users type in the formula, e.g.
  `geom_point(alpha = pct)`.

- build_ui:

  `function(node, label = NULL, selected = NULL, ...)` returning a Shiny
  tag. Pass `node$id` as the underlying widget's `inputId`. Read
  `node$keyword` and `node$param` if you need them. The framework also
  passes any `ui_text_defaults` field you declare by name (`help`,
  `placeholder`, `empty_text`) — or accept a `copy = NULL` list and read
  them off it. Always end the signature with `...`.

  **Widget-seeding contract — `selected`** (authoritative; the four
  `invoke_build_ui` call sites in `R/paintr-server.R` implement what is
  described here, with short pointers back to this docblock).

  Declare `selected = NULL` as an explicit formal, **not** a no-default
  `selected` (or accept `...`). The framework calls `build_ui` on every
  renderUI fire and delivers `selected` per this precedence:

  - First render, `spec=` seed present → seed value.

  - First render, no spec, positional default present (e.g. `ppPct(50)`)
    → `node$default`.

  - First render, no spec, no positional default → framework **omits**
    `selected`; your `selected = NULL` formal default applies.

  - Subsequent renders (Update Plot click, upstream change, layer
    toggle, …) → `current-input` verbatim. The `spec=` seed is
    **boot-only**: it wins on the first render only and never
    participates again, so an upstream-triggered re-render can never
    snap the widget back to the seed and away from the user's live pick.
    If the user emptied the widget, `current-input` is whatever the
    widget emits on clear — one of `NULL`, `character(0)`, `""`,
    `NA_real_`, `NA_character_`, depending on the widget — and the
    framework coerces `NULL` to `character(0)` so you always receive a
    value on subsequent renders.

  Because the framework omits the argument on the first-render-no-
  default path, a hook signature without a formal default for `selected`
  aborts there with `argument "selected" is missing`.

  **Render empty when `selected` is empty.** Treat any of `NULL`,
  `character(0)`, `""`, `NA_real_`, `NA_character_` as "the user wants
  this widget empty" and render the widget empty. Do not substitute a
  hardcoded fallback inside `build_ui` for the empty case — that
  re-introduces the deselect-snaps-back-to-default defect the
  framework's seeding layer is designed to prevent (closure-flag in
  `ptr_setup_value_uis()` / `ptr_setup_consumer_uis()` /
  `ptr_bind_shared_consumer_uis()`). Boot-time defaults belong in the
  formula, e.g. `ppPct(50)`, and reach you through `selected`, not
  through a hardcoded constant in the hook body.

  **Never read `node$default` directly.** It is exposed only so the
  framework can route it into `selected` on the boot fire. Reading it
  inside the hook bypasses the precedence chain and the closure- flag
  persistence guards, and will re-clobber a user-cleared widget on every
  Update Plot click.

- resolve_expr:

  `function(value, node, ...)` returning the R expression spliced into
  the rendered call. `value` is `input[[node$id]]` — whatever Shiny
  stores for that widget. Allowed return types: scalar atomic (numeric /
  character / logical / integer), `name`/`symbol` (build with
  [`rlang::sym()`](https://rlang.r-lib.org/reference/sym.html)),
  `call`/`language` (build with `rlang::ppExpr()`), or `NULL` to **prune
  the argument** from the rendered call. Use `NULL` for empty / not-yet
  input; throw with
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
  malformed input.

- validate_session_input:

  Optional `function(value, ctx)` called before `resolve_expr`. Return
  `TRUE` / `NULL` to accept; return a single character string to reject
  (surfaced inline as the error message, layer pruned). `ctx` is a plain
  list with named fields: `node` (the placeholder AST node, carries
  `$id`, `$keyword`, `$args`), `keyword` (convenience alias for
  `node$keyword`), `upstream_cols`, and `data`. For a *value*
  placeholder, `ctx$upstream_cols` and `ctx$data` are **always `NULL`**
  — value placeholders have no upstream column scope by definition. They
  are present in the signature so the same validator shape works across
  all roles; see
  [`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md)
  for the data-aware role where those fields are populated. ggpaintr
  invokes this function as `validate_input(value, ctx)` — no other
  positional or named arguments are passed, and `ctx` carries exactly
  the four fields above. The signature does not require `...`.

- parse_positional_arg:

  Optional validator closure for the (single) positional argument the
  keyword accepts inside a formula. `NULL` (default) means positional
  arguments are rejected at translate time. A function receives the
  unevaluated AST and must return a canonical value or
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html).
  Validators are expected to operate on the AST only and not call
  [`eval()`](https://rdrr.io/r/base/eval.html); ggpaintr trusts the
  author. Authors who eval in a validator are opting into the risk of
  running user code at translate time.

- parse_named_args:

  Named list of validator closures for additional named arguments beyond
  the reserved `shared = ...`. Each entry's closure receives the
  unevaluated AST and returns a canonical value or
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html).
  Default is [`list()`](https://rdrr.io/r/base/list.html) (no named
  args). The name `"shared"` is reserved and may not appear here.

- embellish_eval:

  Optional `function(x, ...)` body used when the placeholder keyword is
  *also* called as a plain-R function (outside a formula context) — i.e.
  how a placeholder-embellished ggplot expression evaluates with no app.
  When `NULL` (default), the identity from
  [`embellish_identity()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)
  (`function(x, ...) x`) is supplied — calling `pct(0.5)` returns `0.5`
  unchanged. Override to give the keyword a non-identity plain-R meaning
  (e.g.
  [`embellish_symbol_to_string()`](https://willju-wangqian.github.io/ggpaintr/reference/embellish_helpers.md)).
  The same callable is returned to the caller of this helper so authors
  can bind it under the same name as the keyword:
  `ppPct <- ptr_define_placeholder_value(keyword = "ppPct", ...)`.

- ui_text_defaults:

  Named list of single non-NA character defaults feeding the `ui_text`
  tree. Allowed names: `label`, `help`, `placeholder`, `empty_text`.
  Strings may contain `{param}`, which is interpolated to the
  surrounding formal-argument name at render time.

## Value

The runtime callable. Default for a value placeholder is the identity
`function(x, ...) x`; override with `embellish_eval = ...`. The helper
is also called for its registration side effect — use
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
to remove the entry.

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

[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
§ "Defining your own placeholders";
[`ptr_define_placeholder_consumer()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_consumer.md),
[`ptr_define_placeholder_source()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_source.md),
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md).

## Examples

``` r
# A percentage placeholder: user types a number 0-100; we splice
# the fraction 0-1 into the rendered call. The hook reads `selected`
# to honor a formula default like `pct(75)` at boot and to keep a
# user-cleared widget empty across Update Plot fires (do NOT
# substitute a hardcoded fallback when selected is empty).
ptr_define_placeholder_value(
  keyword = "pct",
  build_ui = function(node, label = NULL, selected = NULL, ...) {
    n <- suppressWarnings(as.numeric(selected))
    initial <- if (length(n) == 1L && is.finite(n)) n else NA_real_
    shiny::numericInput(node$id, label = label %||% "Percent",
                        value = initial, min = 0, max = 100, step = 1)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) != 1L || !is.finite(value)) return(NULL)
    value / 100
  },
  parse_positional_arg = ptr_arg_numeric(),  # accept ppPct(50)-style defaults
  ui_text_defaults = list(label = "Percent for {param}")
)
#> function (x, ...) 
#> x
#> <bytecode: 0x55f8a781ab58>
#> <environment: 0x55f8a1c1de90>
ptr_clear_placeholder("pct")
#> ✔ Cleared placeholder: "pct".
```
