# Extensibility — how users plug into ggpaintr

The supported extension surfaces, with exact signatures verified from source on **2026-05-27**. There are five categories:

1. **Custom placeholders** — `ptr_define_placeholder_{value,consumer,source}` register a new `pp*`-style keyword.
2. **L3 custom render** — extract `state$runtime()` from `ptr_server` and route into your own widget.
3. **Constant-fold allowlist** — `ptr_register_constant_fold` adds a symbol the translator can fold at parse time.
4. **UI text overrides** — `ptr_ui_text` deep-merges your overrides into the default copy.
5. **Wrapper functions around `ptr_app`** — e.g. `ptr_app_bslib`. Wrappers may expose downstream-library args without breaking ggpaintr's API claims (see `04-invariants.md` V.4 + auto-memory `feedback-wrapper-scope`).

There is **no** supported way to:
- Define a new structural keyword (analog of `ppLayerOff` / `ppVerbSwitch`) — these are hard-coded in `paintr-translate.R`.
- Define a placeholder that takes a list of expressions (rejected by ADR 0008).
- Wrap a "bare engine" in your own `moduleServer` (rejected by ADR 0006 — `ptr_server_internal` is not public).
- Run ggpaintr non-interactively / headlessly — see `04-invariants.md` V.3.

## 1. Custom placeholders

The placeholder registry (`.ptr_registry` in `R/paintr-registry.R`) is **process-global** (`04-invariants.md` VI.1). Once you register a keyword in any R session, every subsequent `ptr_app` in that process can use it. To remove one: `ptr_clear_placeholder("myKw")`.

Three constructors exist, one per role. Pick the one whose role matches your widget's semantics:

| Role | Constructor | What it represents |
|---|---|---|
| `value` | `ptr_define_placeholder_value` | A scalar / vector input independent of data (text, number, expression). Like the built-in `ppText` / `ppNum` / `ppExpr`. |
| `consumer` | `ptr_define_placeholder_consumer` | A picker that depends on upstream data's column space (column name, list of column names). Like the built-in `ppVar`. |
| `source` | `ptr_define_placeholder_source` | A data source — file upload, paste box, fetched-from-URL. Provides a data frame for downstream consumers. Like the built-in `ppUpload`. |

### `ptr_define_placeholder_value(keyword, build_ui, resolve_expr, …)`
`R/paintr-registry.R:502-536`. Signature:

```r
ptr_define_placeholder_value(
  keyword,                                          # "myKw" — the pp-keyword users will type
  build_ui,                                         # function(node, ...) → Shiny tag(s)
  resolve_expr,                                     # function(value, node, ...) → R value (or rlang expr)
  validate_input = NULL,                            # optional function(value, ctx) → TRUE / NULL / character msg
  default_arg = NULL,                               # optional default for the first positional arg
  named_args = list(),                              # optional named-arg schema for the keyword
  runtime = NULL,                                   # naked-R semantics; defaults to identity function(x, ...) x
  copy_defaults = list(label = "Enter a value for {param}")
) → runtime_fn                                       # returns the runtime fn so you can assign it directly
```

The hooks receive: `build_ui(node)` (node has `$id`, `$keyword`, `$args`), `resolve_expr(value, node)` (where `value` is what the snapshot loop pulled from `input[[node$id]]`), `validate_input(value, ctx)` (`ctx` is `list(node, keyword, upstream_cols=NULL, data=NULL)` since value placeholders are `data_aware = FALSE`). Return shapes:

- `build_ui` → any Shiny tag. **Important**: emit *just* the widget, not a `uiOutput` shell — the server mounts your widget inside an outer `uiOutput` via `invoke_build_ui` (`04-invariants.md` IV.3).
- `resolve_expr` → return a literal value (string / number) or an `rlang` expr. The substitute walker wraps the result as `ptr_literal` (except for `ppExpr`, which wraps as `ptr_user_expr` — that's hard-coded for the `ppExpr` keyword).
- `validate_input` → `TRUE` or `NULL` = valid. A character vector = error message. Anything else fails closed.

Idiomatic registration:

```r
ppSlider <- ptr_define_placeholder_value(
  "ppSlider",
  build_ui = function(node, ...) {
    shiny::sliderInput(node$id, label = NULL, min = 0, max = 100, value = 50)
  },
  resolve_expr = function(value, node, ...) value
)
# now `ppSlider(50)` works in a formula, and `ppSlider(50)` outside ggpaintr returns 50.
```

### `ptr_define_placeholder_consumer(keyword, build_ui, resolve_expr, …)`
`R/paintr-registry.R:643-677`. Same shape as `_value`, but `build_ui` receives **three** args: `function(node, cols, data, ...)`. `cols` is the upstream column-name vector; `data` is the upstream frame (or `NULL` when not data-aware). `role = "consumer"`, `data_aware = TRUE`. The `copy_defaults$label` default becomes `"Pick a column for {param}"`.

Use for any widget whose choice space depends on data — column pickers, faceting variables, ordered-factor pickers, etc.

### `ptr_define_placeholder_source(keyword, build_ui, resolve_data, …)`
`R/paintr-registry.R:810-858`. Different shape — there are **two** resolve hooks:

- `resolve_data(value, node, ...)` → returns the resolved data frame (e.g. read the file).
- `resolve_expr(value, node, ...)` → returns the code-text form (e.g. the variable name). Defaults to `function(value, node, ...) rlang::sym(value)` if not supplied.
- New arg: `shortcut = FALSE` — set `TRUE` to emit the companion textInput that's the ADR 0024 data-loading entry point. The mutex wiring (ADR 0025 §7) is handled automatically.
- The default `runtime` errors with `` `keyword()` is only meaningful inside `ptr_app()` `` instead of identity. (For sources, the naked-R fallback is to refuse — there's no sensible "return your arg" for a data source.)

### What every constructor validates at registration time

All three call:

- `validate_keyword(keyword)` — must be a valid R name.
- `validate_keyword_no_shadow(keyword)` — must not shadow `ggplot2` / `dplyr` / etc. (`R/paintr-registry.R::.ptr_shadow_pkgs`).
- `validate_hook(<hook>, "<name>", <expected_args>)` — every hook must accept the expected arg names.
- `validate_default_arg(default_arg, keyword)`, `validate_named_args(named_args, keyword)`, `validate_copy_defaults(copy_defaults)`.
- `ptr_check_keyword_lhs_drift(keyword)` — guards against the keyword conflicting with an existing LHS pattern.

If any of these fail, registration aborts before `ptr_registry_register` is called — the registry is never put into a partial state.

### Init-ordering — `ensure_registry_initialized` (ADR 0014)

The first line of every `ptr_define_*` call is `ensure_registry_initialized()` — this fixes the race where a user package's `.onLoad` registered a custom placeholder before ggpaintr's own `.onLoad` had populated `.ptr_registry`. **You can call `ptr_define_*` from inside another package's `.onLoad` without ordering concerns.** (`04-invariants.md` VI.2.)

## 2. L3 custom render — extract from `state$runtime()`

ADR 0006: there is one public server. To customize render, capture `state` and read its reactive:

```r
# In your host server:
state <- ptr_server(formula, id = "foo")

# In your UI (or in the same server scope if using ns):
output$my_plotly <- plotly::renderPlotly({
  res <- state$runtime()                       # reactive read; fires after each Update click
  shiny::req(res, res$ok, res$plot)            # gate on ok + non-null
  plotly::ggplotly(res$plot)
})
```

The `res` list shape (built by `ptr_complete_expr_safe` + `ptr_assemble_plot_safe`, see `02-key-paths.md` Path D):

| Field | Meaning |
|---|---|
| `$ok` | `TRUE` = this run succeeded; `FALSE` = transient error (use `state$last_ok_runtime()` to retain previous) |
| `$plot` | `ggplot` object (or `NULL` on error) |
| `$code_text` | rendered code string for the Code panel |
| `$error` | formatted error string (`NULL` if `ok`) |
| `$pruned` | the post-prune AST (rarely needed in custom render; useful for testing) |
| `$snapshot` | the input snapshot used for this run (ADR 0009 bug-1 follow-up) |
| `$eval_env` | the sealed eval env (rarely needed) |

Three public extractors wrap the `state$runtime()$<field>` access:

- `ptr_extract_plot(state)` — `R/paintr-server.R:3226`. One-liner.
- `ptr_extract_error(state)` — `R/paintr-server.R:3230`. One-liner.
- `ptr_extract_code(state)` — `R/paintr-server.R:3234-3239`. Multi-line: also applies `format_code_with_extras` + the upload prologue so the output matches the Code panel verbatim.

**Retain-on-error pattern.** If your custom render should not blank on transient errors (the package's own outputs don't — see `04-invariants.md` II.4), do:

```r
output$my_plotly <- plotly::renderPlotly({
  res <- state$runtime()
  if (is.null(res) || !isTRUE(res$ok) || is.null(res$plot)) {
    last <- state$last_ok_runtime()
    shiny::req(last, last$plot)
    return(plotly::ggplotly(last$plot))
  }
  plotly::ggplotly(res$plot)
})
```

## 3. Constant-fold allowlist — `ptr_register_constant_fold`

The constant-fold env (`R/paintr-default-args.R::ptr_constant_fold_env`) is the sealed namespace where `pp*(default = …)` args are validated at translate time. The default population includes `c`, `list`, `sum`, `mean`, the named constants `Inf`/`NaN`/`NA`/`pi`, etc. If your custom placeholder's `default_arg` needs to call a non-default symbol (say `lubridate::ymd("2026-01-01")` as a default), register it first:

`R/paintr-default-args.R:102-108`:

```r
ptr_register_constant_fold(name, value)
```

Simple: it `assign()`s `value` into the constant-fold env under `name`. Pair with `ptr_clear_constant_fold(name)` to remove. List the current names with `ptr_constant_fold_keywords()`.

Related `ptr_default_*` factories (also in `paintr-default-args.R`) build validators for common shapes — `ptr_default_string`, `ptr_default_numeric`, `ptr_default_numeric_vector`, `ptr_default_expression`, `ptr_default_symbol_or_string`. Use these in your `default_arg = …` arg to `ptr_define_placeholder_*` rather than hand-rolling a validator.

## 4. UI text overrides — `ptr_ui_text`

`R/paintr-copy.R:612-628`. The function:

1. If you pass an already-built `ptr_ui_text` object, returns it as-is.
2. If `NULL`, returns the defaults.
3. Otherwise: validates your overrides, normalizes them (alias expansion: `colour` → `color`, `__unnamed__` → positional param key), deep-merges with the defaults, and returns class `"ptr_ui_text"`.

Idiomatic use:

```r
ptr_app(
  formula = ...,
  ui_text = ptr_ui_text(list(
    ppText = list(label = "Tell me a story:"),                # override one placeholder's label
    ppVar  = list(__unnamed__ = list(label = "Pick X:")),      # override by positional arg
    header = list(title = "My App")                            # override at the header pane
  ))
)
```

Field-shape rules (`R/paintr-copy.R`):

- `ptr_ui_text_param_aliases()` — the alias map (`colour` ↔ `color`, etc.).
- `ptr_ui_text_leaf_fields()` — the per-leaf settable fields (`label`, `help`, `placeholder`, etc.).
- `ptr_ui_text_component_paths()` — the tree of valid component paths (`header.title`, `pp<keyword>.<param>.label`, etc.).
- `__unnamed__` is the positional-arg key (for `ppVar(x)` the param is `__unnamed__`).

`ptr_resolve_ui_text(ui_text, path, …)` — looks up a specific leaf from a built `ptr_ui_text`. Useful inside custom `build_ui` hooks to honor user overrides.

## 5. Wrapper functions around `ptr_app`

A wrapper like `ptr_app_bslib` (`R/paintr-app-bslib.R`) is the supported way to swap out the page shell or pass theme arguments to a downstream library. **Wrappers may expose downstream-library args without breaking the "public-API demo" claim** (auto-memory `feedback-wrapper-scope`). The scope of "ggpaintr's public API" is the `ptr_*` primitives, not the wrapper's full arg list.

Pattern:

```r
my_app <- function(formula, theme = NULL, ...) {
  # 1. Build any non-ggpaintr UI elements
  page <- if (!is.null(theme)) bslib::page_sidebar(theme = theme, ...) else fluidPage()

  # 2. Compose ggpaintr's L3 pieces into your page shell
  ui <- page(
    ptr_ui_controls(formula, id = "main"),
    ptr_ui_plot("main"),
    ptr_ui_code("main")
  )

  # 3. Server uses the one public ptr_server
  server <- function(input, output, session) {
    state <- ptr_server(formula, id = "main")
    # optional: add custom outputs reading state$runtime()$...
  }

  shiny::shinyApp(ui, server)
}
```

Don't wrap `ptr_server_internal` (the unexported 4-arg engine). The single public server entry is `ptr_server(formula, id, …)` returning `state`.

## Re-verification commands

```sh
# Constructor signatures:
grep -nE "^ptr_define_placeholder_(value|consumer|source) <- function" R/paintr-registry.R

# Constant-fold helpers:
grep -nE "^ptr_(register|clear|default)_" R/paintr-default-args.R | head -20

# UI-text public functions:
grep -nE "^ptr_(ui_text|resolve_ui_text)" R/paintr-copy.R
```

Last re-anchor: **2026-05-27**.
