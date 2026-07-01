# Define a data-source placeholder (e.g. upload, database table)

A *source* placeholder produces a data frame the rest of the formula
reads from. Built-in example: `ppUpload`. Custom examples: database
tables, built-in datasets, URL fetches.

## Usage

``` r
ptr_define_placeholder_source(
  keyword,
  build_ui,
  resolve_data,
  resolve_expr = NULL,
  shortcut = FALSE,
  parse_positional_arg = NULL,
  parse_named_args = list(),
  embellish_eval = NULL,
  ui_text_defaults = list(label = "Provide a data source for {param}")
)
```

## Arguments

- keyword, ui_text_defaults:

  See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).
  See
  [`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
  § "Defining your own placeholders" (source role).

- build_ui:

  `function(node, label, ...)` returning a Shiny tag — same shape as in
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).
  Render ONLY the source's data-payload widget at `inputId = node$id`
  (e.g. a `fileInput`, a `selectInput` chooser). With `shortcut = TRUE`
  the framework emits the sibling shortcut `textInput` (at
  `node$shortcut_id`) for you (ADR 0025 item \#7) — do NOT render it
  yourself, or the id would be bound twice. A source whose only entry
  point is the shortcut (e.g. an env-name loader) may return `NULL`.

  *Seeding* — same opt-in shape as the other two helpers: declare an
  optional `selected = NULL` formal (or accept `...`) to receive the
  seeded value. Seeding is **boot-only**: a `spec=` entry naming this
  source wins on the first render, and on every later render (e.g. a
  tree rebuild) the user's current pick is authoritative — the spec seed
  never snaps the widget back. The built-in `ppUpload` declines this
  because a Shiny `fileInput()` can never be seeded programmatically;
  custom sources whose primary widget *can* be seeded (e.g. a
  `selectInput` dataset chooser) should accept it.

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
  `function(value, node, ...) rlang::ppExpr(read.csv(!!value$datapath))`.
  With `shortcut = TRUE`, `value` here is the *shortcut* input's value
  (e.g. the typed dataset name), **not** the primary payload — the
  built-in `ppUpload` relies on this so the default splices the typed
  name as a bare symbol.

- shortcut:

  Single logical (default `FALSE`). When `TRUE`, the framework stamps
  `node$shortcut_id <- paste0(node$id, "_shortcut")` on every translated
  source node AND renders the sibling shortcut `textInput` (at
  `node$shortcut_id`) for you — an "Optional dataset name" box beside
  the source widget (ADR 0025 item \#7). Your `build_ui` renders ONLY
  the data-payload widget at `node$id` (or `NULL` for a shortcut-only
  source); it must NOT render the shortcut input itself. Both inputs
  participate in the runtime substitution cycle: one at `node$id` (the
  data payload) and one at `node$shortcut_id` (a typed-in name that
  resolves a `data.frame` from the caller-supplied `envir`). The
  shortcut value reaches `resolve_data` / `resolve_expr` through `node`.
  Most sources do not need it — one bound input is the common case. The
  built-in `ppUpload` sets `shortcut = TRUE`: the file contents bind to
  `node$id`, the user-typed name binds to `node$shortcut_id`, and the
  substitution uses the name as the symbol inserted into the generated
  code. The framework also re-renders the source widget when the
  shortcut goes non-empty, clearing a stale display (ADR 0025 item \#7).
  The reserved shared key `"shortcut"` is rejected at translate time
  (see ADR 0025 §1).

- parse_positional_arg, parse_named_args:

  See
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md).
  Source placeholders use the same arg-schema slots.

- embellish_eval:

  Optional `function(...)` body used when the placeholder is called as a
  plain-R function (outside
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)).
  `NULL` (default) supplies a guard that aborts with a message naming
  the keyword and noting the call is only meaningful inside
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  — source placeholders typically have no out-of-app meaning (a file
  upload widget cannot produce data at the REPL). Override only if the
  source has a sensible plain-R interpretation.

## Value

The runtime callable. Default for a source placeholder is a guard that
aborts when called outside an app context. Also called for its
registration side effect; use
[`ptr_clear_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_clear_placeholder.md)
to remove the entry.

## `spec=` round-trip

The `spec=` mechanism (see
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md))
captures a sparse snapshot of input values so the preserve-mode panel
can publish a reproducible boot state. For a source placeholder, ONE of
two patterns must hold:

- **Shortcut pattern** — set `shortcut = TRUE`. The shortcut input's
  text value (typically the typed dataset name) carries the round-trip
  identity; the source's own value at `node$id` is dropped from the
  spec, because it is typically a per-session Shiny artifact (a
  `fileInput()` data.frame whose `datapath` is a tempfile path that does
  not survive the session). The built-in `ppUpload` uses this.

  **Data-loading entry point (ADR 0024).** When `shortcut = TRUE`, the
  shortcut sibling is more than a name override for an uploaded frame —
  it is a typed-in shortcut for loading a `data.frame` from the
  embedder's environment (`envir` passed to
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  /
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)).
  Any valid R name typed into the shortcut input (or seeded via
  `spec = list(<shortcut-id> = "df_name")`) is looked up via
  `get(name, envir, inherits = TRUE)` and bound as the resolved source
  frame, with OR without `default=` on the placeholder. The downstream
  pipeline, generated code panel, and consumer pickers all read the
  named frame from `state$eval_env` as if it had been uploaded. Failures
  surface on the inline error pane via `set_resolve_error`:
  `"Object 'x' not found in environment."` /
  `"Object 'x' is not a data frame."` Lookup uses `inherits = TRUE`, so
  package exports become reachable — typing `"plot"` will resolve to
  [`graphics::plot`](https://rdrr.io/r/graphics/plot.default.html) and
  then fail the "is not a data frame" check (loudly, not silently).

- **Scalar pattern** — `shortcut = FALSE`. The widget's value at
  `node$id` must be a literal that round-trips through
  [`deparse()`](https://rdrr.io/r/base/deparse.html) — a length-1 string
  / number / logical, or a simple atomic vector. The `selectInput`-style
  example above qualifies (its value is a single string).

Source widgets whose primary value is a complex object (raw
`fileInput()` data.frame, environment, S4 instance, etc.) without
`shortcut = TRUE` cannot round-trip; opt into `shortcut = TRUE` and the
framework renders the sibling `textInput(node$shortcut_id, ...)` that
carries the binding name, mirroring `ppUpload`.

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
#> function (...) 
#> rlang::abort(paste0("`", kw, "()` is only meaningful inside `ptr_app()`."))
#> <bytecode: 0x55df46948338>
#> <environment: 0x55df476911c0>
ptr_clear_placeholder("dataset")
#> ✔ Cleared placeholder: "dataset".
```
