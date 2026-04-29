# Custom Placeholders with the ggpaintr Registry

## Why a placeholder registry?

The five built-in placeholder keywords (`var`, `text`, `num`, `expr`,
`upload`) cover the common cases, but real apps need more: date pickers,
sliders bound to data extents, colour pickers, row-filter widgets, and
so on. The **placeholder registry** is the extension point that lets you
add such widgets without patching the parser, the runtime, or the copy
system.

Concretely, the registry lets you:

- **Add a new placeholder keyword.** Register a `date` placeholder and
  use `date` inside any formula, e.g. `geom_vline(xintercept = date)`.
- **Replace a built-in widget.** Swap `var`’s
  [`selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html) for
  [`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
  without touching package internals.
- **Ship placeholders from another package.** A registry is a plain
  list, so an external package can export `placeholders = ...` that
  downstream apps pass into
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).

Before reading further, make sure you are comfortable with the formula
syntax and the built-in placeholders — those are covered in
[`vignette("ggpaintr-workflow")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-workflow.md).
This vignette focuses on what you add **on top** of that foundation.

## The placeholder contract at a glance

A placeholder is the value returned by
[`ptr_define_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder.md).
It bundles up to five hooks and a copy-defaults list. Only two hooks are
required:

| Hook               | Signature                                   | Required?    | Purpose                                                                                                                                  |
|--------------------|---------------------------------------------|--------------|------------------------------------------------------------------------------------------------------------------------------------------|
| `build_ui`         | `function(id, copy, meta, context)`         | **required** | Return the Shiny tag for one placeholder occurrence.                                                                                     |
| `resolve_expr`     | `function(value, meta, context)`            | **required** | Turn the current widget value into an R expression spliced into the generated ggplot call.                                               |
| `resolve_input`    | `function(input, id, meta, context)`        | optional     | Pre-process the raw Shiny input before it reaches `resolve_expr`. Defaults to `input[[id]]`.                                             |
| `bind_ui`          | `function(input, output, metas, context)`   | optional     | Reactive wiring inside the server (e.g. `renderUI`, `observe`, `updateSelectInput`). Runs once per keyword with the list of occurrences. |
| `prepare_eval_env` | `function(input, metas, eval_env, context)` | optional     | Inject objects into the environment where the completed formula is evaluated; must return `eval_env`.                                    |

Every hook receives two shared records:

**`meta`** — describes the specific occurrence of the placeholder in the
formula. Fields set by the parser:

- `id` — Shiny input id ggpaintr generated for this occurrence.
- `keyword` — the placeholder keyword (e.g. `"date"`).
- `layer_name` — the ggplot layer, e.g. `"geom_vline"`.
- `param` — the argument name (`"xintercept"`), or `""` for positional
  args.
- `index_path` — integer vector pointing into the parsed expression
  tree.

**`context`** — shared read-only environment:

- `context$ptr_obj` — the parsed `ptr_obj`, including `placeholder_map`.
- `context$placeholders` — the active placeholder registry.
- `context$ui_text` — merged copy rules.
- `context$envir` — caller environment (used to resolve data symbols).
- `context$eval_env`, `context$var_column_map` — set in Shiny mode;
  `NULL` elsewhere.

The copy-defaults list may set any of the four leaf fields ggpaintr
understands: `label`, `help`, `placeholder`, `empty_text`. Strings
support `{param}` and [layer](https://github.com/marcosci/layer)
interpolation. Use
[`ptr_missing_expr()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_missing_expr.md)
from `resolve_expr` when an occurrence should be **removed** from the
completed expression (rather than rendered as `NULL`).

## Built-in placeholders

The five built-ins use the same registry path as any custom placeholder.
The table below shows which hooks each one defines — a useful reference
when you want to mimic or replace one.

| Keyword  | Widget                                                                                                                   | Value produced                             | Hooks used                                                      |
|----------|--------------------------------------------------------------------------------------------------------------------------|--------------------------------------------|-----------------------------------------------------------------|
| `var`    | Dynamic [`shinyWidgets::pickerInput`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html) of data columns | Column name as a symbol                    | `build_ui`, `resolve_expr`, `bind_ui`                           |
| `text`   | `textInput`                                                                                                              | String literal                             | `build_ui`, `resolve_expr`                                      |
| `num`    | `numericInput`                                                                                                           | Numeric scalar                             | `build_ui`, `resolve_expr`                                      |
| `expr`   | Text field parsed as R                                                                                                   | Arbitrary expression (safety-checked)      | `build_ui`, `resolve_expr`                                      |
| `upload` | File input + format picker                                                                                               | User-supplied data frame bound in eval env | `build_ui`, `resolve_expr`, `resolve_input`, `prepare_eval_env` |

`var` uses `bind_ui` because its choices can only be populated after the
data is known at runtime. `upload` uses `prepare_eval_env` because the
uploaded data frame has to live in the eval environment under the name
the user typed.

## A minimal custom placeholder — a date picker

Here is a complete end-to-end example: define a `date` placeholder, use
it in a formula, inspect the generated metadata, and launch a Shiny app
that drives it.

``` r
sales <- data.frame(
  day = as.Date("2024-01-01") + 0:4,
  value = c(10, 13, 12, 16, 18)
)

date_placeholder <- ptr_define_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(as.character(value), "")) {
      return(ptr_missing_expr())
    }
    rlang::expr(as.Date(!!as.character(value)))
  },
  copy_defaults = list(label = "Choose a date for {param}")
)
```

[`ptr_merge_placeholders()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_placeholders.md)
combines your custom entries with the built-ins so the registry you pass
downstream still understands `var`, `text`, etc. Custom entries with a
built-in keyword override the built-in.

``` r
placeholders <- ptr_merge_placeholders(
  list(date = date_placeholder)
)

names(placeholders)
#> [1] "var"    "text"   "num"    "expr"   "upload" "date"
```

Parse a formula with the enriched registry and inspect the metadata
record the parser built for the `date` occurrence:

``` r
obj <- ptr_parse_formula(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)

obj$placeholder_map$geom_vline[["geom_vline_2"]]
#> $id
#> [1] "geom_vline_2"
#> 
#> $keyword
#> [1] "date"
#> 
#> $layer_name
#> [1] "geom_vline"
#> 
#> $param
#> [1] "xintercept"
#> 
#> $index_path
#> [1] 2
```

The fields match the `meta` record every hook receives — `id`,
`keyword`, `layer_name`, `param`, `index_path`.

Execute the plot headlessly with a concrete date. Prefer
`ptr_runtime_input_spec(obj)` for discovering the expected input ids
programmatically rather than hard-coding the `"geom_vline_2"` pattern
used below for clarity.

``` r
runtime <- ptr_exec(
  obj,
  list(
    "geom_line_checkbox"  = TRUE,
    "geom_vline_checkbox" = TRUE,
    "geom_vline_2"        = as.Date("2024-01-03")
  )
)

runtime$code_text
#> [1] "ggplot(data = sales, aes(x = day, y = value)) +\n  geom_line() +\n  geom_vline(xintercept = as.Date(\"2024-01-03\"))"
```

To launch a full Shiny app that drives the same placeholder
interactively, use
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
(see the final section for every API that accepts `placeholders = ...`):

``` r
ptr_app(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)
```

## The five hooks in detail

### `build_ui(id, copy, meta, context)` — required

Returns the Shiny tag for one placeholder occurrence. Called once per
occurrence when ggpaintr constructs the control panel. `id` is the Shiny
input id you must bind; `copy` is a named list with the four leaf fields
(`label`, `help`, `placeholder`, `empty_text`) already resolved for this
occurrence.

``` r
date_placeholder$build_ui
#> function (id, copy, meta, context) 
#> {
#>     shiny::dateInput(id, copy$label)
#> }
```

`build_ui` has no access to reactives. If the widget needs to depend on
current input values, return a container tag
(e.g. [`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html))
and finish wiring it inside `bind_ui()`.

### `resolve_expr(value, meta, context)` — required

Turns the current widget value into an R expression that replaces the
placeholder token in the completed code. Use `!!` (`rlang` unquoting) to
splice the runtime value into the returned expression. Return
[`ptr_missing_expr()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_missing_expr.md)
to **drop** the argument from the completed expression entirely.

The returned value must be one of: a call, symbol, pairlist, character
vector, or a scalar (`numeric`, `logical`, `integer`, `double`,
`complex`, or `NULL`). Anything else is rejected with a clear error.

### `resolve_input(input, id, meta, context)` — optional

Runs before `resolve_expr` and lets you massage the raw Shiny input
value. When omitted, ggpaintr passes `input[[id]]` straight through.

``` r
trimmed_text <- ptr_define_placeholder(
  keyword = "trimmed_text",
  build_ui = function(id, copy, meta, context) {
    shiny::textInput(id, copy$label)
  },
  resolve_input = function(input, id, meta, context) {
    trimws(input[[id]])
  },
  resolve_expr = function(value, meta, context) {
    if (identical(value, "")) return(ptr_missing_expr())
    rlang::expr(!!value)
  }
)
```

### `bind_ui(input, output, metas, context)` — optional

`bind_ui` is called once per keyword inside the Shiny server with the
full list of occurrences (`metas`). Use it to register
[`renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html) outputs or
[`observe()`](https://rdrr.io/pkg/shiny/man/observe.html) blocks for
widgets that depend on reactive state. The return value is a named list
of [`renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html)-capable
tags keyed by `meta$id`, which ggpaintr uses for deferred rendering;
returning `NULL` or `invisible(NULL)` is fine.

Two public helpers exist for the data-aware patterns that show up most
often inside `bind_ui`:

- `ptr_resolve_layer_data(ptr_obj, layer_name, input, context, eval_env)`
  — returns `list(has_data, data)` for a parsed layer. Handles the
  common case where `data =` is a bare symbol, an upload placeholder, or
  omitted (falls back to the layer’s first positional argument). Returns
  `has_data = FALSE` when the data argument is a more elaborate call
  expression — see *When `data =` is a transformation chain* below.
- `ptr_ns_id(ns_fn, id)` — namespaces a raw `meta$id` so it survives
  Shiny module wrappers. Use `context$ns_fn` for input ids and
  `context$ui_ns_fn` for output ids; both default to `shiny::NS(NULL)`
  under
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  so the raw id round-trips unchanged, but they wrap a real namespace
  when ggpaintr is embedded inside a module.

### `prepare_eval_env(input, metas, eval_env, context)` — optional

Runs immediately before the completed expression is evaluated. Use it to
bind objects into the eval environment — uploaded data frames, helper
functions, lookup tables. **The hook must return the modified
`eval_env`**; ggpaintr does not mutate it in place.

## A more complex example — `bind_ui` + `prepare_eval_env`

This example defines a `log_num` placeholder that chooses a numeric
column from the formula’s `data` and wraps it in
[`log()`](https://rdrr.io/r/base/Log.html) in the generated code. It
uses `bind_ui` to populate a `selectInput` with the data’s numeric
columns, and `prepare_eval_env` to expose a small safe-log helper so the
generated call stays readable.

The data-resolution step uses the public
[`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md)
helper, and
[`ptr_ns_id()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ns_id.md)
namespaces the input/output ids so the placeholder survives Shiny module
wrappers.

``` r
log_num <- ptr_define_placeholder(
  keyword = "log_num",

  # Return a container; fill it in from bind_ui once data is available.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Populate the selectInput with numeric columns drawn from the data
  # symbol referenced in the formula's ggplot() layer.
  bind_ui = function(input, output, metas, context) {
    for (meta in metas) {
      local({
        m <- meta
        info <- ptr_resolve_layer_data(
          context$ptr_obj, m$layer_name, input, context, context$eval_env
        )
        numeric_cols <- if (isTRUE(info$has_data) && is.data.frame(info$data)) {
          names(info$data)[vapply(info$data, is.numeric, logical(1))]
        } else {
          character()
        }
        input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), m$id)
        output_id <- ptr_ns_id(
          context$ui_ns_fn %||% shiny::NS(NULL),
          paste0(m$id, "_container")
        )
        output[[output_id]] <- shiny::renderUI({
          shiny::selectInput(
            input_id,
            label   = paste("Log-transform column for", m$param),
            choices = numeric_cols
          )
        })
      })
    }
    invisible(NULL)
  },

  # Generate `.log_safe(<col>)` in the completed expression.
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }
    rlang::expr(.log_safe(!!rlang::sym(value)))
  },

  # Bind the helper the generated call references by name.
  prepare_eval_env = function(input, metas, eval_env, context) {
    eval_env$.log_safe <- function(x) log(pmax(x, .Machine$double.eps))
    eval_env
  },

  copy_defaults = list(label = "Log-transform column for {param}")
)
```

A formula using it, parsed with the merged registry:

``` r
registry <- ptr_merge_placeholders(list(log_num = log_num))

obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = wt, y = log_num)) + geom_point()",
  placeholders = registry
)

obj$formula_text
#> [1] "ggplot(data = mtcars, aes(x = wt, y = log_num)) + geom_point()"
```

Running the same formula headlessly — note that `.log_safe` resolves
because `prepare_eval_env()` bound it in the eval environment:

``` r
runtime <- ptr_exec(
  obj,
  list(
    "geom_point_checkbox" = TRUE,
    "ggplot_3_3"          = "mpg"
  )
)

runtime$code_text
#> [1] "ggplot(data = mtcars, aes(x = wt, y = .log_safe(mpg))) +\n  geom_point()"
```

Launch the live app to exercise `bind_ui`:

``` r
ptr_app(
  "ggplot(data = mtcars, aes(x = wt, y = log_num)) + geom_point()",
  placeholders = registry
)
```

## When `data =` is a transformation chain

[`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md)
covers the common cases — `data = some_data_frame`, `data = upload`,
omitted `data` — by walking the parsed layer for a bare data symbol. It
deliberately does *not* try to evaluate arbitrary call expressions, so
when the formula uses something like
`data = transform(subset(iris, ...), ...)` (or a
`dplyr::select(iris, ...) |> mutate(...)` chain) the helper returns
`has_data = FALSE`.

The fix is a thin fallback inside `bind_ui`: when the helper reports no
data, evaluate the raw `data =` expression in `context$eval_env`. The
chain itself is composed of bound symbols and ordinary functions, so
[`eval()`](https://rdrr.io/r/base/eval.html) succeeds and the
post-transform data frame is exactly what the widget needs to populate
its choices.

``` r
numvar <- ptr_define_placeholder(
  keyword = "numvar",
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },
  bind_ui = function(input, output, metas, context) {
    resolve_layer_df <- function(m) {
      info <- ptr_resolve_layer_data(
        context$ptr_obj, m$layer_name, input, context, context$eval_env
      )
      if (isTRUE(info$has_data) && is.data.frame(info$data)) {
        return(info$data)
      }
      # Fallback: evaluate the raw `data =` expression. Handles
      # transform(subset(...)) and other call chains as long as the
      # chain itself does not reference an unbound placeholder symbol.
      data_expr <- context$ptr_obj$expr_list[[m$layer_name]]$data
      if (!is.null(data_expr) && !is.null(context$eval_env)) {
        df <- tryCatch(
          eval(data_expr, envir = context$eval_env),
          error = function(e) NULL
        )
        if (is.data.frame(df)) return(df)
      }
      NULL
    }

    for (meta in metas) {
      local({
        m <- meta
        df <- resolve_layer_df(m)
        choices <- if (is.data.frame(df)) {
          names(df)[vapply(df, is.numeric, logical(1))]
        } else {
          character()
        }
        input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), m$id)
        output_id <- ptr_ns_id(
          context$ui_ns_fn %||% shiny::NS(NULL),
          paste0(m$id, "_container")
        )
        output[[output_id]] <- shiny::renderUI({
          shiny::selectInput(
            input_id,
            paste("Numeric column for", m$param),
            choices = choices
          )
        })
      })
    }
    invisible(NULL)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }
    rlang::sym(value)
  }
)
```

A formula whose `data =` is a nested call:
[`subset()`](https://rdrr.io/r/base/subset.html) filters to one species,
then [`transform()`](https://rdrr.io/r/base/transform.html) adds a
derived numeric column.
[`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md)
cannot reach into that, but the fallback’s
[`eval()`](https://rdrr.io/r/base/eval.html) returns the full
post-transform frame, and the widget exposes the new `double_length`
column alongside the originals.

``` r
registry_chain <- ptr_merge_placeholders(list(numvar = numvar))

obj_chain <- ptr_parse_formula(
  paste0(
    'ggplot(',
    'data = transform(subset(iris, Species == "setosa"), ',
    'double_length = Sepal.Length * 2), ',
    'aes(x = numvar, y = numvar)) + geom_point()'
  ),
  placeholders = registry_chain
)

obj_chain$formula_text
#> [1] "ggplot(data = transform(subset(iris, Species == \"setosa\"), double_length = Sepal.Length * 2), aes(x = numvar, y = numvar)) + geom_point()"
```

``` r
runtime <- ptr_exec(
  obj_chain,
  list(
    "ggplot_3_2"          = "Sepal.Length",   # x = numvar
    "ggplot_3_3"          = "double_length",  # y = numvar
    "geom_point_checkbox" = TRUE
  )
)

cat(runtime$code_text)
#> ggplot(data = transform(subset(iris, Species == "setosa"), double_length = Sepal.Length * 
#>     2), aes(x = Sepal.Length, y = double_length)) +
#>   geom_point()
```

``` r
ptr_app(
  obj_chain$formula_text,
  placeholders = registry_chain
)
```

## A multi-column picker — `colvars`

Some data-shaping verbs take a *vector* of column names rather than a
single one: `subset(data, select = ...)`,
`dplyr::select(data, all_of(...))`,
`tidyr::pivot_longer(data, cols = all_of(...))`. The placeholder below —
`colvars` — exposes that shape: it gives the user a
[`selectInput()`](https://rdrr.io/pkg/shiny/man/selectInput.html) with
`multiple = TRUE` and resolves to a character-vector literal
`c("col_a", "col_b", ...)` that drops directly into any of those call
sites.

Because the placeholder typically sits *inside* the transformation
(e.g. `subset(iris, select = colvars)`),
[`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md)
cannot evaluate the full data expression — `colvars` itself is unbound
at bind time, so eval fails. The fallback walks the data expression
looking for the underlying source frame: the first bare symbol that
resolves to a data frame in `eval_env` wins. That symbol (`iris`,
`mtcars`, an upload object, …) is what you want to populate the column
choices from.

``` r
# Walk the layer's `data =` expression and return the first bare symbol
# that resolves to a data frame in eval_env. Mirrors the "give me the
# upstream source data" need when colvars is wrapped in subset/select/...
find_root_data <- function(e, eval_env) {
  if (is.null(e) || is.null(eval_env)) return(NULL)
  if (is.symbol(e)) {
    obj <- tryCatch(eval(e, envir = eval_env), error = function(err) NULL)
    if (is.data.frame(obj)) return(obj)
    return(NULL)
  }
  if (is.call(e)) {
    for (i in seq_along(e)[-1]) {
      df <- find_root_data(e[[i]], eval_env)
      if (!is.null(df)) return(df)
    }
  }
  NULL
}

colvars <- ptr_define_placeholder(
  keyword = "colvars",
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },
  bind_ui = function(input, output, metas, context) {
    for (meta in metas) {
      local({
        m <- meta
        info <- ptr_resolve_layer_data(
          context$ptr_obj, m$layer_name, input, context, context$eval_env
        )
        df <- if (isTRUE(info$has_data) && is.data.frame(info$data)) {
          info$data
        } else {
          find_root_data(
            context$ptr_obj$expr_list[[m$layer_name]]$data,
            context$eval_env
          )
        }
        choices <- if (is.data.frame(df)) names(df) else character()
        input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), m$id)
        output_id <- ptr_ns_id(
          context$ui_ns_fn %||% shiny::NS(NULL),
          paste0(m$id, "_container")
        )
        output[[output_id]] <- shiny::renderUI({
          shiny::selectInput(
            input_id,
            paste("Columns for", m$param),
            choices  = choices,
            multiple = TRUE
          )
        })
      })
    }
    invisible(NULL)
  },
  resolve_expr = function(value, meta, context) {
    if (length(value) == 0) return(ptr_missing_expr())
    rlang::expr(c(!!!value))
  }
)
```

Used inside [`subset()`](https://rdrr.io/r/base/subset.html) to pick
which columns to retain before plotting:

``` r
registry_colvars <- ptr_merge_placeholders(list(colvars = colvars))

obj_colvars <- ptr_parse_formula(
  paste0(
    'ggplot(data = subset(iris, select = colvars), ',
    'aes(x = Sepal.Length)) + geom_histogram(bins = 30)'
  ),
  placeholders = registry_colvars
)

obj_colvars$formula_text
#> [1] "ggplot(data = subset(iris, select = colvars), aes(x = Sepal.Length)) + geom_histogram(bins = 30)"
```

``` r
runtime <- ptr_exec(
  obj_colvars,
  list(
    "ggplot_2_3"              = c("Sepal.Length", "Petal.Length"),
    "geom_histogram_checkbox" = TRUE
  )
)

cat(runtime$code_text)
#> ggplot(data = subset(iris, select = c("Sepal.Length", "Petal.Length")), 
#>     aes(x = Sepal.Length)) +
#>   geom_histogram(bins = 30)
```

The same `colvars` placeholder drops into a tidyverse-flavoured pipeline
without changing the placeholder definition:
`dplyr::select(iris, dplyr::all_of(colvars))` keeps a chosen subset of
columns, and `tidyr::pivot_longer(iris, cols = dplyr::all_of(colvars))`
reshapes the chosen numeric columns into long format. In each case the
resolved expression is a plain character vector, so any function that
accepts character column-names accepts the placeholder.

``` r
ptr_app(
  obj_colvars$formula_text,
  placeholders = registry_colvars
)
```

## Replacing a built-in placeholder

Any custom keyword that matches a built-in one wins — the built-in is
evicted from the merged registry. Below, we replace `var` with a
[`shinyWidgets::pickerInput()`](https://dreamrs.github.io/shinyWidgets/reference/pickerInput.html)
that offers search and tick-boxes while keeping the original reactive
behaviour (columns populated from data).

``` r
picker_var <- ptr_define_placeholder(
  keyword = "var",

  # Container filled in by bind_ui once data is known.
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },

  # Produce the same symbol the default `var` does.
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "") || identical(value, "(none)")) {
      return(ptr_missing_expr())
    }
    rlang::sym(value)
  },

  # Mirror the reactive-column behaviour of the default `var`.
  bind_ui = function(input, output, metas, context) {
    eval_env <- context$eval_env
    ggplot_expr <- context$ptr_obj$expr_list[["ggplot"]]
    data_expr <- if (is.call(ggplot_expr)) ggplot_expr$data else NULL
    cols <- character(0)
    if (!is.null(eval_env) && !is.null(data_expr)) {
      d <- tryCatch(eval(data_expr, envir = eval_env), error = function(e) NULL)
      if (is.data.frame(d)) {
        cols <- names(d)
      }
    }
    for (meta in metas) {
      local({
        m <- meta
        output[[paste0(m$id, "_container")]] <- shiny::renderUI({
          shinyWidgets::pickerInput(
            inputId = m$id,
            label   = paste("Column for", m$param),
            choices = c("(none)", cols),
            options = shinyWidgets::pickerOptions(liveSearch = TRUE)
          )
        })
      })
    }
    invisible(NULL)
  },

  copy_defaults = list(
    label      = "Column for {param}",
    empty_text = "(none)"
  )
)

replacement_registry <- ptr_merge_placeholders(list(var = picker_var))
identical(replacement_registry$var, picker_var)
#> [1] TRUE
```

``` r
ptr_app(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
  placeholders = replacement_registry
)
```

## Plugging a registry into every API

Every public entry point that accepts formulas also accepts a
`placeholders = ...` argument holding a merged registry. Pass the same
registry object everywhere so built-in widgets, custom widgets, and copy
rules stay in sync.

### `ptr_app()` and `ptr_app_bslib()` — Level 1

``` r
ptr_app(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)

ptr_app_bslib(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)
```

### `ptr_parse_formula()` + `ptr_exec()` — Level 3 headless

Useful for batch reports, tests, and custom pipelines. The registry is
stored on the returned `ptr_obj`, so downstream calls such as
[`ptr_runtime_input_spec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_runtime_input_spec.md)
and
[`ptr_exec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_exec.md)
pick it up automatically.

``` r
obj <- ptr_parse_formula(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)

spec <- ptr_runtime_input_spec(obj)
names(spec)
#> [1] "input_id"   "role"       "layer_name" "keyword"    "param_key" 
#> [6] "source_id"
```

### Manual embedding with `ptr_input_ui()` / `ptr_server_state()` — Level 2

For embedding ggpaintr inside an existing Shiny app, pass `placeholders`
through
[`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
— it flows from there into the controls, plot, and code registrations
automatically.

``` r
ids <- ptr_build_ids(
  control_panel = "builder_controls",
  draw_button   = "render_plot",
  plot_output   = "main_plot",
  error_output  = "main_error",
  code_output   = "main_code"
)

ui <- shiny::fluidPage(
  shiny::sidebarLayout(
    shiny::sidebarPanel(ptr_input_ui(ids = ids)),
    shiny::mainPanel(ptr_output_ui(ids = ids))
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = sales, aes(x = day, y = value)) +
      geom_line() +
      geom_vline(xintercept = date)",
    ids          = ids,
    placeholders = placeholders
  )

  ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_plot(output, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)
}

shiny::shinyApp(ui, server)
```

See
[`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md)
§3 for the full Level-2 embed walkthrough.

## Copy defaults inside a placeholder definition

`copy_defaults` ships default copy with the placeholder itself, so every
downstream app that uses it gets sensible labels without extra work. The
supported fields match the four leaf fields in
[`ptr_resolve_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_ui_text.md):
`label`, `help`, `placeholder`, `empty_text`. Strings may reference
`{param}` and [layer](https://github.com/marcosci/layer).

``` r
annotated_date <- ptr_define_placeholder(
  keyword  = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::tagList(
      shiny::dateInput(id, copy$label),
      if (nzchar(copy$help)) shiny::helpText(copy$help)
    )
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value)) return(ptr_missing_expr())
    rlang::expr(as.Date(!!as.character(value)))
  },
  copy_defaults = list(
    label = "Choose a {param} date",
    help  = "Used by {layer}."
  )
)

# ptr_resolve_ui_text() shows the final text a `build_ui` call would receive.
ptr_resolve_ui_text(
  component    = "control",
  keyword      = "date",
  layer_name   = "geom_vline",
  param        = "xintercept",
  placeholders = ptr_merge_placeholders(list(date = annotated_date))
)
#> $label
#> [1] "Choose a xintercept date"
#> 
#> $help
#> [1] "Used by geom_vline."
```

Everything beyond `copy_defaults` — the three-layer merge precedence
(`defaults` / `params` / `layers`),
[`ptr_merge_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_ui_text.md)
and the `shell` chrome copy — is covered in
[`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md)
§3e. That section walks through the full override catalogue and shows a
worked example of a single placeholder receiving different copy under
each layer.

## Summary

1.  A placeholder is a bundle of two required hooks (`build_ui`,
    `resolve_expr`) and three optional ones (`resolve_input`, `bind_ui`,
    `prepare_eval_env`) plus `copy_defaults`.
2.  [`ptr_merge_placeholders()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_placeholders.md)
    turns your custom entries into a registry that still knows the
    built-ins, and custom entries win on name clash.
3.  Every public API that accepts formulas also accepts a
    `placeholders = ...` argument.
4.  For copy customisation beyond `copy_defaults`, use the `ui_text`
    system — see
    [`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md)
    §3e.
