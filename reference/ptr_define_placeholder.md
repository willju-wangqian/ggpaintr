# Construct a Custom ggpaintr Placeholder

Build one placeholder specification for use with
[`ptr_merge_placeholders()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_placeholders.md).
Custom placeholders can define their own UI control, runtime expression
replacement, deferred UI binding, and evaluation-environment
preparation.

## Usage

``` r
ptr_define_placeholder(
  keyword,
  build_ui,
  resolve_expr,
  resolve_input = NULL,
  bind_ui = NULL,
  prepare_eval_env = NULL,
  copy_defaults = list(label = "Enter a value for {param}")
)
```

## Arguments

- keyword:

  A single syntactic placeholder name used inside the formula.

- build_ui:

  Function with signature `(id, copy, meta, context)` returning a Shiny
  UI control or placeholder. `id` is already namespaced.

- resolve_expr:

  Function with signature `(value, meta, context)` returning an R
  expression or
  [`ptr_missing_expr()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_missing_expr.md).

- resolve_input:

  Optional function with signature `(input, id, meta, context)`
  returning the raw value to hand to `resolve_expr()`. Defaults to
  `input[[id]]`.

- bind_ui:

  Optional function with signature `(input, output, metas, context)` for
  registering deferred UI. See *Data-aware placeholders* above for the
  contract. The built-in `var` placeholder uses this hook.

- prepare_eval_env:

  Optional function with signature `(input, metas, eval_env, context)`
  returning an updated evaluation environment.

- copy_defaults:

  Optional named list with `label`, `help`, `placeholder`, and
  `empty_text`. Defaults to `list(label = "Enter a value for {param}")`.

## Value

An object of class `ptr_define_placeholder`.

## Data-independent placeholders (the common case)

If the widget choices do not depend on the active dataset (date pickers,
sliders, color pickers, free-text variants), supply `build_ui` and
`resolve_expr` and you are done. The `id` passed to `build_ui` is
already namespaced by ggpaintr; use it directly.

## Data-aware placeholders (`bind_ui`)

If the widget needs to read columns or values from the dataset (a
multi-column selector inside
[`dplyr::select()`](https://dplyr.tidyverse.org/reference/select.html),
a faceting variable picker, etc.), the dataset is not available at
registration time – it depends on the user's formula and on input
values. Use `bind_ui` to register a Shiny `renderUI()` for the widget.

The recommended pattern is the empty-container shape used by the
built-in `var` placeholder: `build_ui` returns a placeholder
[`shiny::uiOutput()`](https://rdrr.io/pkg/shiny/man/htmlOutput.html) and
`bind_ui` fills it in. See *Examples*.

### `bind_ui(input, output, metas, context)` contract

- `metas` is a list of meta objects (one per occurrence of `keyword` in
  the formula). Each meta has `$id` (raw, *not* yet namespaced),
  `$keyword`, `$param` (the ggplot argument name), and `$layer_name`
  (e.g. `"ggplot"`, `"geom_point"`).

- `context` carries:

  - `$ptr_obj` – the parsed formula tree (use
    [`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md)
    to read layer data).

  - `$eval_env` – the evaluation environment used for the plot.

  - `$envir` – the user's calling environment.

  - `$ns_fn`, `$ui_ns_fn` – input/output namespace functions (use
    [`ptr_ns_id()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ns_id.md)
    to namespace `meta$id`).

  - `$var_column_map` – cached column lists for `var` placeholders.

  - `$ui_text` – merged ui text for labels/help.

- Return value: either `NULL` (treated as a no-op; render via
  `output[[...]]` side effects) or a named list keyed by `meta$id`.
  Items in that list are merged into ggpaintr's deferred-UI map and
  rendered in the layer panel. Returning a list is preferred for widgets
  you want ggpaintr to lay out next to other controls.

### Namespacing rule

`meta$id` is the *raw* placeholder id (e.g. `"ggplot_3_2"`). Under
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
the default namespace functions are `shiny::NS(NULL)` so the raw id
round-trips unchanged, but when ggpaintr is embedded inside a Shiny
module those functions wrap a real namespace. Always namespace `meta$id`
via
[`ptr_ns_id()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ns_id.md)
before assigning into `output` or referencing `input`.

## See also

[`ptr_resolve_layer_data()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_resolve_layer_data.md),
[`ptr_ns_id()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ns_id.md),
[`ptr_merge_placeholders()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_merge_placeholders.md).

## Examples

``` r
# ---- Data-independent placeholder: a date picker -------------------------
date_placeholder <- ptr_define_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) {
      return(ptr_missing_expr())
    }

    rlang::expr(as.Date(!!value))
  },
  copy_defaults = list(label = "Choose a date for {param}")
)
date_placeholder$keyword
#> [1] "date"

# ---- Data-aware placeholder: a numeric-columns-only picker --------------
# build_ui returns an empty uiOutput; bind_ui fills it in once the layer
# data has resolved. Use ptr_resolve_layer_data() to fetch the data frame
# and ptr_ns_id() to namespace the input/output ids.
numvar_placeholder <- ptr_define_placeholder(
  keyword = "numvar",
  build_ui = function(id, copy, meta, context) {
    shiny::uiOutput(paste0(id, "_container"))
  },
  bind_ui = function(input, output, metas, context) {
    for (meta in metas) {
      local({
        m <- meta
        layer_data <- ptr_resolve_layer_data(
          context$ptr_obj, m$layer_name, input, context, context$eval_env
        )
        choices <- if (isTRUE(layer_data$has_data) &&
                       is.data.frame(layer_data$data)) {
          df <- layer_data$data
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
numvar_placeholder$keyword
#> [1] "numvar"
```
