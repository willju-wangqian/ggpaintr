# Custom placeholder — end-to-end date picker

Use when: the user needs a widget type not in the five built-ins (`var`,
`text`, `num`, `expr`, `upload`). Common examples: date picker, slider,
colour well, row-filter widget.

## Contract — five hooks

Built by `ptr_define_placeholder()`. Only the first two are required:

| Hook               | Signature                                       | Required? |
|--------------------|-------------------------------------------------|-----------|
| `build_ui`         | `function(id, copy, meta, context)`             | **yes**   |
| `resolve_expr`     | `function(value, meta, context)`                | **yes**   |
| `resolve_input`    | `function(input, id, meta, context)`            | no        |
| `bind_ui`          | `function(input, output, metas, context)`       | no        |
| `prepare_eval_env` | `function(input, metas, eval_env, context)`    | no        |

`meta` fields set by the parser: `id`, `keyword`, `layer_name`, `param`,
`index_path`.

`context` fields: `ptr_obj`, `placeholders`, `ui_text`, `envir`,
`eval_env`, `var_column_map` (the last two only populated in Shiny).

Return `ptr_missing_expr()` from `resolve_expr` when the occurrence
should be **removed** from the completed expression (rather than
rendered as `NULL`).

## End-to-end — date picker

```r
library(ggpaintr); library(ggplot2); library(rlang); library(shiny)

sales <- data.frame(
  day   = as.Date("2024-01-01") + 0:4,
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

# Merge with built-ins (var/text/num/expr/upload stay available).
placeholders <- ptr_merge_placeholders(
  list(date = date_placeholder)
)

# Use at any level —

# Level 1
ptr_app(
  "ggplot(data = sales, aes(x = day, y = value)) +
     geom_line() +
     geom_vline(xintercept = date)",
  placeholders = placeholders
)

# Level 3 (headless)
obj <- ptr_parse_formula(
  "ggplot(data = sales, aes(x = day, y = value)) +
     geom_line() +
     geom_vline(xintercept = date)",
  placeholders = placeholders
)

runtime <- ptr_exec(
  obj,
  list(
    geom_line_checkbox  = TRUE,
    geom_vline_checkbox = TRUE,
    geom_vline_2        = as.Date("2024-01-03")
  )
)
runtime$code_text
```

## Notes

- Custom entries with a built-in keyword **override** the built-in (so
  you can swap `var`'s picker for a radio-button layout).
- Use `bind_ui` when widget choices depend on runtime data (the built-in
  `var` does this to refresh column lists after an upload).
- Use `prepare_eval_env` when the widget produces an object that must
  live in the eval environment under a user-chosen name (the built-in
  `upload` does this for the uploaded data frame).
- Copy-default strings support `{param}` and `{layer}` interpolation.
- Input ids like `"geom_vline_2"` are deterministic but not hand-authored
  API — discover them with `ptr_runtime_input_spec(obj)`.
