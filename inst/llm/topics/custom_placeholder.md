# Custom placeholder — three constructors, end-to-end examples

Use when: the user needs a widget type not in the five built-ins (`var`,
`text`, `num`, `expr`, `upload`). Common examples: date picker, slider,
colour well, row-filter widget, database-backed dataset selector.

## Three constructors — pick by data role

ggpaintr's placeholder registry classifies every widget by what it
*does* to the data flow. Pick the constructor that matches.

| Constructor                            | When the widget …                                            |
|----------------------------------------|--------------------------------------------------------------|
| `ptr_define_placeholder_value()`       | … is not data-aware. Sliders, dates, colours, free text.     |
| `ptr_define_placeholder_consumer()`    | … needs upstream column names (column picker).               |
| `ptr_define_placeholder_source()`      | … *produces* a data frame (upload, dataset selector, query). |

The registry is **process-global**. Register once per R session before
launching any app that references the new keyword; re-registering an
existing keyword warns and overwrites. There is **no `placeholders =`
argument** — `ptr_app()`, `ptr_module_server()`, etc. read from the
process-global registry. Use `ptr_clear_placeholder()` to wipe user
entries between sessions.

## Hook contract — the `node` argument

Every hook receives a `node` list with:

- `node$id` — the rendered Shiny input id, already namespaced.
- `node$keyword` — the placeholder keyword.
- `node$layer_name` — the enclosing layer's name (e.g. `"geom_point"`).
- `node$param` — the argument name the placeholder occupies
  (`"x"`, `"size"`, `"__unnamed__"`, …).
- `node$index_path` — positional path into the call (integer vector).
- `node$companion_id` — only for sources that use `companion_id_fn`.

Hooks accept `...` so the framework can pass extra arguments (`label`,
`copy`, `cols`, `data`, `selected`) without breaking older definitions.

`resolve_expr()` returning `NULL` is the "drop this argument" signal —
the substitute pass strips the corresponding argument from the
generated code, exactly like the built-in `var` keyword returning
`NULL` for an empty pick.

## Value placeholder — date picker

```r
library(ggpaintr); library(shiny); library(ggplot2); library(rlang)

ptr_define_placeholder_value(
  keyword = "date",
  build_ui = function(node, label = NULL, ...) {
    shiny::dateInput(node$id, label = label %||% "Date")
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || identical(as.character(value), "")) return(NULL)
    rlang::expr(as.Date(!!as.character(value)))
  },
  copy_defaults = list(label = "Choose a date for {param}")
)

sales <- data.frame(
  day   = as.Date("2024-01-01") + 0:4,
  value = c(10, 13, 12, 16, 18)
)

ptr_app(
  "ggplot(data = sales, aes(x = day, y = value)) +
     geom_line() +
     geom_vline(xintercept = date)"
)
```

## Value placeholder — range slider

```r
ptr_define_placeholder_value(
  keyword = "range",
  build_ui = function(node, label = NULL, ...) {
    shiny::sliderInput(node$id, label = label %||% "Range",
                       min = 0, max = 100, value = c(0, 100), step = 0.1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Range for {param}")
)
```

## Consumer placeholder — multi-column selector

A consumer's `build_ui` receives `cols` (the resolved upstream column
names) and optionally `data` (the resolved upstream data frame — `NULL`
until upstream resolves). The runtime walks the pipeline and refreshes
`cols` whenever upstream stages change.

```r
ptr_define_placeholder_consumer(
  keyword = "colvars",
  build_ui = function(node, cols = character(), label = NULL,
                      selected = character(0), ...) {
    shiny::selectInput(node$id, label = label %||% "Columns",
                       choices = cols,
                       selected = intersect(selected, cols),
                       multiple = TRUE)
  },
  resolve_expr = function(value, node, ...) {
    if (length(value) == 0L) return(NULL)
    rlang::call2("c", !!!as.list(value))
  },
  copy_defaults = list(label = "Columns for {param}")
)
```

`validate_input = function(value, upstream_cols)` is an optional
sanity-check hook: return `TRUE` / `NULL` for valid input, or an error
message string when the input is bad (e.g. "Column `foo` is no longer
in the upstream data").

## Source placeholder — built-in dataset selector

A source produces the data frame downstream consumers read from.

```r
ptr_define_placeholder_source(
  keyword = "dataset",
  build_ui = function(node, label = NULL, ...) {
    shiny::selectInput(
      node$id, label = label %||% "Built-in dataset",
      choices = c("iris", "mtcars", "diamonds", "economics")
    )
  },
  resolve_data = function(value, node, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    switch(value,
      iris      = iris,
      mtcars    = mtcars,
      diamonds  = ggplot2::diamonds,
      economics = ggplot2::economics,
      NULL)
  },
  copy_defaults = list(label = "Built-in dataset for {param}")
)
```

`resolve_data` returns the data frame. `resolve_expr` defaults to
substituting the bare keyword (`rlang::sym(value)`) — that is right when
`value` names a global object. Override `resolve_expr` when the
generated code should re-fetch rather than carry an object reference
(e.g. `function(value, node, ...) rlang::expr(read.csv(!!path))`).

`companion_id_fn = function(id) <companion_id>` pairs the widget with a
sibling input (the built-in `upload` uses this for the "Optional
dataset name" textbox). Most sources do not need it.

## Unregistering

```r
ptr_clear_placeholder("date")      # remove one keyword
ptr_clear_placeholder("colvars")
ptr_clear_placeholder("dataset")
ptr_clear_placeholder()            # remove every user-registered keyword
```

The five built-ins (`var`, `text`, `num`, `expr`, `upload`) are
protected — passing one to `ptr_clear_placeholder()` errors.
Re-registering an existing keyword via any of the three constructors
overwrites the previous entry (with a `cli` notice); clearing first is
not required.

## Notes

- Custom entries with a built-in keyword **overwrite** the built-in (so
  you can swap `var`'s picker for a radio-button layout). The five
  built-ins themselves cannot be `ptr_clear_placeholder()`d.
- Copy-default strings support `{param}` and `{layer}` interpolation;
  the merged tree lives under `ui_text$defaults[[keyword]]`.
- Input ids generated by the parser are deterministic — see
  `level2_custom_ids` for the naming convention if you need to reach
  one from a test or a host observer.
