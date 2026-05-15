# Level 1 — turn-key app with `ptr_app()`

Use when: the user wants an interactive ggplot explorer and does not
need to own the Shiny UI. No Shiny code is written.

## Signature

```r
ptr_app(
  formula,
  envir             = parent.frame(),
  ui_text           = NULL,
  checkbox_defaults = NULL,
  expr_check        = TRUE,
  safe_to_remove    = character(),
  css               = NULL
)
```

`ptr_app_bslib()` has the same arguments minus `css`, plus `theme` and
`title`, and renders inside a `bslib::page_sidebar()` shell.

`ptr_app_grid(plots, shared_ui = list(), envir, title, draw_all_label,
expr_check, css)` builds a grid of N plots that can share widgets at the
page level — see `level1_ptr_app` for grid usage in the "Multiple plots"
section below.

There is no `placeholders =` argument: custom placeholder keywords are
registered against a **process-global** registry via
`ptr_define_placeholder_value()` / `_consumer()` / `_source()` before the
app launches (see `custom_placeholder`).

## Minimal example

```r
library(ggpaintr)

ptr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text)
")
```

Every `var`, `num`, `text` becomes a sidebar widget. Clicking
**Update plot** re-renders the plot and refreshes the generated code
on the side.

## Multiple plots in one app — `ptr_app_grid()`

```r
ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = var, y = Sepal.Length, fill = Species)) +
       geom_boxplot()",
    "ggplot(iris, aes(x = var, y = Sepal.Width, fill = Species)) +
       geom_violin()"
  ),
  shared_ui = list()
)
```

Annotate any placeholder with `keyword(shared = "<id>")` to lift it into
a single top-level widget that drives every plot. Supply a builder under
the matching `<id>` in `shared_ui`: e.g.
`shared_ui = list(metric = function(id) selectInput(id, "Metric", names(iris)))`
together with `var(shared = "metric")` in each formula. Clicking
**Draw all** triggers a redraw across every tile.

## Data sources — three paths

1. **Named frame in the calling environment.** `data = iris` inside the
   formula string; `iris` is resolved via `envir`.
2. **`upload` keyword.** Replace `data = iris` with `data = upload` (or
   use `upload` anywhere a data frame is needed); the user picks a
   `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, or `.json` file at runtime.
3. **Non-syntactic column names.** Wrap the frame with
   `ptr_normalize_column_names()` before passing it in; uploads get the
   same normalization automatically.

## When to move up a level

- Need a specific Shiny layout (tabs, custom sidebar) → Level 2
  (`ptr_module_ui()` / `ptr_module_server()`, or
  `ptr_controls_ui()` + `ptr_outputs_ui()` + `ptr_server()`).
- Need to render the plot yourself (Plotly, ggiraph), or post-process
  the ggplot object → Level 3 (`ptr_module_server()` returns the state;
  read `state$runtime()$plot` in your own renderer).
- Need a widget type not in the five built-ins → custom placeholder.
