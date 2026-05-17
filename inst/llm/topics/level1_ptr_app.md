# Level 1 — turn-key app with `ptr_app()`

Use when: the user wants an interactive ggplot explorer and does not need to own the Shiny UI. No Shiny code is written.

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

`ptr_app_bslib()` has the same arguments minus `css`, plus `theme` and `title`, and renders inside a `bslib::page_sidebar()` shell.

`ptr_app_grid(plots, shared_ui = list(), envir, title, draw_all_label, expr_check, css)` builds a grid of N plots that can share widgets at the page level — see "Multiple plots" below.

There is no `placeholders =` argument: custom placeholder keywords are registered against a **process-global** registry via `ptr_define_placeholder_value()` / `_consumer()` / `_source()` before the app launches (see `custom_placeholder`).

## Minimal example

```r
library(ggpaintr)

ptr_app(
  "ggplot(iris, aes(var, var, color = var)) + geom_point() + labs(title = text)"
)
```

Every `var`, `text` becomes a sidebar widget. Clicking **Update plot** re-renders the plot and refreshes the generated code on the side.

## Multiple plots in one app — `ptr_app_grid()`

`ptr_app_grid()` takes a list of formulas in `plots =` and renders one tile per formula. Internally it now builds the same shared coordinator the L2 trio exposes, but as an L1 entry point it hides that machinery behind `plots =` / `shared_ui =`.

```r
ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = var, y = Sepal.Length, fill = Species)) +
       geom_boxplot()",
    "ggplot(iris, aes(x = var, y = Sepal.Width, fill = Species)) +
       geom_violin()"
  ),
  shared_ui = list()  # no shared= annotations above; the first var stays per-tile
)
```

Annotate any placeholder with `keyword(shared = "<id>")` to lift it into a single top-level widget that drives every plot, and supply a builder under the matching `<id>` in `shared_ui`. The `<id>` namespace is shared across every formula in `plots`, so editing the top widget propagates to every plot. Clicking **Draw all** triggers a redraw across every tile.

```r
ptr_app_grid(
  plots = list(
    "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Length, fill = Species)) +
       geom_boxplot()",
    "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Width, fill = Species)) +
       geom_violin()"
  ),
  shared_ui = list(metric = function(id) selectInput(id, "Metric", names(iris)))
)
```

The shared mechanism works for any keyword — a built-in `num(shared = "sz")` paired with `shared_ui = list(sz = function(id) sliderInput(id, "Size", 1, 10, 3))` is the simplest two-line example; a `var(shared = "...")` consumer needs no `shared_ui` entry (its picker is built for you).

## Single-instance shared widgets — no coordinator

One ggpaintr instance never sees `ptr_shared()` / `ptr_shared_panel()`. Every shared key is formula-local by definition and auto-renders in the inline shared section. Write the annotation and you are done:

```r
ptr_app(
  "ggplot(iris, aes(x = var(shared = 'col'), y = var(shared = 'col'),
                    color = Species)) + geom_point()"
)
```

The standalone shared panel (and the coordinator that feeds it) only exists for **multiple** instances — see `level2_shared`.

## Data sources — three paths

1. **Named frame in the calling environment.** `data = iris` inside the formula string; `iris` is resolved via `envir`.
2. **`upload` keyword.** Replace `data = iris` with `data = upload` (or use `upload` anywhere a data frame is needed); the user picks a `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, or `.json` file at runtime.
3. **Non-syntactic column names.** Wrap the frame with `ptr_normalize_column_names()` before passing it in; uploads get the same normalization automatically.

## When to move up a level

- Need a specific Shiny layout but ggpaintr's default block is fine → Level 2 (`ptr_module_ui()` / `ptr_module_server()`; see `level2_module`).
- Need multiple linked instances sharing one widget → Level 2 shared trio (see `level2_shared`).
- Need to hand-place every pane, or render the plot yourself (Plotly, ggiraph), or post-process the ggplot object → Level 3 (`ptr_module_server()` / `ptr_server()` returns the state; see `level3_layout` and `level3_custom_render`).
- Need a widget type not in the five built-ins → custom placeholder (see `custom_placeholder`).
