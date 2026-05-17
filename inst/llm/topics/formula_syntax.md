# ggpaintr — formula syntax

A ggpaintr formula is a normal ggplot call written as a string, with placeholder keywords anywhere a value would normally go. Each keyword maps to one widget and one runtime value.

| Keyword  | Widget                         | Runtime value                 | Typical use                                        |
|----------|--------------------------------|-------------------------------|----------------------------------------------------|
| `var`    | column picker (`shinyWidgets::pickerInput`) | column symbol    | pick a column from the data frame                  |
| `text`   | `textInput`                    | string                        | axis labels, titles, color names                   |
| `num`    | `numericInput`                 | numeric                       | point size, alpha, threshold                       |
| `expr`   | code input                     | parsed R expression           | facet specs, label formulas                        |
| `upload` | `fileInput` + dataset name     | data frame (`.csv` / `.tsv` / `.rds` / `.xlsx` / `.xls` / `.json`) | receive a dataset at runtime |

## Example

```r
library(ggpaintr)

ptr_app("
ggplot(iris, aes(x = var, y = var, color = var)) +
  geom_point(size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

Every `var`, `num`, `text`, `expr` becomes a sidebar widget. Clicking "Update plot" re-runs the runtime and refreshes the code pane.

## Data pipelines

A layer's data argument may be a pipeline using `|>`, `%>%`, or a mix of both. Each pipeline stage containing a placeholder gets its own row of widgets under the layer's **Data** subtab plus a per-stage enable/disable toggle. Column pickers downstream of a stage re-resolve against the current Data-subtab inputs.

```r
ptr_app(
  "mpg |>
     dplyr::filter(displ > num) |>
     dplyr::group_by(class) |>
     dplyr::filter(dplyr::n() > num) |>
     dplyr::ungroup() |>
     ggplot(aes(var, var, color = class)) +
     geom_point(alpha = num)"
)
```

Supported: bare-symbol stage heads (`filter(...)`), namespaced heads (`dplyr::filter(...)`), parenthesised heads (`(filter)(...)`), `upload` as the pipeline head, and per-layer pipelines on multi-layer formulas.

Not supported (yet): anonymous functions as pipe stages — lift them to a named helper first (`my_helper <- \(x) ...` then `df |> my_helper() |> ggplot(...)`).

## Sharing a widget across layers / plots — and the partition rule

Annotate a placeholder with `keyword(shared = "<id>")` to bind every occurrence with the same `<id>` to one widget value in lockstep — across `aes()`, pipeline stages, and (multi-instance) across formulas.

*How* the shared widget surfaces is decided **per key by how many formulas reference it** — the partition rule:

> A shared key referenced in **exactly one** formula → that formula's inline **shared section**, rendered inside its own control panel. No coordinator.
> A shared key referenced in **two or more** formulas → the one standalone **shared panel**, built from the coordinator object.

This produces a hard split by instance count:

- **One ggpaintr instance** (one `ptr_app()` formula, or one `ptr_module_*()`): every shared key is formula-local by definition and auto-renders in that instance's inline shared section. **No coordinator, no panel — ever.**

```r
"ggplot(iris, aes(x = var(shared = 'col'), y = var(shared = 'col'),
                  color = Species)) + geom_point()"
```

- **Multiple instances** (`ptr_app_grid()` plots, or several `ptr_module_*()` blocks): you build `obj <- ptr_shared(formulas = list(…))` once; cross-formula keys (≥2 formulas) go to the one `ptr_shared_panel(obj)`, formula-local keys still render inline in each instance.

See `level1_ptr_app` for the grid usage and `level2_shared` for the multi-instance coordinator trio.

## Rules

- **Data entry.** The formula references data by name (`data = iris` or bare `iris`). The name resolves in the caller environment. There is no `data=` argument on `ptr_app()`.
- **`var` is a column picker, not an expression builder.** Formula-level transforms like `aes(x = var + 1, y = log(var))` go in the formula text — the picker still returns the column symbol, and the transform is applied around it.
- **`expr` is safety-checked.** Parsed by `rlang::parse_expr()` with a denylist guard. Controlled by `expr_check = TRUE` (default — leave on).
- **`upload` accepts `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, `.json`.** JSON must be an array of records; nested objects are flattened, nested arrays error. Excel and JSON read via the suggested packages `readxl` and `jsonlite`. Column names are normalized after read-in. For local frames with non-syntactic names, call `ptr_normalize_column_names()` once before passing in.
- **Parameter aliases.** `colour → color`, `size → linewidth` — overrides can use either spelling.
- **Unnamed positional args** (e.g. `facet_wrap(expr)`) resolve under the literal key `__unnamed__` in `ui_text$layers`.
- **Empty-call cleanup.** A placeholder that resolves to "missing" (empty pick, blank `text`, cleared `num`) drops its argument. If the enclosing call has no arguments left and its function is in ggpaintr's safe-no-op list (`labs`, `facet_wrap`, `theme`, …), the whole call drops too. `geom_*()` / `stat_*()` layers are never dropped — empty forms still inherit from `ggplot()`. Use `safe_to_remove = c("pcp_theme")` to opt third-party helpers into cleanup.
