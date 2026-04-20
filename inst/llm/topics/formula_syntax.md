# ggpaintr — formula syntax

A ggpaintr formula is a normal ggplot call written as a string, with
placeholder keywords anywhere a value would normally go. Each keyword
maps to one widget and one runtime value.

| Keyword  | Widget                         | Runtime value                 | Typical use                                        |
|----------|--------------------------------|-------------------------------|----------------------------------------------------|
| `var`    | column picker (`shinyWidgets::pickerInput`) | column symbol    | pick a column from the data frame                  |
| `text`   | `textInput`                    | string                        | axis labels, titles, color names                   |
| `num`    | `numericInput`                 | numeric                       | point size, alpha, threshold                       |
| `expr`   | code input                     | parsed R expression           | facet specs, label formulas                        |
| `upload` | `fileInput` + dataset name     | data frame (`.csv` / `.rds`)  | receive a dataset at runtime                       |

## Example

```r
library(ggpaintr)

ptr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

Every `var`, `num`, `text`, `expr` becomes a sidebar widget. Clicking
"Update plot" re-runs the runtime and refreshes the code pane.

## Rules

- **Data entry.** The formula references data by name (`data = iris`).
  The name resolves in the caller environment. There is no `data=`
  argument on `ptr_app()`.
- **`var` is a column picker, not an expression builder.** Formula-level
  transforms like `aes(x = var + 1, y = log(var))` go in the formula
  text — the picker still returns the column symbol, and the transform
  is applied around it.
- **`expr` is safety-checked.** Parsed by `rlang::parse_expr()` with a
  denylist guard. Controlled by `expr_check = TRUE` (default — leave on).
- **`upload` accepts `.csv` and `.rds`.** Column names are normalized
  after read-in. For local frames with non-syntactic names, call
  `ptr_normalize_column_names()` once before passing in.
- **Parameter aliases.** `colour → color`, `size → linewidth` — overrides
  can use either spelling.
- **Unnamed positional args** (e.g. `facet_wrap(expr)`) resolve under
  the literal key `__unnamed__` in `ui_text$layers`.
