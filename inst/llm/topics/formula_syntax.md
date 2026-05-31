# ggpaintr — formula syntax

A ggpaintr formula is a normal ggplot call written as a string, with placeholder keywords anywhere a value would normally go. Each keyword is a function call that maps to one widget and one runtime value.

<!-- @ptr-gen:token-table -->
| Keyword | Widget | Runtime value |
|---------|--------|---------------|
| `ppVar` | column picker (`shinyWidgets::pickerInput`) | column symbol |
| `ppText` | `textInput` | string |
| `ppNum` | `numericInput` | numeric |
| `ppExpr` | code input (denylist-guarded) | parsed R expression |
| `ppUpload` | `fileInput` (.csv/.tsv/.rds/.xlsx/.xls/.json) | data frame |
<!-- @ptr-gen:end -->

Typical uses: `ppVar` picks a column; `ppText` supplies axis labels / titles / colour names; `ppNum` a point size / alpha / threshold; `ppExpr` a facet spec or label formula; `ppUpload` receives a dataset at runtime.

## Example

```r
library(ggpaintr)

ptr_app("
ggplot(iris, aes(x = ppVar, y = ppVar, color = ppVar)) +
  geom_point(size = ppNum) +
  labs(title = ppText) +
  facet_wrap(ppExpr)
")
```

Every `ppVar`, `ppNum`, `ppText`, `ppExpr` becomes a sidebar widget. Clicking "Update plot" re-runs the runtime and refreshes the code pane.

## Data pipelines

A layer's data argument may be a pipeline using `|>`, `%>%`, or a mix of both. Each pipeline stage containing a placeholder gets its own row of widgets under the layer's **Data** subtab plus a per-stage enable/disable toggle. Column pickers downstream of a stage re-resolve against the current Data-subtab inputs.

```r
ptr_app(
  "mpg |>
     dplyr::filter(displ > ppNum) |>
     dplyr::group_by(class) |>
     dplyr::filter(dplyr::n() > ppNum) |>
     dplyr::ungroup() |>
     ggplot(aes(ppVar, ppVar, color = class)) +
     geom_point(alpha = ppNum)"
)
```

Supported: bare-symbol stage heads (`filter(...)`), namespaced heads (`dplyr::filter(...)`), parenthesised heads (`(filter)(...)`), `ppUpload` as the pipeline head, and per-layer pipelines on multi-layer formulas.

Not supported (yet): anonymous functions as pipe stages — lift them to a named helper first (`my_helper <- \(x) ...` then `df |> my_helper() |> ggplot(...)`).

## Sharing a widget across layers / plots — and the partition rule

Annotate a placeholder with `ppX(shared = "<id>")` to bind every occurrence with the same `<id>` to one widget value in lockstep — across `aes()`, pipeline stages, and (multi-instance) across formulas.

*How* the shared widget surfaces is decided **per key by how many formulas reference it** — the partition rule:

> A shared key referenced in **exactly one** formula → that formula's inline **shared section**, rendered inside its own control panel. No coordinator.
> A shared key referenced in **two or more** formulas → the one standalone **shared panel**, built from the coordinator object.

This produces a hard split by instance count:

- **One ggpaintr instance** (one `ptr_app()` formula, or one `ptr_ui()` / `ptr_server()` block): every shared key is formula-local by definition and auto-renders in that instance's inline shared section. **No coordinator, no panel — ever.**

```r
"ggplot(iris, aes(x = ppVar(shared = 'col'), y = ppVar(shared = 'col'),
                  color = Species)) + geom_point()"
```

- **Multiple instances** (several `ptr_ui()` / `ptr_server()` blocks): you build `obj <- ptr_shared(formulas = list(…))` once; cross-formula keys (≥2 formulas) go to the one `ptr_shared_panel(obj)`, formula-local keys still render inline in each instance.

See `level2_shared` for the multi-instance coordinator trio.

## Rules

- **Data entry.** The formula references data by name (`data = iris` or bare `iris`). The name resolves in the caller environment. There is no `data=` argument on `ptr_app()`.
- **`ppVar` is a column picker, not an expression builder.** Formula-level transforms like `aes(x = ppVar + 1, y = log(ppVar))` go in the formula text — the picker still returns the column symbol, and the transform is applied around it.
- **`ppExpr` is safety-checked.** Parsed by `rlang::parse_expr()` with a denylist guard. Controlled by `expr_check = TRUE` (default — leave on).
- **`ppUpload` accepts `.csv`, `.tsv`, `.rds`, `.xlsx`, `.xls`, `.json`.** JSON must be an array of records; nested objects are flattened, nested arrays error. Excel and JSON read via the suggested packages `readxl` and `jsonlite`. Column names are normalized after read-in. For local frames with non-syntactic names, call `ptr_normalize_column_names()` once before passing in.
- **Parameter aliases.** `colour → color`, `size → linewidth` — overrides can use either spelling.
- **Unnamed positional args** (e.g. `facet_wrap(ppExpr)`) resolve under the literal key `__unnamed__` in `ui_text$layers`.
- **Empty-call cleanup.** A placeholder that resolves to "missing" (empty pick, blank `ppText`, cleared `ppNum`) drops its argument. If the enclosing call has no arguments left and its function is in ggpaintr's safe-no-op list (`labs`, `facet_wrap`, `theme`, …), the whole call drops too. `geom_*()` / `stat_*()` layers are never dropped — empty forms still inherit from `ggplot()`. Use `safe_to_remove = c("pcp_theme")` to opt third-party helpers into cleanup.
