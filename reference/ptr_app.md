# Build a Shiny App from a `ggpaintr` Formula

Translates `formula` into the typed AST, builds the per-layer panel UI,
and wires the server end-to-end. Returns a `shiny.appobj` ready to be
run. This page is the canonical reference for the formula grammar and
the empty-call cleanup rule used by every public entry point.

## Usage

``` r
ptr_app(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  css = NULL,
  spec = NULL
)
```

## Arguments

- formula:

  Either a single character scalar containing a ggplot expression with
  `ggpaintr` placeholders, or an unquoted ggplot expression supplied
  directly. Expression-mode is captured with
  [`rlang::enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  at the public boundary, then deparsed to a string before reaching the
  shared translate pipeline; both modes produce equivalent apps. A bare
  symbol bound to a string in the calling frame (e.g.
  `f <- "..."; ptr_app(f)`) is resolved and treated as string mode.
  Pre-quoted wrappers
  ([`rlang::expr()`](https://rlang.r-lib.org/reference/expr.html),
  [`rlang::quo()`](https://rlang.r-lib.org/reference/defusing-advanced.html),
  [`base::quote()`](https://rdrr.io/r/base/substitute.html),
  [`base::bquote()`](https://rdrr.io/r/base/bquote.html)) at the
  captured root are unwrapped one level. `!!` splicing inside the
  captured expression is honoured via
  [`rlang::enexpr()`](https://rlang.r-lib.org/reference/defusing-advanced.html).
  **Native pipe (`|>`) caveat:** in expression mode, R's parser desugars
  `|>` before capture, so the rendered code shows the desugared
  nested-call form. Stay in string mode (or use `%>%`) if you need `|>`
  preserved.

- envir:

  Environment used to resolve local data objects.

- ui_text:

  Optional named list of copy overrides; see
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  for the full schema and current defaults.

- expr_check:

  Controls formula-level `ppExpr` validation. Three modes: `TRUE`
  (default) applies the built-in denylist + AST walker; `FALSE` disables
  formula-level validation (for local prototyping with trusted formulas
  only); a `list` with `deny_list` and/or `allow_list` entries
  (character vectors) customises the formula-level policy without
  disabling it. Code typed into a `ppExpr` box at runtime is always
  screened against the built-in denylist, regardless of this setting.
  See
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md)
  for the walker model.

- safe_to_remove:

  Character vector of additional function names whose zero-argument
  calls should be dropped after placeholder substitution leaves them
  empty. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html). See
  **Empty-call cleanup** below. A user-typed `ppExpr` always wins —
  whatever the user enters into an `ppExpr` box is honoured verbatim,
  even if its top-level name is in `safe_to_remove`.

- css:

  Optional character vector of paths to additional CSS files. Each is
  served as a static resource and linked after `ggpaintr`'s bundled
  stylesheet, so its rules override the default `.ptr-*` styling.
  Relative `url(...)` references inside a file resolve against that
  file's own directory. Defaults to `NULL` (no extra stylesheet).

- spec:

  An optional named list of fully-qualified Shiny input id -\> value,
  used to override widget defaults at session boot. See [ADR
  0012](https://willju-wangqian.github.io/ggpaintr/reference/dev/adr/0012-role-based-tree-and-ptr-spec.md).

## Value

A `shiny.appobj`.

## Formula grammar

A `ggpaintr` formula is a single
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
written as a string. Drop one of five placeholder keywords anywhere a
value would normally go, and the runtime substitutes the user's input
back into the expression at render time.

- `ppVar`:

  Column picker, data-aware. Renders as a `selectInput` populated with
  the upstream data's column-name vector. Example: `aes(x = ppVar)`.

- `ppText`:

  Free-text input. Renders as a `textInput`. Example:
  `labs(title = text)`.

- `ppNum`:

  Numeric input. Renders as a `numericInput`. Example:
  `geom_point(size = ppNum)`.

- `ppExpr`:

  Code editor, validated by `expr_check`. The only keyword that accepts
  arbitrary R code; see
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md)
  for the model. Example: `facet_wrap(ppExpr)`.

- `ppUpload`:

  File picker, returns a data frame. Renders as a `fileInput` plus an
  optional dataset-name textbox. Accepted formats: `.csv`, `.tsv`,
  `.rds`, `.xlsx`, `.xls`, `.json`. Uploaded data is normalized via
  [`ptr_normalize_column_names()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_normalize_column_names.md)
  automatically. Example: `ggplot(ppUpload, ...)`.

Any keyword occurrence may carry `shared = "<id>"` to lift the widget
out of its per-layer panel into a top-level shared section. Used by
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
to drive multiple plots from one control. See
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
for worked examples of each keyword.

## Empty-call cleanup

When a placeholder resolves to "missing" (an empty `ppVar` pick, a blank
`ppText`, a cleared `ppNum`, an unchecked layer checkbox), its argument
is dropped from the generated code. If the surrounding call is left
empty and its bare name is in the curated cleanup list, the whole call
disappears too. This rule applies to both placeholder-driven empties and
user-authored literal empty calls like `+ labs()`.

Curated `ggplot2` names that are dropped when empty:

    theme, labs, xlab, ylab, ggtitle,
    facet_wrap, facet_grid, facet_null,
    xlim, ylim, lims, expand_limits,
    guides, annotate, annotation_custom,
    annotation_map, annotation_raster,
    aes, aes_, aes_q, aes_string,
    element_text, element_line, element_rect,
    element_point, element_polygon, element_geom

Empty calls to similar no-op helpers from `dplyr`, `tidyr`, `tibble`,
`pillar`, `purrr`, `stringr`, `forcats`, `lubridate`, and `hms` are
covered by the same rule.

`geom_*()` and `stat_*()` layers are **never** dropped, regardless of
whether they end up empty — they inherit their aesthetics from
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) and
remain meaningful with no arguments.

[`element_blank()`](https://ggplot2.tidyverse.org/reference/element.html)
is intentionally **not** in the cleanup list: its empty form is a
meaningful "suppress" directive, not a no-op.

Third-party helpers (e.g. `pcp_theme()` from `ggpcp`) are not in the
cleanup list — being absent is the "removal safety unknown" signal. Use
`safe_to_remove = c("pcp_theme")` to opt a specific name in.

## See also

[`ptr_app_bslib()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md)
for the same contract with a `bslib` theme;
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
for multi-plot apps with shared widgets;
[`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
et al. for registering custom keywords;
[`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
for copy overrides;
[`ptr_css()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_css.md)
for the `css =` argument and themable CSS custom properties;
[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
for tutorial examples.

## Examples

``` r
if (interactive()) {
  # Expression mode (primary): pass the unquoted ggplot expression.
  ptr_app(ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point())
  # `!!` splices a value into the expression.
  col <- rlang::sym("mpg")
  ptr_app(ggplot(mtcars, aes(x = !!col, y = ppVar)) + geom_point())
  # String mode (fallback): the same formula as text.
  ptr_app("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()")
}
```
