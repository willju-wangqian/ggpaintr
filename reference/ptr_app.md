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
  checkbox_defaults = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  css = NULL
)
```

## Arguments

- formula:

  A single formula string with `ggpaintr` placeholders. See **Formula
  grammar** below.

- envir:

  Environment used to resolve local data objects.

- ui_text:

  Optional named list of copy overrides; see
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)
  for the full schema and current defaults.

- checkbox_defaults:

  Optional named list of initial checked states for layer checkboxes.

- expr_check:

  Controls `expr` placeholder validation. Three modes: `TRUE` (default)
  applies the built-in denylist + AST walker; `FALSE` disables all
  validation (for local prototyping with trusted input only); a `list`
  with `deny_list` and/or `allow_list` entries (character vectors)
  customises the policy without disabling it. See
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md)
  for the walker model.

- safe_to_remove:

  Character vector of additional function names whose zero-argument
  calls should be dropped after placeholder substitution leaves them
  empty. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html). See
  **Empty-call cleanup** below. A user-typed `expr` always wins —
  whatever the user enters into an `expr` box is honoured verbatim, even
  if its top-level name is in `safe_to_remove`.

- css:

  Optional character vector of paths to additional CSS files. Each is
  served as a static resource and linked after `ggpaintr`'s bundled
  stylesheet, so its rules override the default `.ptr-*` styling.
  Relative `url(...)` references inside a file resolve against that
  file's own directory. Defaults to `NULL` (no extra stylesheet).

## Value

A `shiny.appobj`.

## Formula grammar

A `ggpaintr` formula is a single
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
written as a string. Drop one of five placeholder keywords anywhere a
value would normally go, and the runtime substitutes the user's input
back into the expression at render time.

- `var`:

  Column picker, data-aware. Renders as a `selectInput` populated with
  the upstream data's column-name vector. Example: `aes(x = var)`.

- `text`:

  Free-text input. Renders as a `textInput`. Example:
  `labs(title = text)`.

- `num`:

  Numeric input. Renders as a `numericInput`. Example:
  `geom_point(size = num)`.

- `expr`:

  Code editor, validated by `expr_check`. The only keyword that accepts
  arbitrary R code; see
  [`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md)
  for the model. Example: `facet_wrap(expr)`.

- `upload`:

  File picker, returns a data frame. Renders as a `fileInput` plus an
  optional dataset-name textbox. Accepted formats: `.csv`, `.tsv`,
  `.rds`, `.xlsx`, `.xls`, `.json`. Uploaded data is normalized via
  [`ptr_normalize_column_names()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_normalize_column_names.md)
  automatically. Example: `ggplot(upload, ...)`.

Any keyword occurrence may carry `shared = "<id>"` to lift the widget
out of its per-layer panel into a top-level shared section. Used by
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
to drive multiple plots from one control. See
[`vignette("ggpaintr-use-cases")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-use-cases.md)
for worked examples of each keyword.

## Empty-call cleanup

When a placeholder resolves to "missing" (an empty `var` pick, a blank
`text`, a cleared `num`, an unchecked layer checkbox), its argument is
dropped from the generated code. If the surrounding call is left empty
and its bare name is in the curated cleanup list, the whole call
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
[`vignette("ggpaintr-use-cases")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-use-cases.md)
for tutorial examples.

## Examples

``` r
if (interactive()) {
  ptr_app("ggplot(mtcars, aes(x = var, y = var)) + geom_point()")
}
```
