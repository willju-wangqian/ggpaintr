# Switchable pipeline-stage wrapper

ADR-0021 *structural* keyword that marks a pipeline stage as user-
toggleable in
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).
The boolean argument is `switch_on` (positive sense: TRUE applies the
verb, FALSE skips it) and an optional `label` carries the UI text for
the resulting checkbox. Inside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
the parser sees the wrapper and unwraps it at translate time, stamping
the boot-state metadata + UI label onto the resulting node. The wrapper
itself never appears in the typed tree.

## Usage

``` r
ppVerbSwitch(.data, verb_expr, switch_on = TRUE, label = NULL)
```

## Arguments

- .data:

  A data frame or pipe-supplied dataset (the implicit `.data` slot when
  used as a pipeline stage).

- verb_expr:

  A data-pipeline verb call (e.g. `mutate(mpg = mpg + 100)`,
  `filter(cyl == 6)`). Evaluated with `.data` inserted as the first
  positional argument only when `switch_on = TRUE`.

- switch_on:

  A length-1 non-NA logical literal. In
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  formulas this MUST be a literal — the translator aborts on a
  non-literal so the formula remains the single source of truth for the
  app's boot state. Defaults to `TRUE` (apply the verb).

- label:

  Optional length-1 character used as the checkbox label inside
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).
  Ignored by the naked-R path. Defaults to `NULL`.

## Value

Outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md):
returns `.data` unchanged when `switch_on = FALSE`, otherwise the result
of `verb_expr` applied to `.data`.

## Details

Outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
it behaves per its R semantics so naked-dplyr scripts still render:
`ppVerbSwitch(.data, mutate(x = 1), FALSE)` returns `.data` unchanged;
`ppVerbSwitch(.data, mutate(x = 1), TRUE)` routes `.data` through the
verb call. `label` is metadata-only outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
(the naked-R path ignores it).

## Data-argument position

`ppVerbSwitch(.data, verb_expr, switch_on = TRUE)` inserts `.data` as
the **first positional argument** of `verb_expr` when `switch_on` is
TRUE. This matches the tidyverse convention and the translator's
pipeline-stage handling; non-tidyverse verbs that take data in a later
argument are not supported (use a lambda stage or a named wrapper
instead).

## Examples

``` r
if (requireNamespace("dplyr", quietly = TRUE)) {
  # Naked-R semantics: switch_on = FALSE leaves the data unchanged.
  identical(
    ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), FALSE),
    mtcars
  )

  # switch_on = TRUE routes .data through the verb.
  result <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), TRUE)
  nrow(result)  # 14
}
#> [1] 14

# Inside ptr_app(), the wrapper becomes a node-level default + a
# labelled boot-state-on checkbox:
if (interactive()) {
  ptr_app(
    "mtcars |> ppVerbSwitch(dplyr::filter(mpg > 20), TRUE, label = 'Filter') |>
     ggplot(aes(x = mpg, y = wt)) + geom_point()"
  )
}
```
