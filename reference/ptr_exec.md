# Build the Full Runtime Result for a Paintr App

Build the Full Runtime Result for a Paintr App

## Usage

``` r
ptr_exec(
  ptr_obj,
  input,
  envir = parent.frame(),
  expr_check = TRUE,
  safe_to_remove = character()
)
```

## Arguments

- ptr_obj:

  A `ptr_obj`.

- input:

  A Shiny input-like object.

- envir:

  The environment used to resolve local data objects.

- expr_check:

  Controls `expr` placeholder validation at runtime. `TRUE` (default)
  applies the built-in denylist of dangerous functions. `FALSE` disables
  all checking. A named list with `deny_list` and/or `allow_list`
  character vectors supplies a custom check; when both are given, denied
  entries are removed from the allowlist. Note: the formula template
  itself is separately validated at parse time via `formula_check` in
  [`ptr_parse_formula`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_parse_formula.md).
  Disabling `expr_check` here does not affect that earlier check, and
  vice versa.

- safe_to_remove:

  Character vector of additional function names whose zero-argument
  calls should be dropped after placeholder substitution leaves them
  empty. Extends the curated default set:
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html),
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html),
  [`xlab()`](https://ggplot2.tidyverse.org/reference/labs.html),
  [`ylab()`](https://ggplot2.tidyverse.org/reference/labs.html),
  [`ggtitle()`](https://ggplot2.tidyverse.org/reference/labs.html),
  [`facet_wrap()`](https://ggplot2.tidyverse.org/reference/facet_wrap.html),
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html),
  [`facet_null()`](https://ggplot2.tidyverse.org/reference/facet_null.html),
  [`xlim()`](https://ggplot2.tidyverse.org/reference/lims.html),
  [`ylim()`](https://ggplot2.tidyverse.org/reference/lims.html),
  [`lims()`](https://ggplot2.tidyverse.org/reference/lims.html),
  [`expand_limits()`](https://ggplot2.tidyverse.org/reference/expand_limits.html),
  [`guides()`](https://ggplot2.tidyverse.org/reference/guides.html),
  [`annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html).
  User-authored zero-arg calls (where substitution did not empty the
  call) and `geom_*()` / `stat_*()` standalone layers are always
  preserved. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html).

## Value

A runtime result list containing `ok`, `stage`, `message`, `code_text`,
`complete_expr_list`, `eval_env`, `condition`, and `plot`.
Completion-stage validation failures return `stage = "complete"`;
plot-construction or render failures return `stage = "plot"`.

## Examples

``` r
obj <- ptr_parse_formula(
  "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
)
spec <- ptr_runtime_input_spec(obj)
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "Sepal.Length"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "Sepal.Width"
runtime <- ptr_exec(
  obj,
  inputs
)
isTRUE(runtime$ok)
#> [1] TRUE
```
