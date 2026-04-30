# Build a ggpaintr Shiny App

Create a Shiny app from a single ggplot-like formula string. The app
exposes generated controls, a draw button, inline error handling, and
code output.

## Usage

``` r
ptr_app(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  placeholders = NULL,
  checkbox_defaults = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  ns = shiny::NS(NULL)
)
```

## Arguments

- formula:

  A single formula string using `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects when building the app.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

- checkbox_defaults:

  Optional named list of initial checked states for layer checkboxes.
  See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md).

- expr_check:

  Controls `expr` placeholder validation. `TRUE` (default) applies the
  built-in denylist of dangerous functions. `FALSE` disables all
  checking. A named list with `deny_list` and/or `allow_list` character
  vectors supplies a custom check; when both are given, denied entries
  are removed from the allowlist.

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
  [`annotate()`](https://ggplot2.tidyverse.org/reference/annotate.html),
  [`annotation_custom()`](https://ggplot2.tidyverse.org/reference/annotation_custom.html),
  [`annotation_map()`](https://ggplot2.tidyverse.org/reference/annotation_map.html),
  [`annotation_raster()`](https://ggplot2.tidyverse.org/reference/annotation_raster.html),
  [`aes()`](https://ggplot2.tidyverse.org/reference/aes.html),
  [`aes_()`](https://ggplot2.tidyverse.org/reference/aes_.html),
  [`aes_q()`](https://ggplot2.tidyverse.org/reference/aes_.html),
  [`aes_string()`](https://ggplot2.tidyverse.org/reference/aes_.html),
  [`vars()`](https://ggplot2.tidyverse.org/reference/vars.html),
  [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_line()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_rect()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_point()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_polygon()`](https://ggplot2.tidyverse.org/reference/element.html),
  [`element_geom()`](https://ggplot2.tidyverse.org/reference/element.html).
  `geom_*()` / `stat_*()` standalone layers are always preserved.
  Defaults to [`character()`](https://rdrr.io/r/base/character.html).

- ns:

  An optional namespace function (`character -> character`). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details. For standalone apps created with `ptr_app()`, namespacing
  is rarely needed; it is most useful when embedding more than one
  ggpaintr instance in a larger root Shiny session.

## Value

A `shiny.appobj`.

## Examples

``` r
app <- ptr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
inherits(app, "shiny.appobj")
#> [1] TRUE
```
