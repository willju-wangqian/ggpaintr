# Build Reactive Server State for ggpaintr

Create the shared reactive state used by the extensible `ptr_*` server
helpers. This object can be passed to the bind helpers or inspected
directly inside a larger Shiny app.

## Usage

``` r
ptr_server_state(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  ids = ptr_build_ids(),
  placeholders = NULL,
  checkbox_defaults = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  ns = shiny::NS(NULL),
  server_ns = ns
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

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

- checkbox_defaults:

  Optional named list controlling the initial checked state of each
  layer's "include this layer" checkbox at app launch. Names match layer
  names from the formula (use `names(parsed$expr_list)` to inspect;
  duplicate layers receive a hyphen-numeric suffix starting at `-2`,
  e.g. a formula with two
  [`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
  calls produces layer names `geom_point` and `geom_point-2`). Each
  value is a single logical or a logical vector applied positionally
  over consecutive instances of that layer; vectors shorter than the
  count of instances are padded with `TRUE` and longer vectors are
  truncated with a warning. A deduped key wrapped in backticks (e.g.
  `` `geom_point-2` ``) addresses one specific instance. `NA` and
  non-logical values raise an error; unrecognized names raise a warning
  and are ignored. Default `NULL` keeps every layer checked (current
  behavior).

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

  An optional namespace function (`character -> character`) used to
  render UI ids and, by default, Shiny server binding ids. Pass
  `shiny::NS("page1")` to avoid id collisions when embedding two or more
  ggpaintr formulas in the same root Shiny session. Defaults to
  `shiny::NS(NULL)` (identity — no prefixing).

- server_ns:

  Optional namespace function for Shiny `input` / `output` binding keys.
  Defaults to `ns`. The module wrapper uses `shiny::NS(NULL)` because
  [`shiny::moduleServer()`](https://rdrr.io/pkg/shiny/man/moduleServer.html)
  already scopes server bindings while rendered UI ids still need the
  module namespace.

## Value

An object of class `ptr_state`. The returned state stores `raw_ids`
(canonical unprefixed ids), `ui_ids` (rendered DOM ids), and
`server_ids` (Shiny binding keys). `ids` is retained as a compatibility
alias for `server_ids`.

## Examples

``` r
state <- ptr_server_state(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
is.function(state$runtime)
#> [1] TRUE
```
