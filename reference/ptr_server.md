# Register ggpaintr Server Logic

Wire the standard `ggpaintr` server behavior into an existing Shiny app.
The returned state object exposes reactive access to the parsed formula,
latest runtime result, and current dynamic `var` UI definitions so
callers can extend the app with additional observers and outputs.

## Usage

``` r
ptr_server(
  input,
  output,
  session,
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  placeholders = NULL,
  ids = ptr_build_ids(),
  checkbox_defaults = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  shared = list(),
  draw_trigger = NULL,
  ns = shiny::NS(NULL)
)
```

## Arguments

- input:

  A Shiny `input` object.

- output:

  A Shiny `output` object.

- session:

  A Shiny `session` object.

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

- ids:

  A `ptr_build_ids` object controlling the Shiny element IDs used by the
  integration helpers. Defaults to
  [`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md).

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

- shared:

  Named list of Shiny reactives (or `NULL`/empty list). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details. Use this to drive multiple ptr_obj instances from a
  single externally-rendered control.

- draw_trigger:

  Optional Shiny reactive (or `NULL`). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details.

- ns:

  An optional namespace function (`character -> character`). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details.

## Value

A `ptr_state` object containing reactive accessors named `obj`,
`runtime`, and `var_ui_list`, plus shared metadata used by the bind
helpers.

## Note

For Shiny modules, prefer
[`ptr_module_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_module_ui.md)
and
[`ptr_module_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_module_server.md).
`moduleServer()` already scopes `input` and `output`, while UI generated
from `renderUI()` still needs `session$ns`; the module wrappers handle
that split.
