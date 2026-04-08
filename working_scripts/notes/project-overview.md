# Project Overview

## Purpose

This is the durable architecture overview for the maintained `ggpaintr` package
surface.

## Repo shape

The repo has two clearly separated areas:

1. active package code for the maintained `ggpaintr` workflow
2. archived legacy package content under `archive/legacy-package/`

The repo also still contains tracked historical exploratory material under
`preconsideration/`. It is excluded from the package/build path and is not part
of the maintained package surface.  
References: `.gitignore:44-46`, `.Rbuildignore:5-7`

The active package is centered on:

- `R/paintr-app.R`
- `R/paintr-copy.R`
- `R/paintr-data.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-placeholders.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`

## `ggpaintr` mental model

In the maintained workflow, a plot specification is written as one formula
string containing a ggplot-like expression template. Optional `copy_rules`
customize the user-facing control text without changing runtime semantics.
Optional `placeholders` let downstream developers add new placeholder types per
app without editing parser, UI, runtime, copy-rule, or export internals.

Key runtime path:

1. `paintr_formula()` parses the formula and constructs a `paintr_obj`
2. `ggpaintr_effective_placeholders()` builds the effective placeholder
   registry from built-ins plus any per-app custom placeholders
3. `paintr_effective_copy_rules()` merges internal copy defaults with any
   runtime overrides using the effective placeholder registry
4. `ggpaintr_server_state()` initializes shared reactive state for the wrapper
   and integration helpers
5. `ggpaintr_bind_control_panel()` builds static and deferred controls from
   parsed metadata plus resolved copy
6. `ggpaintr_bind_draw()`, `ggpaintr_bind_export()`, `ggpaintr_bind_plot()`,
   `ggpaintr_bind_error()`, and `ggpaintr_bind_code()` wire the standard Shiny
   behavior for the default wrapper and for embedded integrations
7. `ggpaintr_server()` is a thin wrapper over that shared state plus the
   standard bind helpers
8. `ggpaintr_app()` wraps the default UI shell around that server behavior
9. `generate_shiny()` writes a standalone app script with explicit shell-copy
   objects, explicit `ui`, explicit `server`, and a `ggpaintr_server()` call
   that rebuilds custom placeholders only when needed

References:

- `R/paintr-parse.R:20-77`
- `R/paintr-placeholders.R:69-149`
- `R/paintr-copy.R:12-18`
- `R/paintr-ui.R:210-341`
- `R/paintr-runtime.R:180-402`
- `R/paintr-app.R:191-567`
- `R/paintr-export.R:95-252`

## Public API

The maintained exported package surface is:

- `ggpaintr_app()`
- `ggpaintr_server()`
- `ggpaintr_ids()`
- `ggpaintr_server_state()`
- `ggpaintr_bind_control_panel()`
- `ggpaintr_bind_draw()`
- `ggpaintr_bind_export()`
- `ggpaintr_bind_plot()`
- `ggpaintr_bind_error()`
- `ggpaintr_bind_code()`
- `ggpaintr_plot_value()`
- `ggpaintr_error_value()`
- `ggpaintr_code_value()`
- `ggpaintr_controls_ui()`
- `ggpaintr_outputs_ui()`
- `ggpaintr_placeholder()`
- `ggpaintr_effective_placeholders()`
- `ggpaintr_missing_expr()`
- `ggpaintr_normalize_column_names()`
- `paintr_formula()`
- `paintr_build_runtime()`
- `paintr_get_plot()`
- `generate_shiny()`

Current public customization boundary:

- `ggpaintr_app()`, `ggpaintr_server()`, `ggpaintr_server_state()`, and
  `generate_shiny()` accept optional named-list `copy_rules`
- `ggpaintr_app()`, `ggpaintr_server()`, `ggpaintr_server_state()`,
  `paintr_formula()`, and `generate_shiny()` accept optional custom
  `placeholders`
- `ggpaintr_ids()` lets embedded integrations customize only the six top-level
  Shiny ids for control panel, draw, export, plot, error, and code
- `ggpaintr_server_state()` plus the `ggpaintr_bind_*()` helpers are the
  supported way to embed the package runtime into an existing Shiny app
- `ggpaintr_plot_value()`, `ggpaintr_error_value()`, and
  `ggpaintr_code_value()` are the pure-value seam for custom `renderPlot()`,
  `renderUI()`, and `renderText()` code
- `ggpaintr_controls_ui()` and `ggpaintr_outputs_ui()` provide optional default
  UI fragments for those integrations
- `ggpaintr_placeholder()`, `ggpaintr_effective_placeholders()`, and
  `ggpaintr_missing_expr()` are the supported contributor-facing extension path
  for custom placeholder/widget types
- built-in and custom placeholders now share the same registry lifecycle for
  parse metadata, UI construction, runtime substitution, copy validation, and
  export serialization
- `paintr_effective_copy_rules()` and `paintr_resolve_copy()` remain exported
  copy helpers for runtime and exported-app customization
- internal placeholder ids and dynamic `var-*` outputs remain package-owned in
  the current integration layer
- everything else in `R/` remains package-internal implementation support

References:

- `R/paintr-app.R:21-567`
- `R/paintr-placeholders.R:44-131`
- `R/paintr-copy.R:12-146`
- `R/paintr-export.R:95-252`
- `NAMESPACE:3-26`
- `_pkgdown.yml:7-48`

## Documentation workflow

- edit `README.Rmd`, not `README.md`
- regenerate `README.md` by knitting or rendering `README.Rmd` after README
  source changes
- if a session cannot regenerate `README.md`, treat `README.Rmd` as the
  source-of-truth edit and leave an explicit note that the knitted `README.md`
  still needs manual regeneration

References:

- `README.Rmd:1-5`

## Exported app design

The exported Shiny app is intended to be a starting point for users to build
their own Shiny apps, not a hidden wrapper around package internals.

Design expectations:

- the generated app should stay understandable, editable, and extensible
- users should be able to add their own observers, outputs, and other Shiny
  behavior directly in the exported file
- the generated app should keep explicit `ui <- shiny::fluidPage(...)` and
  explicit `server <- function(...) { paintr_state <- ggpaintr_server(...) }`
  structure
- the exported file should expose a visible `copy_rules` hook:
  `copy_rules <- NULL` for the default case and compact
  `custom_copy_rules <- ...` plus
  `copy_rules <- paintr_effective_copy_rules(custom_copy_rules)` for
  non-default customized exports
- the exported file should expose a visible `placeholders` hook:
  `placeholders <- NULL` for the default case and compact
  `custom_placeholders <- ...` plus
  `placeholders <- ggpaintr_effective_placeholders(custom_placeholders)` for
  custom-placeholder exports
- `input_formula` should stay readable in the generated source, and if the
  original formula spans multiple lines, the exported `input_formula` should
  also span multiple lines
- exported custom placeholders must stay standalone, so only definitions built
  from `ggpaintr_placeholder()` with inline hook functions are currently
  exportable
- the current architecture intentionally keeps exported apps on the established
  `ggpaintr_server()` path rather than exporting a binder-based template

References:

- `R/paintr-export.R:95-201`
- `R/paintr-export.R:229-252`
- `R/paintr-placeholders.R:451-509`
- `tests/testthat/test-export-shiny.R:1-420`

## Placeholder and copy model

Built-in placeholders:

- `var`
- `text`
- `num`
- `expr`
- `upload`

Custom placeholders:

- are registered per app with `ggpaintr_effective_placeholders()`
- are constructed with `ggpaintr_placeholder()`
- receive `meta` records containing `id`, `keyword`, `layer_name`, `param`,
  and `index_path`
- can define `build_ui()`, `resolve_expr()`, and optional `resolve_input()`,
  `bind_ui()`, and `prepare_eval_env()` hooks
- can participate in `copy_rules` through placeholder-specific defaults and
  registered keywords

Boundary notes:

- `var` now expects one exact column name from the resolved dataset, while
  derived mappings such as `var + 1` or `log(var)` stay supported when the
  transform is written in the formula text around `var`
- `expr` input must still be valid R code
- `upload` currently supports `.csv` and `.rds`
- local non-syntactic column names should be normalized with
  `ggpaintr_normalize_column_names()`, and uploaded datasets now pass through
  the same normalization path automatically
- structurally invalid formulas still block launch
- formulas using `var` with no data source still block during UI preparation
- unresolved local data objects are deferred to draw-time inline errors
- copy customization is runtime-configurable through named-list rules merged
  with package defaults
- positional arguments use the internal `__unnamed__` key when resolving
  layer-specific copy rules, and aliases such as `colour` normalize to `color`
- default exported apps rely on package-owned default copy behavior through
  `copy_rules <- NULL`, while customized exports preserve only compact
  non-default overrides and reconstruct effective rules in the generated app
- exported custom placeholders are reconstructed from serialized definition
  calls rather than from the built-in placeholder registry
- advanced integrations can customize the built plot through
  `ggpaintr_plot_value()` while the default binder preserves the blank-on-failure
  render behavior

References:

- `R/paintr-parse.R:20-77`
- `R/paintr-placeholders.R:44-131`
- `R/paintr-placeholders.R:752-854`
- `R/paintr-data.R:1-121`
- `R/paintr-placeholders.R:312-449`
- `R/paintr-placeholders.R:451-509`
- `R/paintr-upload.R:35-95`
- `R/paintr-copy.R:117-174`
- `R/paintr-copy.R:252-643`
- `R/paintr-app.R:322-447`
- `R/paintr-export.R:95-201`
