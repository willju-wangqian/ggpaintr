---
name: project-overview
type: reference
scope: [architecture, api]
created: 2026-04-09
---

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
References: `.gitignore:44-46`, `.Rbuildignore:8`

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
string containing a ggplot-like expression template. Optional `ui_text`
customize the user-facing control text without changing runtime semantics.
Optional `placeholders` let downstream developers add new placeholder types per
app without editing parser, UI, runtime, copy-rule, or export internals.

Key runtime path:

1. `ptr_parse_formula()` parses the formula and constructs a `ptr_obj`
2. `ptr_merge_placeholders()` builds the effective placeholder
   registry from built-ins plus any per-app custom placeholders
3. `ptr_merge_ui_text()` merges internal copy defaults with any
   runtime overrides using the effective placeholder registry
4. `ptr_server_state()` initializes shared reactive state for the wrapper
   and integration helpers
5. `ptr_register_controls()` builds static and deferred controls from
   parsed metadata plus resolved copy
6. `ptr_register_draw()`, `ptr_register_export()`, `ptr_register_plot()`,
   `ptr_register_error()`, and `ptr_register_code()` wire the standard Shiny
   behavior for the default wrapper and for embedded integrations
7. `ptr_server()` is a thin wrapper over that shared state plus the
   standard bind helpers
8. `ptr_app()` wraps the default UI shell around that server behavior
9. `ptr_generate_shiny()` writes a standalone app script with explicit shell-copy
   objects, explicit `ui`, explicit `server`, and a `ptr_server()` call
   that rebuilds custom placeholders only when needed

References:

- `R/paintr-parse.R:20-77`
- `R/paintr-placeholders.R:69-149`
- `R/paintr-copy.R:12-18`
- `R/paintr-ui.R:210-319`
- `R/paintr-runtime.R:180-369`
- `R/paintr-app.R:191-567`
- `R/paintr-export.R:95-252`

## Public API

The maintained exported package surface is:

- `ptr_app()`
- `ptr_server()`
- `ptr_build_ids()`
- `ptr_server_state()`
- `ptr_register_controls()`
- `ptr_register_draw()`
- `ptr_register_export()`
- `ptr_register_plot()`
- `ptr_register_error()`
- `ptr_register_code()`
- `ptr_extract_plot()`
- `ptr_extract_error()`
- `ptr_extract_code()`
- `ptr_input_ui()`
- `ptr_output_ui()`
- `ptr_define_placeholder()`
- `ptr_merge_placeholders()`
- `ptr_missing_expr()`
- `ptr_normalize_column_names()`
- `ptr_runtime_input_spec()`
- `ptr_parse_formula()`
- `ptr_exec()`
- `ptr_assemble_plot()`
- `ptr_merge_ui_text()`
- `ptr_resolve_ui_text()`
- `ptr_generate_shiny()`

Current public customization boundary:

- `ptr_app()`, `ptr_server()`, `ptr_server_state()`, and
  `ptr_generate_shiny()` accept optional named-list `ui_text`
- `ptr_app()`, `ptr_server()`, `ptr_server_state()`,
  `ptr_parse_formula()`, and `ptr_generate_shiny()` accept optional custom
  `placeholders`
- `ptr_build_ids()` lets embedded integrations customize only the six top-level
  Shiny ids for control panel, draw, export, plot, error, and code
- `ptr_server_state()` plus the `ptr_register_*()` helpers are the
  supported way to embed the package runtime into an existing Shiny app
- `ptr_extract_plot()`, `ptr_extract_error()`, and
  `ptr_extract_code()` are the pure-value seam for custom `renderPlot()`,
  `renderUI()`, and `renderText()` code
- `ptr_runtime_input_spec()` is the supported low-level discovery helper
  for advanced tests, tooling, and package authors who need to build runtime
  inputs without relying on raw id encoding by hand
- `ptr_input_ui()` and `ptr_output_ui()` provide optional default
  UI fragments for those integrations
- `ptr_define_placeholder()`, `ptr_merge_placeholders()`, and
  `ptr_missing_expr()` are the supported contributor-facing extension path
  for custom placeholder/widget types
- built-in and custom placeholders now share the same registry lifecycle for
  parse metadata, UI construction, runtime substitution, copy validation, and
  export serialization
- `ptr_merge_ui_text()` and `ptr_resolve_ui_text()` remain exported
  copy helpers for runtime and exported-app customization
- internal placeholder ids and dynamic `var-*` outputs remain package-owned in
  the current integration layer
- everything else in `R/` remains package-internal implementation support

References:

- `R/paintr-app.R:21-567`
- `R/paintr-parse.R:80-184`
- `R/paintr-placeholders.R:44-131`
- `R/paintr-copy.R:12-146`
- `R/paintr-export.R:95-252`
- `NAMESPACE:3-28`
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
  explicit `server <- function(...) { ptr_state <- ptr_server(...) }`
  structure
- the exported file should expose a visible `ui_text` hook:
  `ui_text <- NULL` for the default case and compact
  `custom_ui_text <- ...` plus
  `ui_text <- ptr_merge_ui_text(custom_ui_text)` for
  non-default customized exports
- the exported file should expose a visible `placeholders` hook:
  `placeholders <- NULL` for the default case and compact
  `custom_placeholders <- ...` plus
  `placeholders <- ptr_merge_placeholders(custom_placeholders)` for
  custom-placeholder exports
- `input_formula` should stay readable in the generated source, and if the
  original formula spans multiple lines, the exported `input_formula` should
  also span multiple lines
- exported custom placeholders must stay standalone, so only definitions built
  from `ptr_define_placeholder()` with inline hook functions are currently
  exportable
- the current architecture intentionally keeps exported apps on the established
  `ptr_server()` path rather than exporting a binder-based template

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

- are registered per app with `ptr_merge_placeholders()`
- are constructed with `ptr_define_placeholder()`
- receive `meta` records containing `id`, `keyword`, `layer_name`, `param`,
  and `index_path`
- can define `build_ui()`, `resolve_expr()`, and optional `resolve_input()`,
  `bind_ui()`, and `prepare_eval_env()` hooks
- can participate in `ui_text` through placeholder-specific defaults and
  registered keywords

Boundary notes:

- `var` now expects one exact column name from the resolved dataset, while
  derived mappings such as `var + 1` or `log(var)` stay supported when the
  transform is written in the formula text around `var`
- `expr` input must still be valid R code
- `upload` currently supports `.csv` and `.rds`
- local non-syntactic column names should be normalized with
  `ptr_normalize_column_names()`, and uploaded datasets now pass through
  the same normalization path automatically
- structurally invalid formulas still block launch
- formulas using `var` with no data source still block during UI preparation
- unresolved local data objects are deferred to draw-time inline errors
- copy customization is runtime-configurable through named-list rules merged
  with package defaults
- positional arguments use the internal `__unnamed__` key when resolving
  layer-specific copy rules, and aliases such as `colour` normalize to `color`
- default exported apps rely on package-owned default copy behavior through
  `ui_text <- NULL`, while customized exports preserve only compact
  non-default overrides and reconstruct effective rules in the generated app
- exported custom placeholders are reconstructed from serialized definition
  calls rather than from the built-in placeholder registry
- advanced integrations can customize the built plot through
  `ptr_extract_plot()` while the default binder preserves the blank-on-failure
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
