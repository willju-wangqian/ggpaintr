# Project Overview

## Purpose

This is the durable architecture overview for the maintained `ggpaintr` package
surface.

## Repo shape

The repo has two clearly separated areas:

1. active package code for the maintained `ggpaintr` workflow
2. archived legacy package content under `archive/legacy-package/`

The active package is centered on:

- `R/paintr-app.R`
- `R/paintr-copy.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`

## `ggpaintr` mental model

In the maintained workflow, a plot specification is written as one formula
string containing a ggplot-like expression template. Optional `copy_rules`
customize the user-facing control text without changing the runtime semantics.

Key runtime path:

1. `paintr_formula()` parses the formula and constructs a `paintr_obj`
2. `paintr_effective_copy_rules()` merges internal copy defaults with any
   runtime overrides
3. `paintr_get_tab_ui()` and `register_var_ui_outputs()` build static and
   deferred controls from formula metadata plus resolved copy
4. `paintr_build_runtime()` completes the expression, builds the plot, validates
   render-time behavior, and formats errors
5. `ggpaintr_server()` wires the standard Shiny server behavior, runtime state,
   and export handler while preserving the original `copy_rules` input for
   export
6. `ggpaintr_app()` wraps the default UI shell around that server behavior
7. `generate_shiny()` writes a standalone app script with explicit shell-copy
   objects, explicit `ui`, explicit `server`, and a `ggpaintr_server()` call

References:

- `R/paintr-parse.R:17-67`
- `R/paintr-copy.R:507-625`
- `R/paintr-ui.R:210-426`
- `R/paintr-runtime.R:289-402`
- `R/paintr-app.R:40-186`
- `R/paintr-export.R:64-179`

## Public API

The maintained exported package surface is:

- `ggpaintr_app()`
- `ggpaintr_server()`
- `paintr_formula()`
- `paintr_build_runtime()`
- `paintr_get_plot()`
- `generate_shiny()`

Current public customization boundary:

- `ggpaintr_app()`, `ggpaintr_server()`, and `generate_shiny()` accept optional
  named-list `copy_rules`
- `paintr_effective_copy_rules()` is now exported so users and exported apps
  can rebuild a full runtime copy registry from compact custom overrides
- exported apps can also call exported `paintr_resolve_copy()` to keep shell
  labels aligned with a visible `copy_rules` object
- `copy_rules` only affects labels, helper text, placeholders, and related UI
  wording
- everything else in `R/` remains package-internal implementation support

References:

- `R/paintr-app.R:7-18`
- `R/paintr-app.R:29-46`
- `R/paintr-copy.R:507-625`
- `R/paintr-export.R:78-179`

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
- `input_formula` should stay readable in the generated source, and if the
  original formula spans multiple lines, the exported `input_formula` should
  also span multiple lines

References:

- `R/paintr-app.R:45-77`
- `R/paintr-copy.R:507-580`
- `R/paintr-export.R:34-155`
- `R/paintr-export.R:166-179`
- `tests/testthat/test-export-shiny.R:1-302`

## Placeholder and copy model

Supported placeholders:

- `var`
- `text`
- `num`
- `expr`
- `upload`

Boundary notes:

- `var` currently allows expression-like input, not only plain column names
- `expr` input must still be valid R code
- `upload` currently supports `.csv` and `.rds`
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

References:

- `R/paintr-parse.R:3-6`
- `R/paintr-runtime.R:9-17`
- `R/paintr-ui.R:285-320`
- `R/paintr-runtime.R:326-377`
- `R/paintr-upload.R:35-95`
- `R/paintr-copy.R:117-125`
- `R/paintr-copy.R:150-174`
- `R/paintr-copy.R:252-580`
- `R/paintr-export.R:64-155`
