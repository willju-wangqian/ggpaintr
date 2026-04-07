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
   and export handler
6. `ggpaintr_app()` wraps the default UI shell around that server behavior
7. `generate_shiny()` writes a standalone app script that embeds the effective
   copy rules and calls `ggpaintr_server()`

References:

- `R/paintr-parse.R:17-67`
- `R/paintr-copy.R:507-585`
- `R/paintr-ui.R:210-426`
- `R/paintr-runtime.R:289-402`
- `R/paintr-app.R:40-159`
- `R/paintr-export.R:18-120`

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
- `copy_rules` only affects labels, helper text, placeholders, and related UI
  wording
- everything else in `R/` remains package-internal implementation support

References:

- `R/paintr-app.R:7-18`
- `R/paintr-app.R:29-46`
- `R/paintr-export.R:91-120`

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
- exported apps preserve the merged effective rules at export time

References:

- `R/paintr-parse.R:3-6`
- `R/paintr-runtime.R:9-17`
- `R/paintr-ui.R:285-320`
- `R/paintr-runtime.R:326-377`
- `R/paintr-upload.R:35-95`
- `R/paintr-copy.R:117-125`
- `R/paintr-copy.R:150-174`
- `R/paintr-copy.R:252-585`
- `R/paintr-export.R:18-82`
