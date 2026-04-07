# Project Overview

## Purpose

This is the durable architecture overview for the maintained `ggpaintr` package
surface.

## Repo shape

The repo now has two clearly separated areas:

1. active package code for the maintained `ggpaintr` workflow
2. archived legacy package content under `archive/legacy-package/`

The active package is centered on:

- `R/paintr-app.R`
- `R/paintr-export.R`
- `R/paintr-parse.R`
- `R/paintr-runtime.R`
- `R/paintr-ui.R`
- `R/paintr-upload.R`
- `R/paintr-utils.R`

## `ggpaintr` mental model

In the maintained workflow, a plot specification is written as one formula
string containing a ggplot-like expression template. Placeholder tokens are
detected, turned into Shiny controls, substituted back into the expression, and
used to produce:

- a plot
- generated code
- inline runtime feedback
- an exportable standalone app

Key runtime path:

1. `paintr_formula()` parses the formula and constructs a `paintr_obj`
2. `register_var_ui_outputs()` resolves dynamic `var` controls from available data
3. `paintr_build_runtime()` completes the expression, builds the plot, validates
   render-time behavior, and formats errors
4. `ggpaintr_server()` wires the standard Shiny server behavior and exposes
   reactive state for extension
5. `ggpaintr_app()` wraps the default UI around that server behavior
6. `generate_shiny()` writes a standalone app script with explicit `ui`,
   explicit `server`, and a `ggpaintr_server()` call

## Public API

The maintained exported package surface is:

- `ggpaintr_app()`
- `ggpaintr_server()`
- `paintr_formula()`
- `paintr_build_runtime()`
- `paintr_get_plot()`
- `generate_shiny()`

Everything else in `R/` is package-internal implementation support.

## Placeholder model

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
- render-time ggplot failures are part of the shared runtime error path
