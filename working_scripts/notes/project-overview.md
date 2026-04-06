# Project Overview

## Purpose

This is the durable architecture overview for the current maintained development
path in `ggpaintr`.

- Update only when architecture or supported behavior boundaries meaningfully
  change.
- Do not use this file as a session log.

## Repo Shape

`ggpaintr` currently contains two tracks:

1. the older package-level `ggpaintr` path
2. the newer `paintr2` formula-driven workflow

The current development focus is the `paintr2` path centered on:

- `R/paintr2_func.R`
- `R/ui_function.R`
- `working_scripts/paintr_distribute.R`

## `paintr2` Mental Model

In the `paintr2` workflow, a plot specification is written as a single formula
string containing a valid `ggplot2` expression template. Placeholder tokens in
that string are detected, converted into Shiny controls, then substituted back
into the expression to produce:

- a plot
- generated code
- runtime feedback when completion or plot building fails

Key runtime path:

1. `paintr_formula()` parses the formula and constructs a `paintr_obj`
2. `output_embed_var()` resolves dynamic `var` controls from available data
3. `paintr_complete_expr()` substitutes current input values
4. `paintr_build_runtime()` captures completion success/failure, plot
   construction success/failure, render-time ggplot success/failure, generated
   code, and readable errors
5. `generate_shiny()` exports a standalone app using the same runtime helpers

References:

- `R/paintr2_func.R`
- `R/ui_function.R`
- `working_scripts/paintr_distribute.R`

## Placeholder Model

Supported placeholders in the maintained `paintr2` path:

- `var`
- `text`
- `num`
- `expr`
- `upload`

Current meanings:

- `var`: variable-like expression chosen by the user
- `text`: character input
- `num`: numeric input
- `expr`: arbitrary R expression entered as text and parsed
- `upload`: uploaded dataset placeholder with file input and dataset-name input

Current boundary notes:

- `var` currently allows expression-like input, not only plain column names
- `expr` inputs must still be valid R code
- `upload` currently supports `.csv` and `.rds`
- structurally invalid formulas still block launch
- formulas using `var` with no data source still block launch
- unresolved local data objects such as `data = unknown_object` are now deferred
  to draw-time inline errors rather than blocking app launch
- render-time ggplot failures are part of the shared runtime error path

## Main Runtime Entry Points

Read these first when tracing behavior:

- `paintr_formula()` in `R/paintr2_func.R`
- `paintr_complete_expr()` in `R/paintr2_func.R`
- `paintr_build_runtime()` in `R/paintr2_func.R`
- `output_embed_var()` in `R/ui_function.R`
- `ggpaintr_basic2()` in `working_scripts/paintr_distribute.R`
- `generate_shiny()` in `R/paintr2_func.R`
