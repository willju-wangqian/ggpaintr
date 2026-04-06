# Testing Strategy

## Purpose

This file defines the maintained testing strategy for the `ggpaintr` package
surface.

## Strategy summary

Use package-standard automated testing plus targeted manual checks.

Automated tests live in:

- `tests/testthat/`
- `tests/testthat/fixtures/`

Manual verification lives in:

- `tests/manual/manual-checklist-ggpaintr.md`
- `tests/manual/manual-test-ggpaintr.Rmd`

Current target under test:

- `ggpaintr_app()`
- `paintr_formula()`
- `paintr_build_runtime()`
- `paintr_get_plot()`
- `generate_shiny()`
- internal helpers that define runtime semantics

## Maintenance rules

When behavior changes in the maintained path:

- update focused `testthat` coverage in the same change
- update manual docs only when human interaction behavior changed
- preserve the current `tests/testthat/` structure
- keep export-path coverage when `generate_shiny()` changes

## Required test layers

Automated coverage should continue to include:

- formula parsing and metadata construction
- placeholder detection and substitution
- upload helper behavior
- dynamic `var` UI behavior
- expression completion
- structured runtime error handling
- plot construction
- exported app generation
- package-surface behavior for exported functions

Manual coverage should continue to include:

- real Shiny interaction
- upload flows
- inline error feedback
- exported app smoke tests

## Acceptance expectations

For package work on the maintained path, the default expectation is:

1. update or add focused `testthat` coverage
2. keep the suite passing
3. rebuild generated docs when the public package surface changes
4. run package-level verification for docs, site, and check readiness
