# Testing Strategy

## Purpose

This file defines the maintained testing strategy for the current `paintr2`
workflow.

- Update only when testing structure, policy, or acceptance expectations
  meaningfully change.
- Do not use this file as a task log.

## Strategy Summary

Use package-standard automated testing plus targeted manual checks.

Automated tests live in:

- `tests/testthat/`
- `tests/testthat/fixtures/`

Manual verification lives in:

- `tests/manual/manual-checklist-paintr2.md`
- `tests/manual/manual_test.Rmd`

Current target under test:

- the `paintr2` flow, especially `paintr_formula()`, `output_embed_var()`,
  `paintr_complete_expr()`, `paintr_build_runtime()`, `paintr_get_plot()`, and
  `generate_shiny()`

## Maintenance Rules

When behavior changes in the maintained `paintr2` path:

- add or update automated tests for new logic
- update manual docs only when manual verification expectations changed
- keep tests package-standard under `tests/testthat/`
- prefer small committed fixtures over generated-on-the-fly fixtures for core
  cases

## Required Test Layers

Automated coverage should continue to include:

- formula parsing and metadata construction
- placeholder detection and substitution
- upload helper behavior
- dynamic `var` UI behavior
- expression completion
- structured runtime error handling
- plot construction
- exported app generation
- supported and unsupported behavior boundaries

Manual coverage should continue to include:

- real Shiny interaction
- upload flows
- inline error feedback
- export smoke tests

## Acceptance Expectations

For feature work in `paintr2`, the default expectation is:

1. update or add focused `testthat` coverage
2. keep the existing suite passing
3. update manual docs if the human interaction flow changed
4. preserve exported-app behavior when export paths are touched
