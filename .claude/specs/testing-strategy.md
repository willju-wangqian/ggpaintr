---
name: testing-strategy
type: reference
scope: [testing]
created: 2026-04-09
---

# Testing Strategy

## Purpose

This file defines the maintained testing strategy for the `ggpaintr` package
surface.

## Strategy summary

Use package-standard automated testing plus targeted manual checks.

Automated tests live in:

- `tests/testthat/`
- `tests/testthat/fixtures/`

Manual verification:

- `tests/manual/manual-checklist-ggpaintr.md`
- `tests/manual/manual-test-ggpaintr.Rmd`

Current target under test:

- `ptr_app()`
- `ptr_server()`
- `ptr_define_placeholder()`
- `ptr_merge_placeholders()`
- `ptr_parse_formula()`
- `ptr_exec()`
- `ptr_assemble_plot()`
- internal helpers that define runtime semantics or user-facing control copy

## Maintenance rules

When behavior changes in the maintained path:

- update focused `testthat` coverage in the same change
- update manual docs when human interaction behavior changes
- preserve the current `tests/testthat/` structure
- if user-facing prompt text or `ui_text` behavior changes, update
  `tests/testthat/test-copy-rules.R` and the relevant manual copy-rule checks
- if placeholder-registry behavior changes, update
  `tests/testthat/test-placeholder-registry.R` and the relevant manual
  placeholder-registry checks

## Required test layers

Automated coverage should continue to include:

- formula parsing and metadata construction
- placeholder detection and substitution
- placeholder-registry construction, validation, and metadata propagation
- upload helper behavior
- dynamic `var` UI behavior
- expression completion
- structured runtime error handling
- plot construction
- copy-rule validation, normalization, merge precedence, and readable fallbacks
- reusable server-state behavior for custom apps built on the public API
- package-surface behavior for exported functions

Manual coverage should continue to include:

- real Shiny interaction
- upload flows
- inline error feedback
- default copy-rule behavior
- custom `ui_text` overrides
- custom Shiny integration with bind helpers and custom top-level ids
- custom plot rendering with `ptr_extract_plot()`
- custom placeholder controls

## Acceptance expectations

For package work on the maintained path, the default expectation is:

1. update or add focused `testthat` coverage
2. keep the suite passing
3. rebuild generated docs when the public package surface changes
4. run package-level verification for docs, site, and check readiness

Current default verification commands:

- `Rscript -e 'devtools::document()'`
- `Rscript -e 'testthat::test_dir("tests/testthat")'`
- `Rscript -e 'pkgdown::build_site_github_pages(new_process = FALSE, install = TRUE)'`
- `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`
- `Rscript -e 'devtools::load_all("."); rmarkdown::render("README.Rmd", envir = globalenv())'`
