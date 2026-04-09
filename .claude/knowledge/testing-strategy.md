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
- `ggpaintr_server()`
- `ggpaintr_placeholder()`
- `ggpaintr_effective_placeholders()`
- `ggpaintr_formula()`
- `ggpaintr_build_runtime()`
- `ggpaintr_get_plot()`
- `generate_shiny()`
- internal helpers that define runtime semantics or user-facing control copy

## Maintenance rules

When behavior changes in the maintained path:

- update focused `testthat` coverage in the same change
- update manual docs when human interaction behavior or exported-app behavior
  changes
- preserve the current `tests/testthat/` structure
- keep export-path coverage when `generate_shiny()` changes
- if user-facing prompt text or `copy_rules` behavior changes, update
  `tests/testthat/test-copy-rules.R` and the relevant manual copy-rule checks
- if placeholder-registry behavior changes, update
  `tests/testthat/test-placeholder-registry.R`, the relevant export tests, and
  the relevant manual placeholder-registry checks

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
- exported app generation, including default `copy_rules <- NULL`,
  compact `custom_copy_rules` reconstruction, multiline `input_formula`
  parity, default `placeholders <- NULL`, and compact
  `custom_placeholders` reconstruction
- reusable server-state behavior for exported/custom apps
- package-surface behavior for exported functions

Manual coverage should continue to include:

- real Shiny interaction
- upload flows
- inline error feedback
- default copy-rule behavior
- custom `copy_rules` overrides
- custom Shiny integration with bind helpers and custom top-level ids
- custom plot rendering with `ggpaintr_plot_value()`
- custom placeholder controls and exported custom-placeholder apps
- exported app smoke tests, including custom-copy parity and the explicit
  editable export-template shape

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
