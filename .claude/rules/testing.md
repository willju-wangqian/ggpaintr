---
name: testing
description: ggpaintr-specific testing standards — supplements harness testing rule
scope: [testing]
severity: default
inject_into: [tester]
---

## ggpaintr Test Conventions

- testthat edition 3. Tests in `tests/testthat/test-*.R`.
- Helpers in `tests/testthat/helper-*.R`. Fixtures in `tests/testthat/fixtures/`.
- Use `test_that("description", { ... })` blocks.
- Use `expect_equal()`, `expect_error()`, `expect_true()`, `expect_s3_class()`.
- Use `withr::local_*()` for temporary state, not `on.exit()` directly.
- Group tests by feature/behavior, not by source file.
- Hunt for edge cases: NULL inputs, empty strings, malformed formulas, missing columns.
- Check that placeholder parsing handles unusual ggplot constructs.
- Run full suite: `Rscript -e "devtools::test()"`.
- Run R CMD check when asked: `Rscript -e "devtools::check()"`.

## Coverage expectations

Automated coverage should include: formula parsing, placeholder detection &
substitution, placeholder-registry construction/validation, upload helpers,
dynamic `var` UI, expression completion, runtime error handling, plot
construction, copy-rule validation/merge/normalization, server-state behavior
for custom apps, and exported package-surface behavior.

Manual coverage (`tests/manual/`): real Shiny interaction, upload flows,
inline errors, default and custom `ui_text`, custom integrations with bind
helpers + custom top-level ids, custom plot rendering via `ptr_extract_plot()`,
and custom placeholder controls.

When behavior changes on the maintained path:

- update focused `testthat` coverage in the same change
- update manual docs when human-interaction behavior changes
- if `ui_text` behavior changes, update `test-copy-rules.R` + manual copy checks
- if placeholder-registry behavior changes, update
  `test-placeholder-registry.R` + manual placeholder checks
