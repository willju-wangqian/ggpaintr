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
