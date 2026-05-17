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

## Browser e2e (shinytest2) — hard-won gotchas

These cost a full debugging cycle once. Read before adding any shinytest2 test.

- **Boot an app *directory* that `pkgload::load_all()`s — never a parent-built
  `shiny.appobj`.** shinytest2 runs the app in a fresh child R process. A
  serialized appobj makes that child resolve `library(ggpaintr)` to the
  **stale system-installed** ggpaintr (pkgload `_build/` is empty; an old
  pre-redesign 0.9.1 is installed at the system library), so the suite
  silently tests dead code (e.g. an old `ptr_shared_server` →
  coordinator-boot crash) while *looking* green. Pattern in use:
  `tests/testthat/fixtures/vignette-apps/<slug>/app.R`, first line
  `pkgload::load_all(Sys.getenv("GGP_PKG"), quiet=TRUE, helpers=FALSE,
  attach_testthat=FALSE)`; test sets
  `withr::local_envvar(GGP_PKG = normalizePath(test_path("..","..")))` and
  `AppDriver$new(test_path("fixtures","vignette-apps",slug))`. The `app.R`
  body stays verbatim-equivalent to the source it represents (diffable).
- **Do not call `app$get_values()`.** It snapshots *every* output and 500s
  ("invalid char in json text") on custom-renderer apps whose host output is
  in a pre-draw `shiny.silent.error` state. Assert with targeted
  `app$get_html("#id")` / `app$get_value(output="id")` instead.
- **ggpaintr only re-renders on the Update/Draw click.** Setting a
  placeholder widget never updates an output by itself, so
  `app$set_inputs(id = value)` with the default `wait_ = TRUE` *times out*.
  Use `wait_ = FALSE` for every placeholder set, then click the draw button.
- **`var` pickers for source/consumer placeholders are suspended.** They live
  in a `renderUI` under the layer's "Data" subtab and aren't bound until that
  subtab is shown. Set the source/consumer input, then
  `set_inputs(<layer>_subtab = "Controls")`, `wait_for_idle()`, *then* set the
  var pickers — otherwise "Unable to find input binding".
- **Gating:** every test starts `skip_on_cran(); skip_if_not_installed(
  "shinytest2")` (+ `chromote` + per-example extension pkg). `devtools::test()`
  sets `NOT_CRAN=true` so AppDriver runs; R CMD check (CRAN) → all-skip. A
  clean all-skip (SKIP n / FAIL 0) without the browser stack is correct, not
  a failure.
- Wrap `AppDriver$new()` in scoped `suppressWarnings()` (the "Failed to locate
  globals" / htmlDependency-prefix warnings are benign — see project memory);
  never blanket-suppress assertion output.
