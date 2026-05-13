# BUG-4 — browser follow-up: custom consumer gets empty cols after upload

Status: pending browser-harness verification. Filed 2026-05-13 during the local autonomous bug-fix loop.

## Symptom (from the original report)

App C (`ptr_app(ex3_formula)`) — after uploading `tests/testthat/fixtures/penguins.csv` into both `ggplot_1_upload_NA` and `geom_line_0_upload_NA`, the `select(colvars)` custom-consumer widget renders with `<select>` containing 0 options. Sibling built-in `var` consumers in the same pipeline (`mutate`, `filter`, `aes(x = var, y = var, color = var)`) show all uploaded columns.

The traced suspicion was that the framework passes `cols = character()` to the custom-consumer `build_ui` while the built-in `var` consumer at the same upstream depth gets the columns.

## What was tried locally

- Direct call to `ptr_resolve_upstream()` at the colvars node's upstream subtree: returns the full column set (parity with sibling var consumers).
- `testServer()` reproduction (`tests/testthat/test-regression-bugs-2026-05-12.R::BUG-4`) — wiring upload → companion id → `state$resolved_sources` and reading every consumer's `<id>_ui` output to force `renderUI` evaluation: the custom-consumer `build_ui` receives the same cols as the built-in `var` `build_ui` at the same depth. Test passes.
- Inspected: `runtime_consumer_entry`, `ptr_setup_consumer_uis`'s `entry_reactive` snapshot, `find_source_companion_ids_in_upstream`, `invoke_build_ui` and `build_ui_copy_args`. No data-path branch is keyed on the keyword — `var` and a custom consumer at the same depth go through identical code.

## Why this is shipped as-is

The runtime contract (custom-consumer parity with built-in `var` at the same upstream depth) is now pinned by the `testServer()` test, so any future regression that breaks the headless path is caught. The original report's screenshot-level symptom (empty `<select>`) was not reproducible without the real browser; it may have been transient render-timing in Chrome, or specific to the autoname-companion debounce vs. the renderUI invalidation order — both of which `testServer()` collapses.

## What to verify in the browser harness

1. Launch `dev/scripts/feature-coverage-examples.R` App C exactly as the original report did (`ptr_app(ex3_formula)`, port 4321).
2. Upload `tests/testthat/fixtures/penguins.csv` into both `#ggplot_1_upload_NA` and `#geom_line_0_upload_NA`.
3. After both auto-name companions settle to `"penguins"`, inspect `document.getElementById("ggplot_3_1_colvars_NA").options.length` and compare with `document.getElementById("ggplot_4_1_1_var_NA").options.length`.
4. If colvars is still 0 while var is 8, the bug is real and the next step is to instrument `ptr_setup_consumer_uis`' `entry_reactive` to log the snapshot fed into `runtime_consumer_entry` at the colvars node id — focusing on whether the upload-companion key is missing from the snapshot at the moment colvars renders.

## Acceptance

The browser-side check at step 3 above (colvars option count > 0 after upload completes) is the definitive acceptance test. The headless regression test catches the contract-level violation but cannot stand in for the browser path.
