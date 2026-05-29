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

## Test scope during iteration — inner loop vs full e2e gate

The full e2e browser suite (shinytest2/chromote) is ~100% of the suite wall-clock and is the **merge gate**, not an inner-loop check. Running all ~150 browser boots on every small fix is waste, and under parallel it invites the boot-tail contention flakes the helpers were hardened against (4b5f4d0). Unless the user explicitly asks, do **not** run the full e2e suite while iterating. Instead:

1. **All non-e2e tests — always.** Cheap (seconds, ~2680 assertions), catch most regressions. No `NOT_CRAN` → browser tests cleanly SKIP via `skip_on_cran()`:

   ```
   Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_dir("tests/testthat", reporter="progress", stop_on_failure=FALSE)'
   ```
   Expected: FAIL 0 / ERROR 0 / PASS N / SKIP ≈112 (the SKIPs are the browser tests — correct here, not the gate).

2. **Only the e2e tests in the change's BLAST RADIUS** — never the whole e2e set. Blast radius = the e2e files covering the touched code path **plus** any e2e on the *same invariant / shared mechanism*, because bugs here routinely span the cross-cutting runtime (seeding ↔ source-binding ↔ boot-default ↔ auto-name; see the iceberg-bugs and verify-breadth memories):
   - **Leaf change** (one placeholder's copy/type, a parse edge case, a pure unexported helper) → relevant e2e is narrow or empty.
   - **Shared runtime / codegen change** (`R/paintr-runtime.R`, `R/paintr-server.R`, `R/paintr-substitute.R`, `R/paintr-ui.R`) → widens to the seeding/binding/source-coordinator e2e cluster; if you cannot confidently bound it, that **is** a full-e2e case — partial coverage on a shared-mechanism change is exactly where icebergs hide.

   Targeted e2e file/cluster (serial, browser ON via `devtools::test()` setting `NOT_CRAN`; `filter` is a regex on the name after `test-` and before `.R`, so it can match a cluster):
   ```
   Rscript -e 'devtools::test(filter="adr15-consumer-binding", reporter="progress", stop_on_failure=FALSE)'
   ```

Run the **full e2e suite** ONLY when the user explicitly asks, or at a **merge boundary** — the post-merge gate and the sibling-worktree landing check (kept deliberately: they cover the branch in isolation vs. integrated with everything else merged, not redundant). Parallelism (`TESTTHAT_PARALLEL=TRUE TESTTHAT_CPUS=4`) is opt-in for those full-suite runs only — see the authoritative-gate block in `CLAUDE.md`.

This governs iteration speed only. It does **not** relax the Definition of Done: nothing is "green" / done / mergeable until the full-suite authoritative gate has passed at the merge boundary. A relevant-e2e-only pass never substitutes for the gate and never grounds a "done" / "verified" claim.
