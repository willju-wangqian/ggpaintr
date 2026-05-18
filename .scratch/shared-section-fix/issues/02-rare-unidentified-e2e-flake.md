# Rare unidentified intermittent failure in the NOT_CRAN e2e gate (~1 in 27)

Status: fixed

## Origin

Surfaced during W4+W3 integration of `dev/plans/2026-05-17-shared-section-fix.html`. The first-ever `NOT_CRAN=true … test_dir` run on the W4+W3 worktree reported `[ FAIL 1 | WARN 0 | SKIP 0 | PASS 1636 ]`. It was **never reproduced** in **26 subsequent full-suite runs** on the same worktree (all `FAIL 0 | PASS 1637`), and the failing-test block was never captured (the failing run's log was overwritten before extraction; the diagnostic loops that followed never re-triggered it).

## Evidence / attribution

- W4+W3 worktree: **1 FAIL in 27 runs (~3.7%)**, the single occurrence being the cold first run.
- Base `vignette-review` @ `c9d2239` (no W4/W3): **0 FAIL in 5 runs**.
- W3/W4 source change is tiny and deterministic (copy strings + one `shared_panel_body_tag()` `NULL`-return guard). A logic regression there would fail every run, not 1/27. W3/W4 unit tests (`test-copy-rules.R`, `test-shared-coordinator.R`) passed in **all 27** runs.
- Total test count constant (1637) between green and the one red run → exactly one normally-passing test intermittently failed; not a new-test error.
- Post-merge (`796ba6e`) integrator re-verify: authoritative gate `FAIL 0 / PASS 1637`; official `--as-cran` check `0 errors / 0 warnings / 2 notes`.

Conclusion: **not a W3/W4 logic defect.** Most plausibly a first-run environment transient (initial chromote/Chrome launch, port/disk cold) or this repo's **pre-existing order-dependent shinytest2 isolation class** — see commit `77a927e` ("fix order-dependent shinytest2 lockstep isolation defect") and project memory `shinytest2-appdir-pkgload`. Merged W4+W3 per explicit decision (26/27 + clean base + verified deterministic logic), surfacing this rather than absorbing it.

## Next steps (triage)

- Add robust failure capture to any future flake hunt: write each run to a distinct persistent log AND, on `FAIL>0`, immediately archive the full log + extract the `── Failure/Error (test-….R:NN)` block before the next run (the capture gap is why this is unidentified).
- Consider `testthat::test_dir(..., reporter = testthat::JunitReporter$new(file=...))` per-run so a flaky failure records the exact test/assertion automatically.
- If it recurs, prime suspects are the `*-shinytest2.R` lockstep/isolation tests (the known class), not the W3/W4 units. Check whether W4's now-`NULL` shared panel removes a DOM node another e2e test transitively assumed present under a specific run order.

## Investigation 2026-05-17 — strong hypothesis (capture in flight, NOT yet confirmed)

Static asymmetry found: `prune_dead_ggpaintr_resource_paths()` (the blessed fix for this repo's known order-dependent shinytest2 isolation hazard, documented in `tests/testthat/helper-shinytest2.R:1-27`) is called by `test-shared-lockstep-shinytest2.R:22` but is **NOT** called by `boot_vignette_app()` (`tests/testthat/helper-vignette-apps.R`), which boots every fixture in `test-e2e-vignette-examples-shinytest2.R` (~20+ AppDriver boots).

Mechanism (verbatim from helper-shinytest2.R): a user-css test registers a process-global `ggpaintr-user-<hash>` resourcePath into a `withr` tempdir; the tempdir is torn down at scope exit but the registration lingers; `AppDriver$new()` replays the parent's `shiny::resourcePaths()` into the child; a replayed entry whose dir is gone makes `addResourcePath()` abort ("Couldn't normalize path") → the app fails to boot → that e2e test ERRORs. Manifests only if a css test ran earlier in the process AND its tempdir was already removed by the time a later `boot_vignette_app()` fires → order- and timing-dependent → consistent with the observed ~1/27 single-test ERROR (constant total count).

Predicted fix (the exact prescribed one-liner, already blessed in `test-shared-lockstep-shinytest2.R`): call `prune_dead_ggpaintr_resource_paths()` at the top of `boot_vignette_app()` before `AppDriver$new()`. Semantically a no-op for live paths; touches no product behaviour.

**Not applied — and this hypothesis was DISCONFIRMED by the capture (kept here as a record, not a conclusion).** The JUnit-instrumented 40-run loop caught run 16 (`FAIL 1 | PASS 1638`, 1/40 ≈ 2.5%); `/tmp/iss02/j_16.xml` + `run_16.log` show the failing test is **not** a resourcePath/boot abort.

## Captured root cause 2026-05-17 (confirmed, JUnit artifact j_16.xml)

Failing test: `test-e2e-vignette-examples-shinytest2.R:337:3` — *"gallery plotly-paintr (§5.1): module + custom plotly host output"*. Assertion `expect_no_inline_error(app, "plotly_demo-ptr_error")`: `actual TRUE / expected FALSE` — an inline error was present at the sampling instant. Sequence (lines 331-337): `set_input(plotly_demo-ggplot_1_1_var_NA, "displ")`, `set_input(..._1_2_var_NA, "hwy")`, `draw()`, then assert no error. 39/40 runs identical → clean; 1/40 → transient error sampled. A deterministic product bug would fail every run, so this is a **test sampling/timing race, not a product defect**.

Mechanism (matches memory `shinytest2-appdir-pkgload` gotchas #2 + #4): `plotly_demo-ggplot_1_{1,2}_var_NA` are **consumer var pickers**, suspended in a `renderUI` under the layer Data subtab and not bound until that subtab is shown. The test sets them with `set_input(wait_=FALSE)` **without** the prescribed subtab-activation dance, so a consumer-resolution `shiny.silent.error` can briefly flush into the custom-plotly host's error pane before the draw settles; `draw()`'s `wait_for_idle` can return while that transient is still on screen. The `plotly-paintr` fixture is untouched by the shared-section-fix plan → pre-existing, latent, unrelated to W1–W4.

The prune-asymmetry (above) remains a *separate, real latent isolation gap* worth closing on its own merits (the documented hazard + the `test-shared-lockstep-shinytest2.R` vs `boot_vignette_app()` asymmetry are factual), but it was **not** the cause of the observed flake. Do not conflate the two.

### Resolution 2026-05-17 (test-only, scoped per-test sync)

Applied: in `test-e2e-vignette-examples-shinytest2.R`, §5.1 (plotly-paintr) and §5.2 (ggiraph-paintr, by parity) now call `app$wait_for_idle(timeout = 25 * 1000)` after the custom-host render assertions and **before** `expect_no_inline_error(...)`. The custom host (`req(isTRUE(res$ok))`) drives an extra post-draw reactive flush; `draw()`'s first `wait_for_idle` can return while the bundled error pane still holds a transient pre-quiescence value. The added settle makes the assertion sample the error reactive's **final, settled** state. No product code touched; no test-count change (still 1639).

**Basis of correctness — mechanism first, loop confirmatory (stated honestly, not overclaimed):**
- *Primary:* `wait_for_idle` blocks until Shiny reports sustained reactive quiescence, so the only state `expect_no_inline_error` can observe is the post-flush settled one (proven clean by 39/40 pre-fix + the transient nature). The race is removed by construction.
- *Confirmatory:* same JUnit-instrumented harness, before vs after — **pre-fix 1 fault / 40 runs; post-fix 0 faults / 40 runs** (40/40 `FAIL 0 / PASS 1639`, `/tmp/iss02fix/`). Statistical caveat: at the observed ~2.5% base rate, 0/40 alone has P(0 | unfixed) ≈ 0.36 — weak in isolation; it corroborates the mechanism argument, it does not independently prove elimination.

Full authoritative gate re-run after the fix: `FAIL 0 / WARN 0 / SKIP 0 / PASS 1639` (no regression). The prune-asymmetry (separate latent isolation gap, NOT this occurrence's cause) was tracked in `dev/notes/2026-05-17-e2e-gate-perf.html` and is now **closed** — see below.

## Prune-asymmetry closed 2026-05-18 (the separate latent gap, not this flake)

`boot_vignette_app()` (`tests/testthat/helper-vignette-apps.R`) now calls `prune_dead_ggpaintr_resource_paths()` before `AppDriver$new()`, matching what `test-shared-lockstep-shinytest2.R:22` already did — the asymmetry is gone. Pruning a resource path whose dir no longer exists is a semantic no-op for live paths (helper-shinytest2.R), so this hardens isolation (esp. order-dependent css-test-then-vignette-boot sequences) with zero behaviour change. Authoritative gate after the fix: `FAIL 0 / WARN 0 / SKIP 0 / PASS 1655` — unchanged count, confirming the no-op-for-live-paths property. This also removes the hard isolation prerequisite for any future e2e parallelism (perf note Bottom line); the remaining parallelism work (split the e2e file, enable parallel config, re-trust via the 40× JUnit loop) stays deferred behind an explicit trigger.

## Notes

Low frequency, unattributed, not blocking (both gates deterministically green across 26+ runs and post-merge). Tracked so it is not silently forgotten; not in scope for the shared-section-fix plan (all of W1–W4 + the harness fix are complete and verified).
