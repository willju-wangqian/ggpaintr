# Rare unidentified intermittent failure in the NOT_CRAN e2e gate (~1 in 27)

Status: needs-triage

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

## Notes

Low frequency, unattributed, not blocking (both gates deterministically green across 26+ runs and post-merge). Tracked so it is not silently forgotten; not in scope for the shared-section-fix plan (all of W1–W4 + the harness fix are complete and verified).
