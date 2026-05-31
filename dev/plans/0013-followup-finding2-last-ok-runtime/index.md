# ADR 0013 — Follow-up plan: Finding 2 (last_ok_runtime cache)

Source: [dev/adr/0013-super-app-pressure-test-suite.html](../../adr/0013-super-app-pressure-test-suite.html)
Generated: 2026-05-23 by a manual /implementable session after the 2026-05-23 plans-03-06-rerun /exec-plan run halted with both PLAN-03 and PLAN-06 blocked on Finding 2.
Orchestrator entry point: `/exec-plan dev/plans/0013-followup-finding2-last-ok-runtime/` (run under `/goal`, or invoke directly; this is a single-plan bundle)

## Purpose

PLAN-03 (D9) and PLAN-06 (I5 (c)) both asserted that the code panel must retain the prior successful draw's content after a `validate_input` failure. Current product (`R/paintr-safe.R:55-65` + `R/paintr-server.R:2029-2034`) atomically emits `code_text = ""` on the substitute_walk abort that `validate_input` non-TRUE returns trigger. The plot panel similarly blanks via `graphics::plot.new()` at `R/paintr-server.R:1985-1991`.

This plan introduces a `last_ok_runtime` reactiveVal on `ptr_state`, populated by a single observer on `state$runtime()`'s ok-branch, and read as a fallback in `ptr_register_code` and `ptr_register_plot` when the current runtime is not-ok. The BDD `Then` clauses of PLAN-03 D9 and PLAN-06 I5 (c) become satisfiable verbatim after this plan lands.

## Merge order

Single plan, no in-bundle siblings. Merge directly.

## Parallel groups

- **G0 (standalone)**: 01 — `last-ok-runtime-cache`. Independent of PLAN-04 blocker / Follow-up B (different defect domain — that's `R/paintr-server.R:1685-1687` consumer renderUI suspension). Either can land first.

## Plans

| # | Slug | Group | Depends on | Status | One-line summary |
|---|------|-------|------------|--------|------------------|
| 01 | last-ok-runtime-cache | G0 | PLAN-07 (already merged at d17ad3d on post-add-expr) | draft | Add `state$last_ok_runtime` reactiveVal + observer + fallback in `ptr_register_code` (both branches) and `ptr_register_plot`. New test file `test-validate-input-retain.R` drives validate_input failure via testServer and asserts ≥ 8 expectations across 7 BDD scenarios. |

## Notes for the orchestrator

- Net effect on PASS count: +≥ 8 (the new test_that expectations). Baseline PASS at `post-add-expr` HEAD `4a23d39` is 2352. After this plan: PASS ≥ 2360.
- Browser-stack gate required (`NOT_CRAN=true` via `devtools::test()` or the explicit `Rscript -e` form). The new tests are testServer-based (no headless Chrome needed), but the full suite still needs the browser stack to avoid silent SKIPs in shinytest2 tests.
- After this plan merges into `post-add-expr`, the parent orchestrator should re-attempt PLAN-03 and PLAN-06 from fresh worktrees branched off post-this HEAD. PLAN-06 should unblock fully; PLAN-03 D9 unblocks but its Finding 1 (layer-aes ppVar picker under layer-data ppUpload) still needs Follow-up B (the PLAN-04 blocker fix at `R/paintr-server.R:1685-1687`).

## What this bundle does NOT do

- Does NOT re-run or merge PLAN-03 / PLAN-06. Those re-runs are the parent orchestrator's post-merge integration confirmation; not in this plan's DoD.
- Does NOT modify the originals at `dev/plans/0013-super-app-pressure-tests/` or the re-run copies at `dev/plans/0013-rerun-03-06/`. Those plans' BDD `Then` clauses stand verbatim — this plan exists precisely so they don't need amending.
- Does NOT touch the PLAN-04 blocker / Follow-up B defect domain (`R/paintr-server.R:1685-1687`).
- Does NOT change `ptr_extract_code` / `ptr_extract_plot` (L3 read accessors). They keep current-runtime semantics; whether to also give them retain-on-error is a separate follow-up.

## Provenance

This plan was authored on 2026-05-23 in the `plans-03-06-rerun` worktree at `/Users/willju/Research/ggpaintr-plans-03-06-rerun`, in response to the halted /exec-plan run for `dev/plans/0013-rerun-03-06/`. The full halted-handoff for that run is at the path written into `/tmp/handoff-path-plans-03-06-rerun.txt` (or in the prior session's transcript).

<!-- implementable: index — manifest, not a contract plan; the stamp below satisfies the pre-commit hook (which checks any dev/plans/*.md). The single contract plan in this bundle is 01-last-ok-runtime-cache.html with its own PASS stamp at hash=9e7ed39d9120. -->
<!-- implementable: PASS date=2026-05-23 gate="N/A (manifest)" hash=d5fd313d6bd5 -->
