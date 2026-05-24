# ADR 0015 — Implementation plans

Source: [dev/adr/0015-consumer-picker-source-headed-resolution.html](../../adr/0015-consumer-picker-source-headed-resolution.html)
Generated: 2026-05-23 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0015-consumer-picker-source-headed-resolution/` (run under `/goal`)

## Merge order

`01` (single plan)

## Parallel groups

- **G1**: 01 — only plan in the set. ADR 0015's scope is two narrow edits to one source file + one new regression-test file + one new fixture directory; partitioning into multiple plans would split a single coherent change across worktrees with no parallelism benefit.

## Plans

| #  | Slug                                            | Group | Depends on | Status | One-line summary |
|----|-------------------------------------------------|-------|------------|--------|------------------|
| 01 | eager-bind-source-headed-consumer-pickers       | G1    | —          | draft  | Drop pre-warm visibility-gate + add `req()` source-ready guard + drop subtab dep in `ptr_setup_consumer_uis()`; add symmetric `state$resolved_data` dep loop in `ptr_bind_shared_consumer_uis()`. Ships new fixture `adr15-consumer-binding/` + new test file `test-adr15-consumer-binding.R` with three `test_that` blocks (non-shared upload, shared upload, no-source pre-warm). |

## Blocked plans

None.

## Notes for the orchestrator

- This plan's worktree base is the orchestrator HEAD on `followup-b` (currently `4a23d39`).
- The Definition-of-Done command must be run from the worktree post-implement and post-merge. Bare `Rscript -e 'testthat::test_dir(...)'` without `NOT_CRAN=true` silently SKIPs every shinytest2 test — not the gate.
- Post-merge: ADR-0013 PLAN-04 (`04-app-2b-customsource-splice`) becomes implementable. That plan lives in `dev/plans/0013-super-app-pressure-tests/` and is owned by the ADR-0013 plan set; do NOT touch it from this exec-plan run.
- This plan's owned-files list explicitly excludes `tests/testthat/test-super-pressure.R` so PLAN-04 can land its own super-2b block cleanly afterwards.

<!-- implementable: PASS date=2026-05-23 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=ff6b07051c77 -->
