# ADR 0015 — Implementation plans

Source: [dev/adr/0015-consumer-picker-source-headed-resolution.html](../../adr/0015-consumer-picker-source-headed-resolution.html)
Generated: 2026-05-23 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0015-consumer-picker-source-headed-resolution/` (run under `/goal`)

## Merge order

`01` → `02` (sequential; PLAN-02 supersedes PLAN-01's Option-A workaround tail but BUILDS ON its SC-1/SC-2/SC-3/SC-4 head)

## Parallel groups

- **G1**: 01 — eager-bind architecture (landed on `followup-b`).
- **G2**: 02 — Option E structural race fix (this PR set). Depends on G1.

## Plans

| #  | Slug                                            | Group | Depends on | Status     | One-line summary |
|----|-------------------------------------------------|-------|------------|------------|------------------|
| 01 | eager-bind-source-headed-consumer-pickers       | G1    | —          | merged     | Drop pre-warm visibility-gate + add `req()` source-ready guard + drop subtab dep in `ptr_setup_consumer_uis()`; add symmetric `state$resolved_data` dep loop in `ptr_bind_shared_consumer_uis()`. |
| 02 | server-side-bound-names                         | G2    | 01         | stamped    | Option E: server-side `state$bound_names[[id]]` reactiveVal written by source observers after `assign()`; consumers `req()` it instead of polling `eval_env`. Supersedes PLAN-01's `invalidateLater(50)` + `cache=NULL` workaround tail; restores `state$upstream_cache`; same fix in shared consumer site. |

## Blocked plans

None.

## Notes for the orchestrator

- PLAN-02's worktree base is the orchestrator HEAD on `followup-b` (or `post-add-expr` if `followup-b` has merged by then — confirm before naming the branch).
- The Definition-of-Done command must be run from the worktree post-implement and post-merge. Bare `Rscript -e 'testthat::test_dir(...)'` without `NOT_CRAN=true` silently SKIPs every shinytest2 test — not the gate.
- PLAN-02's ordering invariant (`assign()` before `state$bound_names[[key]](name)`) is enforced by the new `bind_source_value()` helper. Auditor must verify exactly ONE write to `state$bound_names` exists in the codebase (inside the helper body).
- Post-merge: ADR-0013 PLAN-04 was unblocked by PLAN-01 already; PLAN-02 has no new downstream dependants.

<!-- implementable: PASS date=2026-05-23 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=0b6e7dc9a819 -->
