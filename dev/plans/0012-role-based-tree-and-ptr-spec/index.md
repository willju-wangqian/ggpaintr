# ADR 0012 — Implementation plans

Source: [dev/adr/0012-role-based-tree-and-ptr-spec.html](../../adr/0012-role-based-tree-and-ptr-spec.html)
Generated: 2026-05-21 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0012-role-based-tree-and-ptr-spec/` (run under `/goal`)

## Merge order

`01 → 02 → {03, 04} → 05 → 06`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01
- **G2**: 02
- **G3**: 03, 04   (touch disjoint files — plan 03 owns a new test file only; plan 04 owns the runtime deletion + a new e2e fixture + a new test file. No file overlap.)
- **G4**: 05
- **G5**: 06

## Plans

| #  | Slug                          | Group | Depends on | Status | One-line summary |
|----|-------------------------------|-------|------------|--------|------------------|
| 01 | classify-calls-noop-pass      | G1    | —          | PASS   | Reserve the `ptr_classify_calls` slot in `ptr_translate` as a true no-op identity pass; baseline tree shape unchanged. |
| 02 | resugar-pipeline-lift         | G2    | 01         | PASS   | Implement `resugar_pipeline` + terminal-grounding inside `ptr_classify_calls`; nested-call and `|>` data args now lift into the same `ptr_pipeline` shape `%>%` already produces. |
| 03 | bug-3b-regression-net         | G3    | 02         | PASS   | New `test-prune-bare-pipe-stage.R` asserting empty `filter()` elides under `|>`, `%>%`, and nested-call surface forms; non-empty preserved. |
| 04 | delete-upstream-fast-path     | G3    | 02         | PASS   | Delete the per-layer `state$resolved_data[[layer_name]]` fast-path in `runtime_upstream_data`; add `adr12-bug-3a` browser fixture asserting in-filter pickers populate correctly after upload. |
| 05 | spec-emission-preserve-mode   | G4    | 04         | PASS   | Add `state$spec` reactive + `ptr_spec_from_snapshot` + `format_spec_for_panel`; preserve-mode code panel now emits a sparse `spec = list(...)` block alongside the formula. |
| 06 | spec-apply-session-boot       | G5    | 05         | PASS   | Thread `spec=` through `ptr_server_internal` and the four wrappers (`ptr_app`, `ptr_app_bslib`, `ptr_app_grid`, `ptr_server`); apply via `updateXyzInput` at session boot; add round-trip browser fixture. |

## Blocked plans

(Populated by auto-verdict loop if any plan stays FAIL after 3 /implementable attempts.)

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in the same parallel_group all branch off the same point (the latest merged commit before the group).
- Definition-of-Done command is in every plan; run it on the orchestrator branch after every merge.
- Plan 04 and plan 05 both touch `R/paintr-server.R`. They are NOT in the same parallel group — plan 05 depends on plan 04 by file-conflict sequencing (recorded in plan 05's `depends_on`).
- Plans 03 and 04 share parallel group G3 but touch disjoint files: plan 03 only adds `tests/testthat/test-prune-bare-pipe-stage.R`; plan 04 touches `R/paintr-server.R` (deletion) + adds an e2e fixture + adds `test-adr12-bug-3a.R`. Safe to implement concurrently.

## Deferred to future rounds

These items are explicitly out of scope for the S1–S6 staging per ADR 0012:

- **Render-time pipe-style preservation** (ADR 0012 §5 open question 2): track which surface form (`|>` vs `%>%`) the user wrote per stage, render preserve-mode panel accordingly. Pure cosmetic; default to `|>` for now.
- **Per-plot grid spec accessor** (variant of ADR 0012 §5 open question 3): the combined-flat spec is built in S5 via `ptr_spec_combine()` and exposed by the grid in S6. A separate per-plot accessor (un-combined list-of-specs) is a future enhancement.
- **`ptr_migrate_spec(old, from = "...")` helper** (ADR 0012 §2 row "Versioning"): only needed once the id contract is broken — not on day one.
- **Reactive re-application of spec** (ADR 0012 §2 row "Apply timing of spec="): one-shot at boot is the locked decision; a watcher/reset-button is explicitly out of scope.

<!-- implementable: PASS date=2026-05-21 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=0b0bf695003b -->

