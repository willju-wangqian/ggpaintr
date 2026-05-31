# ADR 0012 — Implementation plans

Source: [dev/adr/0012-role-based-tree-and-ptr-spec.html](../../adr/0012-role-based-tree-and-ptr-spec.html)
Generated: 2026-05-21 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0012-role-based-tree-and-ptr-spec/` (run under `/goal`)

## Merge order

`01 → {02, 04 (atomic pair)} → 03 → 05 → 06`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01
- **G2**: 02, 04   (**atomic pair** — both must PASS in-worktree audit; orchestrator merges 02 first, then 04 immediately after, with no other plan between. Neither may merge alone. See "Notes for the orchestrator" for the rationale.)
- **G3**: 03
- **G4**: 05
- **G5**: 06

## Plans

| #  | Slug                          | Group | Depends on | Status | One-line summary |
|----|-------------------------------|-------|------------|--------|------------------|
| 01 | classify-calls-noop-pass      | G1    | —          | PASS   | Reserve the `ptr_classify_calls` slot in `ptr_translate` as a true no-op identity pass; baseline tree shape unchanged. |
| 02 | resugar-pipeline-lift         | G2    | 01         | PASS   | Atomic-pair with 04: 4-step desugar → split → round-trip → ground engine in `ptr_classify_calls`; all three surface forms (`|>`, `%>%`, nested-call) lift into one canonical `ptr_pipeline` shape; retire `%>%`-only path; pass reorder `classify_data → classify_calls → assign_ids → shared_bind`. |
| 03 | bug-3b-regression-net         | G3    | 02, 04     | PASS   | New `test-prune-bare-pipe-stage.R` asserting empty `filter()` elides under `|>`, `%>%`, and nested-call surface forms; non-empty preserved. |
| 04 | delete-upstream-fast-path     | G2    | 01         | PASS   | Atomic-pair with 02: delete the per-layer fast-path in both `runtime_upstream_data` and `runtime_consumer_entry`; preserve-mode prefix-collapse render rule; consumer-uniformity test net across the three surface forms; `adr12-bug-3a` browser fixture. |
| 05 | spec-emission-preserve-mode   | G4    | 04         | PASS   | Add `state$spec` reactive + `ptr_spec_from_snapshot` + `format_spec_for_panel`; preserve-mode code panel now emits a sparse `spec = list(...)` block alongside the formula. |
| 06 | spec-apply-session-boot       | G5    | 05         | PASS   | Thread `spec=` through `ptr_server_internal` and the four wrappers (`ptr_app`, `ptr_app_bslib`, `ptr_app_grid`, `ptr_server`); apply via `updateXyzInput` at session boot; add round-trip browser fixture. |

## Blocked plans

(Populated by auto-verdict loop if any plan stays FAIL after 3 /implementable attempts.)

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in the same parallel_group all branch off the same point (the latest merged commit before the group).
- Definition-of-Done command is in every plan; run it on the orchestrator branch after every merge.
- Plan 04 and plan 05 both touch `R/paintr-server.R`. They are NOT in the same parallel group — plan 05 depends on plan 04 by file-conflict sequencing (recorded in plan 05's `depends_on`).
- **G2 is an atomic pair.** Plan 02 (canonical-shape lift in `ptr_classify_calls`) and plan 04 (downstream-consumer uniformity + preserve-mode prefix-collapse render + runtime fast-path deletion in both `runtime_upstream_data` and `runtime_consumer_entry` + `adr12-bug-3a` e2e fixture) MUST merge together — plan 02 first, then plan 04 immediately after, with no other plan between them. Neither may merge alone. Rationale: PLAN-02 lifts data_arg subtrees to the canonical `ptr_pipeline` shape; PLAN-04 makes downstream consumers (eval / prune / safety / picker upstream resolution / preserve-mode render) handle that shape uniformly across `|>`, `%>%`, and nested-call surface forms. A half-state where only one is merged reproduces the cd9b74a-era narrowing risk that was reverted at commit `f2954e5` (see ADR 0012 §1 "the tree is semantic, not syntactic"; see also PLAN-02's BDD "Atomic pair — PLAN-02 alone fails the consumer-uniformity test net"). The post-merge integration audit for G2 verifies BOTH plans' Definition-of-Done together against the merged-02-then-merged-04 state.

## Deferred to future rounds

These items are explicitly out of scope for the S1–S6 staging per ADR 0012:

- **Render-time pipe-style preservation** (ADR 0012 §5 open question 2): track which surface form (`|>` vs `%>%`) the user wrote per stage, render preserve-mode panel accordingly. Pure cosmetic; default to `|>` for now.
- **Per-plot grid spec accessor** (variant of ADR 0012 §5 open question 3): the combined-flat spec is built in S5 via `ptr_spec_combine()` and exposed by the grid in S6. A separate per-plot accessor (un-combined list-of-specs) is a future enhancement.
- **`ptr_migrate_spec(old, from = "...")` helper** (ADR 0012 §2 row "Versioning"): only needed once the id contract is broken — not on day one.
- **Reactive re-application of spec** (ADR 0012 §2 row "Apply timing of spec="): one-shot at boot is the locked decision; a watcher/reset-button is explicitly out of scope.

<!-- implementable: PASS date=2026-05-22 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=4a9bf6cb1602 -->

