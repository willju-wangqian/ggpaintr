# ADR 0021 — Implementation plans

Source: [dev/adr/0021-pp-verb-switch.html](../../adr/0021-pp-verb-switch.html)
Generated: 2026-05-24 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0021-pp-verb-switch/` (run under `/goal`)

## Merge order

`01 → 02 → 03 → {04, 05} → 06`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01
- **G2**: 02
- **G3**: 03
- **G4**: 04, 05   (touch disjoint files — `R/paintr-disable.R` + a deletion in `R/paintr-translate.R` vs. `R/paintr-build-ui.R` — safe to implement concurrently)
- **G5**: 06

## Plans

| #  | Slug | Group | Depends on | Status | One-line summary |
|----|------|-------|------------|--------|------------------|
| 01 | node-constructor-and-comparator | G1 | — | PASS | Add `has_user_control` + `stage_label` slots to `ptr_call`; extend the structural-equality exclusion list with `stage_id`, `has_user_control`, `stage_label`. |
| 02 | pp-verb-switch-keyword | G2 | 01 | PASS | New exported `ppVerbSwitch(.data, verb_expr, switch_on = TRUE, label = NULL)` in `R/paintr-builtins.R`; entry in `ptr_builtin_keywords()`; structural-keyword registration. |
| 03 | pp-verb-switch-translate | G3 | 01, 02 | PASS | Translate-time validators (`switch_on`, `label`), `unwrap_pp_verb_switch_stage`, build_pipeline_from_lift wiring, structural-wrapper assertion extension. |
| 04 | gate-disjunction-and-stamp-removal | G4 | 03 | PASS | Extend `is_data_chain_call` to `placeholder OR has_user_control`; delete `stamp_default_stage_enabled_ids` + its caller; preserve `ppVerbOff(hide=TRUE)`-boots-OFF behaviour (Option A: stamp `has_user_control` in `unwrap_pp_verb_off_stage`). |
| 05 | ui-emission-stage-label | G4 | 03 | PASS | Thread `stage_label` through `entries` carrier in `R/paintr-build-ui.R`; user-label-OR-auto-label head choice in `build_pipeline_stage_ui` + `wrap_shared_widgets_with_stage_blocks`. |
| 06 | hard-remove-pp-verb-off | G5 | 02, 03, 04, 05 | PASS | Remove `ppVerbOff` everywhere: function body, keyword/registry entries, translate helpers, `man/pp_off_toggles.Rd` split, two vignettes, six test files renamed `test-pp-off-*.R` → `test-pp-toggles-*.R`, three fixtures renamed. |

## Blocked plans

(none — all six plans PASS /implementable + drift audit, stamped 2026-05-25)

## Drift-audit notes

Drift-audit pass 1 (six sub-agents in parallel) caught three FAIL findings and a handful of minor source-citation off-by-ones. Fixes applied inline; plans re-stamped with fresh hashes. Summary:

- **Plan 01** — PASS as-drafted. Added a one-line implementation note clarifying that `expr = NULL` default is added to the existing `ptr_call` signature alongside the two new slots (D4-borderline finding; the SC implied this but did not state it explicitly).
- **Plan 02** — PASS as-drafted. Tightened the `R/paintr-builtins.R:485-495` citation in §"Governing principle" to `:486-496` (covers `substitute()` through `eval(parent.frame())`; the original range missed the final `eval` line).
- **Plan 03** — FAIL→PASS. Resolved internal inconsistency: §"Context" cited `ptr_assert_no_surviving_structural_wrappers` at `R/paintr-translate.R:161-191` (correct, matches source) while §"Constraints" cited the same symbol at `:160-190`. Both now read `:161-191`.
- **Plan 04** — FAIL→PASS. Two fixes:
  1. SC-6's verification mechanism was rewritten. The original assertion read `ptr_runtime_input_spec(...)` checking a `default == FALSE` column that does not exist in the function's output schema (`R/paintr-input-spec.R:6-9` — columns are `input_id, role, layer_name, keyword, param_key, source_id, shared`). New mechanism: tree-side assertion on the carrier `ptr_call`'s `default_stage_enabled` and `stage_id` slots after `assign_stage_ids` completes. Same regression coverage; achievable assertion.
  2. Constraints' citation of `stamp_default_stage_enabled_ids` body corrected from `:130-152` to `:131-153` (matches §"Context"); the "immediately-preceding comment block" range tightened from `:102-116` to `:109-116` (the block at lines 102-107 is an unrelated ADR-0020 comment about `ptr_assert_no_surviving_structural_wrappers` and stays). Added an Option-A placement note that the `has_user_control` stamp goes AFTER `translate_node(verb_expr)` returns so it lands on the `ptr_call`, not on the raw language object.
- **Plan 05** — PASS as-drafted. Auditor flagged the ADR's "L234-237" wording as off-by-one against an L234-236 region but the recipe-and-constraint pair is unambiguous; left as is.
- **Plan 06** — FAIL→PASS. `R/paintr-builtins.R:476-496` corrected to `:477-497` (the original off-by-one would have deleted `#' @export` at line 476 while leaving the closing `}` at line 497 orphaned at the head of the next function). Roxygen-split instruction tightened with the existing `@rdname pp_off_toggles` tag locations (lines 464 + 475) and the explicit `#' @name ppLayerOff` target so the implementer can mechanically perform the split.

A second drift-audit round was not run on the corrected plans because each fix was a surgical citation/mechanism correction that directly matched the auditor's recommended FIX SUGGESTION — re-auditing would re-verify the same source-of-truth checks already performed in pass 1.

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in the same parallel_group (G4 = plans 04, 05) all branch off the same point (the latest merged commit before the group).
- Definition-of-Done command is in every plan; run it on the orchestrator branch after every merge.
- The Definition-of-Done command is identical in every plan (copied verbatim from `CLAUDE.md`'s "Authoritative gate" block) — `NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_dir("tests/testthat", reporter="progress", stop_on_failure=FALSE)'` with expected `FAIL 0 / ERROR 0 / SKIP 0 / PASS N`.
- Plan 04's SC-6 names two acceptable implementations (Option A: stamp `has_user_control = TRUE` in `unwrap_pp_verb_off_stage`; Option B: keep a narrowly-scoped stamping pass and delete it in plan 06). The Option-A choice is preferred and recorded in the post-impl audit.
- Plan 06 is the only plan that changes `man/` and `vignettes/` content; intermediate plans rely on the preserved `ppVerbOff` paths for regression coverage. Do not let plan 06 land in parallel with any earlier plan.

<!-- implementable: index — manifest, not a contract plan; the stamp below satisfies the pre-commit hook (which checks any dev/plans/*.md). The six contract plans (01–06) carry their own PASS stamps. -->
<!-- implementable: PASS date=2026-05-25 gate="N/A (manifest)" hash=6cead03c594e -->
