# ADR 0020 — Implementation plans

Source: [dev/adr/0020-pp-layer-off-pp-verb-off-toggles.html](../../adr/0020-pp-layer-off-pp-verb-off-toggles.html)
Generated: 2026-05-24 by `/decision-to-plan`
Orchestrator entry point: `/exec-plan dev/plans/0020-pp-layer-off-pp-verb-off-toggles/` (run under `/goal`)

## Merge order

`01 → {02, 03} → 04 → 05`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01
- **G2**: 02, 03   (depend on 01; touch disjoint files — 02 = headless.R / server.R / build-ui.R; 03 = nodes.R comparator + test-adr12-consumer-uniformity.R)
- **G3**: 04   (depends on 02; touches many files that 02 also touched, so cannot parallel)
- **G4**: 05   (depends on 04; sequential so vignette is written against post-deprecation code)

## Plans

| #  | Slug                                | Group | Depends on | Status | One-line summary |
|----|-------------------------------------|-------|------------|--------|------------------|
| 01 | translate-wiring                    | G1    | —          | draft  | Add `ppLayerOff` / `ppVerbOff` exported stubs, registry recognition, special-unwrap translate rules, `default_active` / `default_stage_enabled` node-field stamping. |
| 02 | readers-honor-new-fields            | G2    | 01         | draft  | Snapshot (headless.R) + 4 UI sites (build-ui.R) + server-side `ptr_init_state` / `ptr_spec_defaults_from_state` all switch from `checkbox_defaults` consultation to direct `node$default_active` / `node$default_stage_enabled` reads. |
| 03 | comparator-and-adr12-test-switch    | G2    | 01         | draft  | Extend `ptr_tree_structural_equal()`'s exclusion list to ignore the new fields; switch `test-adr12-consumer-uniformity.R` from its inline class-dispatched helper to the real `ggpaintr:::ptr_tree_structural_equal`. |
| 04 | deprecate-checkbox-defaults         | G3    | 02         | draft  | Remove the `checkbox_defaults =` formal across the public API (8 entry points + 1 internal); remove the `checkbox_default_all_other_layer` option; delete `R/paintr-checkbox-defaults.R`, `tests/testthat/test-checkbox-defaults.R`, the `resolve_layer_default()` helper, the deferred-test skips Plan 02 added; migrate vignettes + manual fixtures to `ppLayerOff`. |
| 05 | docs-use-cases-vignette             | G4    | 04         | draft  | Add a use-cases-vignette section covering `ppLayerOff` / `ppVerbOff` with motivation, runtime-symmetry example, `ppVerbOff` data-arg-position pitfall, and `spec =` precedence. |

## Blocked plans

_(empty — populated only if a plan fails to PASS after 3 `/implementable` attempts or 3 drift-audit cycles)_

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: each plan's worktree is branched off the orchestrator HEAD *after* the previous plan was merged. Plans in the same parallel_group (G2 = 02, 03) both branch off the same point (the latest merged commit before the group — here, the post-01 commit).
- **Definition-of-Done command is identical across every plan** (copied verbatim from `CLAUDE.md`'s "Authoritative gate" block):
  ```
  NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_dir("tests/testthat", reporter="progress", stop_on_failure=FALSE)'
  ```
  Expected output: **FAIL 0 / ERROR 0 / SKIP 0 / PASS N**. Each plan documents its expected PASS-delta (positive for 01/02/03/05, negative for 04).
- **Plan 02 may temporarily raise SKIP** by wrapping deprecated-machinery tests (in `test-checkbox-defaults.R`, `test-options.R`, `test-rewrite-server-state.R`, `test-rewrite-layer-panel.R`, `test-module-ui.R`) in `testthat::skip("superseded by ADR 0020 ppLayerOff; Plan 04 will delete this file")`. Plan 04 brings SKIP back to baseline by deleting or rewriting those tests. The integration auditor between 02 and 04 verifies the SKIP rise matches Plan 02's recorded delta, and the post-04 integration audit verifies SKIP has dropped back.
- **Plan 04 is the only plan with a negative PASS delta** — it deletes the 287-line `test-checkbox-defaults.R`, the four `test-options.R` blocks for the global option, and prunes/rewrites the `state$checkbox_defaults`-asserting tests. The audit must verify the drop equals (deleted-tests) - (rewritten-replacement-tests) and that no genuine regression is hidden in the delta.
- **Generated files** (`NAMESPACE`, `man/*.Rd`, `vignettes/*.html`, `vignettes/*.R`) are owned by the plan that triggers their regeneration:
  - Plan 01 triggers `NAMESPACE` + `man/ppLayerOff.Rd` + `man/ppVerbOff.Rd` (new `@export`s).
  - Plan 04 triggers regen of every man page that previously documented `checkbox_defaults =` (`man/ptr_app.Rd`, `man/ptr_app_bslib.Rd`, `man/ptr_init_state.Rd`, `man/ptr_options.Rd`, `man/ptr_server.Rd`, `man/ptr_ui.Rd`, `man/ptr_ui_controls.Rd`, `man/build_ui_for.Rd`).
  - Plan 05 triggers regen of `vignettes/ggpaintr-use-cases.html` + `vignettes/ggpaintr-use-cases.R`.
  Each plan's commit message should call out the regeneration so the post-merge auditor knows the diff is mechanical.
- **ADR 0020 omission caught during plan carving:** the ADR enumerated two reader sites (snapshot + UI); plan carving found a third (server.R `ptr_spec_defaults_from_state` + the `state$checkbox_defaults` field populated by `ptr_init_state`). Plan 02's scope was extended to include this site. The drift audit for Plan 02 should confirm the scope expansion is correctly bounded and that no fourth reader site exists.

## Deferred to future rounds

- The exact spelling of translate-time error messages (literal-only `hide`; wrong-position keyword). Plan 01 picks the wording; future copy-polish is out of scope here.
- Whether the same machinery should also let a user *hide* placeholder widgets (e.g. `ppText(initial = "hi", visible = FALSE)`). ADR 0020 explicitly defers this — separate ADR territory.
- The order in which `ppLayerOff` / `ppVerbOff` are introduced into the registry init pipeline (covered by ADR 0014, not this ADR).

<!-- implementable: PASS date=2026-05-24 gate="NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(\".\")); testthat::test_dir(\"tests/testthat\", reporter=\"progress\", stop_on_failure=FALSE)'" hash=62de0383d35c -->
