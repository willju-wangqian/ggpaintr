# ADR 0012 Bug B — spec-apply uniform via build_ui (Implementation plans)

Source: [dev/adr/0012-role-based-tree-and-ptr-spec.html](../../adr/0012-role-based-tree-and-ptr-spec.html) (successor concern: PLAN-06's `apply_spec_entry` is structurally exclusive to built-in keywords).
Source handoff: `/var/folders/86/znw5mx352fj9523j26lssygh0000gn/T/handoff-XXXXXX.md.KONpWBGcUL` (2026-05-22, framing locked option `(P)` "pure uniform spec-apply via build_ui via renderUI + state$spec_seed").
Generated: 2026-05-22 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0012b-spec-apply-uniform/` (run under `/goal`)

## Merge order

`01 → 02`

(Sequential. PLAN-02 strictly depends on PLAN-01's `state$spec_seed`, setup helpers, and `build_ui_for.*` reshapes.)

## Parallel groups

- **G1**: 01 — spec-seed-and-renderui-emission
- **G2**: 02 — collapse-apply-spec-entry-and-revert-workarounds

## Plans

| #  | Slug | Group | Depends on | Status | One-line summary |
|----|------|-------|------------|--------|------------------|
| 01 | spec-seed-and-renderui-emission                       | G1 | —  | draft | Adds `state$spec_seed`, reshapes `build_ui_for.ptr_ph_value` + `build_ui_for.ptr_ph_data_source` to emit `uiOutput` containers, adds `ptr_setup_value_uis` + `ptr_setup_source_uis`, threads seed into `ptr_setup_consumer_uis`'s `renderUI`. Both seed path and legacy `updateXyzInput` path coexist; custom-keyword spec entries now apply via seed (Bug B fix). |
| 02 | collapse-apply-spec-entry-and-revert-workarounds      | G2 | 01 | draft | Removes `updateXyzInput` calls from `apply_spec_entry` (placeholder rows only — framework-internal `layer_checkbox` / `stage_enabled` / `source_companion` keep their `updateCheckboxInput` / `updateTextInput`); spec-apply becomes seed-only for placeholders. Reverts `app$get_value` workarounds in `test-adr12-spec-roundtrip.R` to `app$get_html`-based assertions where shinyWidgets' server-rendered HTML now carries `<option selected>`. |

## Deferred to future rounds

- **Step C (framework-internal widgets to renderUI)** — moving `stage_enabled`, `layer_checkbox`, `source_companion` to `renderUI` for full uniformity (~30 extra lines). Out of scope per the handoff's two open uncertainties: these are not registry placeholders, so the governing principle does not bind them. Defer to a user-gated plan only if requested.
- **Per-plot vs combined spec accessor at the grid level** — ADR 0012 §5 OQ3.
- **shinyWidgets HTML-divergence investigation** — if PLAN-02's SC-3 probe shows `app$get_html` cannot observe the picker selection (e.g., it lives in a sibling JS-driven div), open a follow-up plan rather than absorbing the workaround.
- **shinytest2 dynamic-input round-trip test (drift-audit M2)** — under shinytest2 the dynamic `numericInput` created by `renderUI` does not push its initial value back to the server, so `app$get_value(input = "geom_point_2_2_ppNum_NA")` returns NULL even after `wait_for_idle(15s)`. `test-adr12-spec-roundtrip.R:67–75` migrated to `app$get_html` (matches `value="5"`) to capture the renderUI→HTML contract literally. Open question: does the user-facing reactive expression that reads `input$<raw_id>` in production also receive NULL until first user interaction? Recommended follow-up: a shinytest2 test that interacts with the widget (e.g., `app$set_inputs(<id> = 5, wait_ = FALSE)` then `wait_for_idle()` then `app$get_value(input = <id>)`) to close the round-trip. Out of scope for Bug B; see `dev/notes/0012b-drift-audit-2026-05-22.html` Finding M2.
- **ppUpload `source_companion` seed-write coverage (M3 addendum follow-up)** — no existing test asserts the `source_companion` seed-write triggered specifically by a `ppUpload` spec entry. The `test-adr12-spec-roundtrip.R` fixture uses `ppVar`, `ppNum`, `layer_checkbox`, and one unknown id — none of which produce a `source_companion` row. A ~25-line `test_that` block exercising `spec = list(ggplot_1_1_ppUpload_NA = "ignored.csv", ggplot_1_1_ppUpload_NA_name = "my_data")` would close the gap. Optional. See `01-addendum-ppupload-silent.html` SC-A2.

## Blocked plans

(None — both plans drafted; auto-verdict and drift audit reported below.)

## Notes for the orchestrator

- Both plans land in the same worktree (`.claude/worktrees/bug-b-spec-apply-uniform/`) on branch `bug-b-spec-apply-uniform`, branched off `vignette-review@a76310a`. Baseline gate (DoD command) at `a76310a` verified 2026-05-22 in this session: `FAIL 0 / WARN 0 / SKIP 2 / PASS 2181` (107.1s).
- The DoD command (authoritative from `CLAUDE.md`) is identical for both plans: `NOT_CRAN=true Rscript -e 'suppressMessages(devtools::load_all(".")); testthat::test_dir("tests/testthat", reporter="progress", stop_on_failure=FALSE)'`. Expected: `FAIL 0 / WARN 0 / SKIP 2 / PASS N` with N rising as new fixtures land.
- The parallel Bug A session owns `R/paintr-classify.R`, `R/paintr-walk.R`, `R/paintr-translate.R`, `R/paintr-shared.R`, and the `consumer-colvars` + `adr12-bug-3a` fixtures. Coordinate via `git status` before any final merge to `vignette-review`; conflicts there are Bug-A territory, not this plan's.
- Each plan's "done" report is a claim — re-run the DoD command in the worktree before merging. Per `CLAUDE.md` proxy traps: only `NOT_CRAN=true` / `devtools::test()` actually runs the browser e2e file.
