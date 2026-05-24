# ADR 0013 — PLAN-03 rerun bundle (App-2a upload + registry + mid-pipeline)

Source: [dev/adr/0013-super-app-pressure-test-suite.html](../../adr/0013-super-app-pressure-test-suite.html)
Generated: 2026-05-23 in the `plan-03` worktree at `/Users/willju/Research/ggpaintr-plan-03`, off `post-add-expr` HEAD `6ee63d7`.
Orchestrator entry point: `/exec-plan dev/plans/0013-plan-03-rerun/` (single-plan bundle).

## Purpose

Re-run PLAN-03 from `dev/plans/0013-super-app-pressure-tests/` now that both prior blockers are merged on `post-add-expr`:

- **Finding 1** — consumer-discovery walker missed layer-aes `ppVar` nested under layer-data `ppUpload`. Fixed at `6ee63d7` via ADR-0015 eager-bind (walker descends into layer `data = ppX(...)` wrappers; Option-A workaround for the autoname-race noted in `dev/audit/audit-adr15-autoname-race-20260523-204227.html`).
- **Finding 2** — `validate_input` non-TRUE return atomically cleared the code panel + blanked the plot panel, breaking PLAN-03 D9's retain-prior-content assertion. Fixed at `04b2fc2` via `plans-03-06-rerun`: `state$last_ok_runtime` reactiveVal + observer-driven cache; `ptr_register_plot` and both branches of `ptr_register_code` fall back to the cache on transient `validate_input` failure.

The committed PLAN-03 body (Success Criteria, Constraints, BDD, DoD) is **verbatim usable**; only the meta block (depends_on, prior PASS stamp hash) was stale.

## Merge order

Single plan, no in-bundle siblings. Merge directly.

## Parallel groups

- **G0 (standalone)**: 01 — `app-2a-upload-registry-rerun`. Independent of any sibling rerun.

## Plans

| # | Slug | Group | Depends on | Status | One-line summary |
|---|------|-------|------------|--------|------------------|
| 01 | app-2a-upload-registry-rerun | G0 | none in-bundle (PLAN-01 anchors satisfied externally at `6ee63d7`) | draft | `ptr_app()` + mid-pipeline ppUpload (two data sources via `data = ppUpload(df_aux)`) + ADR-0010 positional companion names + custom `ppPower` value-role + custom `ppMultiVar` consumer; one `test_that` in the `super-2a` anchor region covering 8 BDD scenarios (deterministic uploads, ppPower `resolve_expr`, ppMultiVar `interaction()` synthesis, both ppUpload names round-trip, layer-data picker pulls df_aux columns, D9 retain-on-error, shared-scope confinement, B3 preserve↔final toggle differential). Net effect: +1 PASS. |

## Notes for the orchestrator

- Baseline PASS at `plan-03` HEAD `6ee63d7` = **2384** (per entry-point handoff). After this plan: PASS ≥ 2385 with FAIL 0 / ERROR 0 / SKIP 0.
- Browser-stack gate required (`NOT_CRAN=true` via `devtools::test()` or the explicit `Rscript -e` form). The test booths the fixture via `boot_vignette_app()` → `pkgload::load_all()` (app-dir + pkgload pattern per memory `shinytest2-appdir-pkgload`).
- Worktree convention: per memory `feedback-parallel-worktree-workflow`, this bundle runs in the **sibling** worktree `/Users/willju/Research/ggpaintr-plan-03` (branch `plan-03`). Do NOT use `EnterWorktree` — Serena's project root does not follow worktree changes (memory `feedback-serena-project-root-doesnt-follow-worktree`). Sub-agents spawned by `/exec-plan` must use `Edit`/`Write` with absolute paths under their assigned WD only, and MUST avoid `mcp__serena__*` edit tools.

## What this bundle does NOT do

- Does NOT modify the original at `dev/plans/0013-super-app-pressure-tests/03-app-2a-upload-registry.html`. That stamped copy stands as the historical record; this rerun copy gets its own fresh stamp.
- Does NOT re-run PLAN-02, PLAN-04, PLAN-05, or PLAN-06 from the ADR-0013 bundle. Those are separate worktree efforts (PLAN-04 = `assess-fb-patch`; others as scheduled by the parent orchestrator).
- Does NOT touch any `R/paintr-*.R` file. If a product defect surfaces (e.g. layer-data picker regression of ADR-0015), the implementer STOPs and escalates; the fix is a follow-up plan.
- Does NOT push `plan-03` or merge it into `post-add-expr`. The parent orchestrator at `/Users/willju/Research/ggpaintr-post-add-expr` runs the verify-and-merge cycle after independent verification (per memory `feedback-risk-discussion` + `feedback-parallel-worktree-workflow`).

## Provenance

This bundle was authored on 2026-05-23 in the `plan-03` worktree, in response to the entry-point handoff queued by the parent orchestrator. Both blockers that previously aborted the original PLAN-03 attempt (`0013-app-2a-upload-registry` branch, since stale) were merged on `post-add-expr` before `plan-03` was cut:

- Finding 1 fix: merged at `6ee63d7` (followup-b — ADR-0015 eager-bind).
- Finding 2 fix: merged at `04b2fc2` (plans-03-06-rerun — `last_ok_runtime` cache).

The committed plan body was reused verbatim per the entry-point handoff's "Plan body otherwise verbatim" guidance; only the meta block (slug suffix `-rerun`, `depends_on: []`, fresh provenance `note:`) and the branch-convention paragraph were updated.

<!-- implementable: index — manifest, not a contract plan; the stamp below satisfies the pre-commit hook (which checks any dev/plans/*.md). The single contract plan in this bundle is 01-app-2a-upload-registry-rerun.html with its own PASS stamp at hash=897407a4a652. -->
<!-- implementable: PASS date=2026-05-23 gate="N/A (manifest)" hash=f9a362f2bb72 -->
