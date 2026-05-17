# API audit → L2/L3 redesign — index

**Rewritten 2026-05-16, then split into per-step plan pieces.** The audit became a structural L2/L3 redesign. This file is now the **index**; the path is kept because ADR 0002/0004/0005 and `css-unification` reference it.

- **Decision (authoritative):** `dev/adr/0005-l2-l3-redesign.html` — supersedes ADR 0004 in full.
- **Domain language (authoritative):** `CONTEXT.md` — all sections marked *LOCKED 2026-05-16*.
- **Implementation:** the step pieces below, each self-contained with Success Criteria / Constraints / BDD.

Implementation is **not done** — the code still reflects the pre-redesign API. No deprecation shims (single user, pre-1.0).

## Step pieces (dependency-ordered)

| Step | File | Pass | Depends on |
|---|---|---|---|
| 01 | `l2-l3-redesign-01-coordinator.html` | #S — `ptr_shared()` + net-new partition; split `ptr_shared_ui` | — |
| 02 | `l2-l3-redesign-02-server-consistency.html` | #P2 — UI↔server partition invariant; `ptr_shared_server(obj)` | 01 |
| 03 | `l2-l3-redesign-03-orthogonal-outputs.html` | #O — bare outputs + combinators; drop `ptr_ui_plot` flags; del `ptr_ui_code_toggle` | — (∥ 01) |
| 04 | `l2-l3-redesign-04-fold-shared-section.html` | #C — fold shared section into `ptr_ui_controls(shared=obj)` | 01 |
| 05 | `l2-l3-redesign-05-module-rebuild.html` | #M — rebuild `ptr_module_ui`; remove `ptr_controls_ui`/`ptr_outputs_ui` | 03, 04 |
| 06 | `l2-l3-redesign-06-app-grid-coordinator.html` | #G — route `ptr_app_grid` through the coordinator | 01, 02 |
| 07 | `l2-l3-redesign-07-vignette.html` | #DOC — rewrite `ggpaintr-use-cases.Rmd` | 01–06 |
| 08 | `l2-l3-redesign-08-llm-topics.html` | #LLM — rewrite `inst/llm/topics/*` | 01–06 |
| 09 | `l2-l3-redesign-09-memory-claude.html` | #MEM — strike "demoted to L2" clause; memory/CLAUDE.md sweep | 01–06 |
| 10 | `l2-l3-redesign-10-tests.html` | #TEST — suite sweep + new regressions + pkgdown retier (final gate) | 01–09 |

Steps 01–02 are highest-risk (net-new cross-formula partition + the UI↔server consistency invariant). 03 can run parallel to 01–02. 07–09 are large, separate, post-code passes. 10 is the gate.

## Cross-cutting: per-symbol action table (live `NAMESPACE`)

Spans all steps; the authoritative add/keep/remove diff.

| Current export | Action | Target / owning step |
|---|---|---|
| `ptr_app`, `ptr_app_bslib` | KEEP* | unchanged externally; `ptr_app_bslib` rewired (Step 05) |
| `ptr_app_grid` | KEEP* | internals → coordinator (Step 06) |
| `ptr_module_ui` | KEEP* | rebuilt on L3 (Step 05); external DOM/ids preserved |
| `ptr_module_server` | KEEP* | respects partition (Step 02); bundle contract unchanged |
| `ptr_server`, `ptr_init_state` | KEEP | — |
| `ptr_ui_controls` | KEEP* | gains `shared=obj`, folds section (Step 04) |
| `ptr_ui_plot` | KEEP* | drop `error=`/`code_toggle=` flags (Step 03) |
| `ptr_ui_error`, `ptr_ui_code`, `ptr_ui_header`, `ptr_ui_page`, `ptr_ui_assets` | KEEP | — |
| `ptr_ui_code_toggle` | REMOVE | combinators replace (Step 03) |
| `ptr_controls_ui`, `ptr_outputs_ui` | REMOVE | split gone (Step 05) |
| `ptr_shared_ui` | SPLIT | → `ptr_shared` + `ptr_shared_panel` + `ptr_ui_shared_panel` (Step 01) |
| `ptr_shared_server` | KEEP* | name kept; `formulas → obj` (Steps 01/02) |
| `ptr_ui_text`, `ptr_resolve_ui_text`, `build_ui_for` | KEEP | out of scope |
| `ptr_extract_*`, `ptr_gg_extra` | KEEP | API unchanged; docs reclassify use as L3 (Step 07) |
| `ptr_define_placeholder_*`, `ptr_clear_placeholder`, `ptr_normalize_column_names`, `ptr_options` | KEEP | unaffected |
| `ptr_llm_*` | KEEP* | content rewrite (Step 08) |
| **NEW** | ADD | `ptr_shared`, `ptr_shared_panel`, `ptr_ui_shared_panel`, `ptr_ui_inline_error`, `ptr_ui_toggle_code` |

**§INTERNAL (never public, never an advertised escape hatch):** `state$server_ns_fn`/`ui_ns_fn` (server-side only), `ptr_controls_panel`, `collect_shared_placeholders`, the new partition fn.

## Global verification gate (Step 10 owns it)

`devtools::document()` → NAMESPACE matches the table · `devtools::check(--as-cran --no-manual)` 0/0 · `devtools::test()` green incl. partition + lockstep + combinator-snapshot · `_pkgdown.yml` retiered L1/L2/L3 + `build_pkgdown_clean()` clean · the `f1=A+A+B / f2=C+C+B` scenario verified in a running app.

## Out of scope (recorded, not re-litigated)

`build_ui_for` non-`ptr_` name; `?ptr_css` roxygen-only topic + `[ptr_css()]` links (valid — never "fix the dangling link"); `ptr_resolve_ui_text` naming; the copy subsystem.
