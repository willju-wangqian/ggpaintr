# API audit — action checklist

Referenced by ADR 0002 (`dev/adr/0002-public-api-tiering-and-trims.md` §3), ADR 0004 (`dev/adr/0004-l3-ui-piece-model.html`), and `dev/plans/2026-05-15-css-unification.html` §10. This file was missing (the references were dangling); recreated 2026-05-16 as the live audit checklist.

## Status of the post-rewrite tiering audit (ADR 0002)

Done and in-tree:

- [x] `_pkgdown.yml` purged of ~12 nonexistent functions; five explicit tiers established.
- [x] `ptr_runtime_input_spec`, `ptr_ns_id` un-exported (internal callers / removed contracts).
- [x] `placeholders=` argument removed from the copy functions; `ns=` removed from `ptr_app`/`ptr_app_bslib`.
- [x] Renames `ptr_merge_ui_text→ptr_ui_text`, `ptr_server_state→ptr_init_state`.
- [x] Headless / non-Shiny path kept intentionally internal (`ptr_translate`, `ptr_run_formula`, `ptr_setup_*`).
- [x] css-unification (`dev/plans/2026-05-15-css-unification.html`) shipped: internal `ptr_assets()` bundle, `?ptr_css` token topic, `ptr_shared_ui(css=)`.

Superseded since ADR 0002:

- css-unification §2 "assets bundle stays internal / public surface only shrinks" is **superseded in part by ADR 0004** for the L3 path: `ptr_ui_assets()` is exported as the hand-composition escape hatch. Not a regression — a recorded decision.

## Pending — execute in the API-audit pass (deferred from the L3-piece-model work, ADR 0004)

Renames (no deprecation shims — single user, pre-1.0):

- [ ] `ptr_shared_ui` → `ptr_ui_shared`
- [ ] `ptr_shared_server` → `ptr_shared_resolve`
- [ ] `ptr_outputs_ui` → `ptr_ui_outputs` (canonical default; **error nested**)
- [ ] `ptr_ui_code_toggle` → `ptr_ui_plot_code` (default `error = FALSE`)
- [ ] Collapse `ptr_controls_ui` / `ptr_ui_controls` duplication into one name (`ptr_ui_controls`)
- [ ] Review final names for `ptr_ui_page` and `ptr_ui_assets`

Locked contracts the audit must honor (do not re-litigate — ADR 0004):

- [ ] `ptr_module_ui` / `ptr_module_server` stay a deliberate sealed-bundle naming exception (not folded into `ptr_ui_*` / `ptr_*`). Document the exception so it does not read as an oversight.
- [ ] `ptr_ui_outputs ≡ ptr_ui_plot_code(error = TRUE)` — intentional discoverability alias, different default DOM; do not "dedupe" it away.
- [ ] Server family ends as: `ptr_server` (generic), `ptr_shared_resolve` (shared), `ptr_module_server` (bundle exception). UI family: `ptr_ui_*`.

Open decisions for the audit:

- [ ] Long-term fate of `ptr_ui_assets()` exposure: keep as escape hatch, or replace with a tighter surface. Currently a recorded supersession of css-unification §2.
- [ ] Whether `ptr_ui_page()` is the final shell name, and whether `page=` should accept a curated enum vs. any `is.function`.

## Hazards recorded for future readers

- `?ptr_css` is a deliberate roxygen-only topic (`R/paintr-css-doc.R`, `@keywords internal`). The `[ptr_css()]` links across the man pages are **valid**. Do not "fix the dangling link" — same trap ADR 0002 records for re-adding removed functions.
- `ptr_register_plot/error/code` are intentionally internal post-rewrite (only `ptr_server` can drive their `state$runtime()`). Do not re-export.
