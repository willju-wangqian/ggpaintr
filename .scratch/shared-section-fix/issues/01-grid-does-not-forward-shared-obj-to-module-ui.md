# `ptr_app_grid()` does not forward `shared = obj` to its `ptr_module_ui()` cells → panel keys double-render inline

Status: fixed

## Resolution (2026-05-17)

`ptr_app_grid_components()` (`R/paintr-app.R`) now calls `ptr_module_ui(plot_module_ids[[i]], plots[[i]], expr_check = expr_check, shared = obj)` — `obj` is `NULL` when no formula declares `shared = "..."` (ptr_module_ui's single-instance render-all default, behaviour unchanged), and the coordinator spec otherwise so panel keys are excluded from each cell's inline section. Locked by two new assertions in the `grid-shared-partition` e2e test (`tests/testthat/test-e2e-vignette-examples-shinytest2.R`): `expect_no_dom_id(app, "plot_1-shared_sz")` / `plot_2-shared_sz`. Authoritative gate green after the fix: `FAIL 0 / WARN 0 / SKIP 0 / PASS 1639` (the two new assertions account for 1637 → 1639 and would fail without the source change).

## Origin

Surfaced during **W1** of `dev/plans/2026-05-17-shared-section-fix.html` (formula-local shared-consumer binder). Not absorbed into W1 (out of W1's scope: W1 owns `ptr_make_app_server`, `ptr_module_server`, `ptr_shared_server`, the binder helper — not `ptr_app_grid_components`).

## Problem

`ptr_app_grid_components()` (`R/paintr-app.R`) builds its per-plot cells with:

```r
ptr_module_ui(plot_module_ids[[i]], plots[[i]], expr_check = expr_check)
```

It never passes `shared = obj`, even though it has constructed `obj <- ptr_shared(...)` immediately above. W2 (#B2, merged 21c1778) added the `shared=` parameter to `ptr_module_ui()` precisely so panel (cross-formula) keys can be excluded from the inline section, but the grid entrypoint was not wired to use it. Consequently, in a grid where a key is a **panel** key (e.g. `num(shared = "sz")` used by ≥2 formulas), `sz` renders **twice**: once in the standalone `ptr_shared_panel()` and once inline in every cell — the exact B2 double-render the embed path now avoids.

## Evidence

- `grid-shared-partition` e2e fixture (added in W1) deliberately does **not** assert `expect_no_dom_id(app, "plot_1-shared_sz")` — unlike the `l2-shared-partition` embed test, which does — because the grid still emits the inline `sz`. The W1 grid scenario in the plan only requires the formula-local axis pickers to populate and the panel to hold `sz`; it does not require inline-`sz` absence, so W1's DoD is not blocked.

## Proposed fix (out of scope for W1)

In `ptr_app_grid_components()`, pass `shared = obj` to the per-cell `ptr_module_ui()` call when `obj` is non-NULL:

```r
ptr_module_ui(plot_module_ids[[i]], plots[[i]], expr_check = expr_check,
              shared = obj)
```

Then extend the `grid-shared-partition` test with `expect_no_dom_id(app, "plot_1-shared_sz")` / `plot_2-shared_sz` to lock the exclude. This mirrors the `l2-shared-partition` embed fixture (which passes `shared = obj` to both `ptr_module_ui()` calls).

## Notes

Low risk, mechanical, single call site. Belongs to the #B2 family. Track separately so the W1 merge stays revertable as exactly the formula-local-binder change.
