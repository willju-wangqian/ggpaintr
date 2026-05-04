---
status: proposed
created: 2026-05-03
size: large
depends-on: 05-pipeline-var-resolution-bug
blocks: none
---

# Concern 06 — Cohesion of `shared` placeholders with the data-pipeline UI/UX

## Problem

`shared = "..."` placeholders interact with the data-pipeline subsystem in three ways that are individually surprising and collectively incoherent:

1. **No auto-rendering for shared widgets.** A `shared = "..."` annotation in a formula does not produce a widget anywhere unless the embedder externally supplies a matching reactive in `shared = list(...)`. Today, the per-instance placeholder gets rendered locally inside the per-layer panel, but its value is then ignored at resolve time when an external binding exists, or the formula's `shared` annotation is silently inert when no binding exists. Either way the user sees inert UI in the per-layer panel.
2. **Per-instance "Update Data" friction across N plots.** In `ptr_app_grid()`, a shared input that drives multiple data pipelines marks every plot's stale flag, but each plot's cache is gated behind its own per-layer "Update Data" click. Moving one slider that affects N plots requires N clicks.
3. **Shared metas leak into the data tab UI.** `ptr_build_pipeline_layer_controls` (`R/paintr-ui.R:276`) does not filter `meta$shared`, so the per-layer Data tab renders a local widget for every pipeline placeholder — including shared ones — even though the resolver ignores those local widgets in favor of the external binding (`R/paintr-placeholders.R:485-489`). Aesthetic controls get this right (`ptr_bind_placeholder_ui` filters at `R/paintr-placeholders.R:576`); data tab controls do not.

The shared-placeholder feature was originally motivated by `ptr_app_grid()` (one widget driving multiple plot modules) and the gallery pattern (custom multi-`ptr_server` composition with embedder-supplied reactives). Within a single `ptr_app()` instance, the design did not anticipate shared placeholders inside data pipelines, and the UI/UX has gaps in every flow.

## Status quo (verified)

- `ptr_validate_shared_bindings` (`R/paintr-utils.R:428`) accepts whatever the embedder passes; it does not cross-check that every `shared = "x"` annotation in the formula has a matching `shared_ui` / `shared_bindings` entry. Typos and omissions fail silently.
- `ptr_resolve_placeholder_input` (`R/paintr-placeholders.R:482-496`) consults `context$shared_bindings[[shared_name]]` first; if NULL, falls back to `input[[meta$id]]` (the local widget). So `shared = "..."` becomes inert when no external binding is supplied.
- `ptr_setup_data_pipeline_observers` (`R/paintr-app.R:528-663`) re-evaluates a layer's pipeline only on click of the per-layer `ptr_update_data_<layer>` button. There is no mechanism to refresh multiple plots' caches from one user action.
- `ptr_build_pipeline_layer_controls` (`R/paintr-ui.R:276`) iterates every `info$placeholder_ids` and renders a widget for each, regardless of `meta$shared`. By contrast, `ptr_bind_placeholder_ui` (`R/paintr-placeholders.R:551-590`) does filter shared metas before rendering aesthetic controls.
- `ptr_app_grid()` (`R/paintr-app.R:1266`) renders shared widgets in a top-level `wellPanel` (`R/paintr-app.R:1326`); single-plot flows (`ptr_app()`, `ptr_app_bslib()`, `ptr_module_ui()`) have no shared panel — formulas with `shared = "..."` annotations have nowhere natural to render the widget.
- Reproducers live in `dev/scripts/probe-shared-pipeline.R` (testServer trace of N-click friction) and `dev/scripts/demo-shared-pipeline.R` (interactive grid app demonstrating issues 2 and 3 together).

## Decisions locked during brainstorming

The brainstorm session that produced this plan locked the following design directions:

- **D5. Remove the per-layer "Update Data" button.** Pipeline re-eval is gated by user actions that already exist for other reasons.
- **D6. Pipeline re-eval triggers, snapshot-cache-keyed.** Two trigger points: (i) Draw click; (ii) activation of a Plot Panel tab in the layer picker that contains `var` placeholders bound to a pipeline. Each trigger checks whether pipeline inputs have changed since the last cache; if not, no work. Initial-render tab activation is harmless (cache hit).
- **D7'. Shared placeholders auto-render their widget when no external binding is provided.** Placement:
  - Single-plot ptr instance, no external binding: render in an instance-level **shared section** (see L4').
  - `ptr_app_grid()`: keep the existing top-level `shared_panel`. Untouched.
  - Custom multi-instance composition (gallery pattern): embedder remains responsible for the widget, same as today.
- **D8. Suppress local controls for shared placeholders in BOTH aesthetics and data tabs.** Mirror `ptr_bind_placeholder_ui`'s filter inside `ptr_build_pipeline_layer_controls`. Exact bug from issue 3 above.
- **D9. Shared panel hosts a refresh button**, conditionally rendered when at least one data pipeline references at least one shared placeholder (computable from `data_pipeline_info` × `placeholder_map`).
- **D10. Refresh button semantics: eager re-eval, no redraw.** Click → immediately re-evaluate all pipelines that depend on shared inputs; refresh `resolved_data` reactiveVals and var dropdowns. Plot does not redraw — Draw remains the plot trigger.
- **L4'. Foldable shared section above the layer picker.** For single-plot flows, render an always-on, collapsible section above the picker. Contents: shared widgets + the conditional refresh button. Lives inside the opaque `output[[control_panel]]` slot — consistent with plan 01's contract; no new public surface.
- **M1. Multi-reference deduplication: single widget in first-reference layer.** When a `shared = "x"` name has N references in one formula, render one widget in the first-reference layer (instance-level shared section in single-plot, top-level shared panel in grid). Don't mirror across panels.

## Implications and follow-throughs

- D5 + D6 + D9 together replace today's button-driven model with a trigger-driven model. The snapshot cache mechanism that today drives the stale flag becomes the cache-invalidation key.
- D7' + L4' make a single-plot formula with `shared = "..."` self-sufficient: the widget renders, drives the resolver, and lives in a stable location.
- D8 fixes the inert-widget bug (issue 3).
- D9 + D10 cover the niche where the user changes a shared widget and wants fresh var dropdowns *without* drawing or switching layers. (Niche confirmed real during brainstorm.)
- M1 keeps the rendering rule simple at the cost of cross-layer discoverability for the rare cross-layer-cross-tab case. Upgrade to mirrored-and-synced (M2) is additive — the resolver doesn't change.
- The cross-cutting "data-aware shared placeholder in its own pipeline" case (e.g., `data |> select(var(shared = 'v')) |> ggplot(aes(x = var(shared = 'v')))`) needs the shared `var` widget's choice list to come from the *upstream* (pre-pipeline) source data, not from `resolved_data` — otherwise the user picks once and locks themselves into a single column. Same pattern as the bootstrap-from-upstream resolution discussed for plan 05's bug.

## Open questions

1. **Validation of shared bindings.** When the embedder supplies `shared_bindings` (or `shared_ui`) but a key doesn't match any formula annotation — or vice versa — should the package error, warn (`cli::cli_warn`), or stay silent? Current behavior is silent. Recommendation: error on missing-from-formula (typo), warn on extra-in-bindings (potentially intentional during iteration).
2. **Choice-list source for shared `var` widgets that participate in their own pipeline.** Upstream-source data, previously-cached resolved data, or hybrid? Mirrors plan 05's bootstrap question for pipeline `var` resolution; the answer should be consistent.
3. **Foldable section default state.** Folded or unfolded on first render? Folded reduces vertical space when ignored; unfolded surfaces shared widgets immediately for new users.
4. **Refresh button copy.** "Refresh data-aware controls"? Icon-only with tooltip? `ui_text$shell$shared_refresh$label` override path needed.
5. **Stale-flag coherence after D5.** Today the per-layer stale flag drives a CSS class on the per-layer Update Data button. After button removal, what replaces this affordance? Options: silent (cache invalidates without UI signal), per-layer stale indicator inline in the picker entry, or no indicator at all.
6. **`var_column_map` for the shared `var` widget itself.** In single-plot, the widget renders in the shared section (above the picker), outside any layer's panel. Its choice list needs a context — which layer's column map? Most natural: the union of column maps for all layers that reference the shared name. Single-layer references degenerate to the layer's own map.
7. **Interaction with plan 05's audit.** The audit may surface other context fields that should populate consistently across context-construction sites (pipeline observer, aesthetic substitution, UI bind, refresh-button observer). Cross-reference once 05 lands.

## Out of scope

- The `var`-in-pipeline silent-drop bug — covered by plan 05.
- Restructuring `ptr_resolve_data_pipeline_expr` beyond minimum — covered by plan 05.
- Mirrored-and-synced widget rendering (M2) — defer until a concrete cross-layer use case surfaces.
- Restyling `ptr_app_grid()`'s top-level shared_panel — purely visual.
- New custom-placeholder authoring contract docs — separate concern (likely plan 03 docs-rewrite).

## Success criteria

- A formula with `shared = "..."` annotations renders correctly in `ptr_app()`, `ptr_app_bslib()`, and `ptr_app_grid()` without requiring the embedder to provide bindings (auto-render kicks in).
- A single-plot formula with shared placeholders surfaces the widgets in a foldable section above the picker; widgets are consistent with the placeholder's `build_ui` and overridable.
- Moving a shared widget that drives N data pipelines requires zero per-plot button clicks; one Draw refreshes everything; or one refresh-button click refreshes data-only without redrawing.
- The Data tab no longer renders inert local widgets for shared placeholders.
- Tests cover: single-plot with shared placeholder; grid with shared inside data pipeline; refresh-button eager-eval semantics; M1 single-widget rendering for multi-reference formulas; D8 filtering verified.
- Manual coverage updated in `tests/manual/` (per `.claude/rules/testing.md` — manual coverage required when human-interaction behavior changes).
- Decision recorded for each open question (1–7).

## Recommended next-session approach

Plan 05 must land first — D6's pipeline re-eval triggers depend on `var_column_map` being populated correctly inside the pipeline observer's context, which is plan 05's fix.

Once 05 ships, invoke `brainstorming` with this file. Order: question 5 (stale-flag UI replacement) before implementation, since it shapes the picker visual; then 1, 2, 3, 4, 6, 7 in any order — they're independent.

Reproducers in `dev/scripts/`:
- `probe-shared-pipeline.R` — testServer trace, single module with shared inside pipeline. Demonstrates the wiring works end-to-end when click reaches the module; good as the regression-test scaffold.
- `demo-shared-pipeline.R` — interactive `ptr_app_grid()` app showing the N-click friction and inert-widget issues; good for UX validation after the fix.
- `probe-select-var.R` and `probe-select-var-runtime.R` — drove the discovery of plan 05's bug; relevant cross-reference.
