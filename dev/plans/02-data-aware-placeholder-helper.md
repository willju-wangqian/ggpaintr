---
status: superseded
created: 2026-05-02
superseded: 2026-05-06
superseded-by: ../../.claude/specs/core-rewrite.md
size: large
depends-on: 01-public-ui-api (weakly)
blocks: 03-docs-rewrite
---

> **Superseded by [core-rewrite](../../.claude/specs/core-rewrite.md).** The data-aware helper this plan proposes is delivered structurally by the rewrite's `ptr_define_placeholder_consumer` constructor (registry section in core-rewrite, BDD scenarios in `core-rewrite-bdd.md` under "P6 — ui-build" and "Registry Contracts"). The rewrite makes data-awareness a node-class distinction (`ptr_ph_data_consumer` vs `ptr_ph_data_source`) so consumers receive a resolved `cols` vector from the core; no per-author marker plumbing or `bind_ui` hook is needed. The cleaner authoring surface this plan was reaching for is the registry's reduced hook list (≤5 hooks for consumers).

# Concern 02 — Helper / wrapper for data-aware custom placeholders

## Problem

Defining a custom placeholder that needs the active dataset (e.g. column choices, value ranges) is currently boilerplate-heavy. The L3 author has to:

1. Call `ptr_resolve_layer_data(ptr_obj, layer_name, input, context, eval_env)` inside `bind_ui()`.
2. Branch on `has_data` and `is.null(data)`, fall back to a non-data widget.
3. Namespace IDs via `ptr_ns_id()`.
4. Manually wire a Shiny observer that re-renders the widget when uploads / data-pipeline placeholders change.
5. Update widget choices / values without losing user selection across rebuilds.

The placeholder-registry vignette has ~5 worked examples but each repeats the same scaffolding. The built-in `var` placeholder embodies the canonical version of this dance and reaches into private helpers users do not have.

## Status quo (verified)

- `ptr_resolve_layer_data()` is now exported (`R/paintr-placeholders.R:~1125`, restored in current branch).
- `ptr_define_placeholder()` (`R/paintr-placeholders.R`) is the only registration entry point — it does not distinguish data-aware vs. data-independent placeholders.
- `ptr_ns_id()` is exported.
- The built-in `var` placeholder uses internal helpers (column caching via `context$var_column_map`, selection preservation across rebuilds — see `ad1f3d4` "preserve var-dropdown selection across data-cache rebuilds") that are not part of the public surface.
- Vignette `vignettes/ggpaintr-placeholder-registry.Rmd` covers the pattern in prose + examples but no helper exists.

## Open questions for the fresh session

1. What is the right API shape for a data-aware helper?
   - **Option A:** A new `ptr_define_data_aware_placeholder()` wrapper that takes a `columns_to_ui = function(columns, meta, context) {...}` builder and handles resolution + reactivity.
   - **Option B:** Extend `ptr_define_placeholder()` with optional `data_aware = TRUE` plus a richer callback signature (`bind_ui_with_data(input, metas, eval_env, context, layer_data)`).
   - **Option C:** Provide composable helpers (`ptr_data_reactive(meta, context) -> reactive(data)`, `ptr_columns_reactive(meta, context) -> reactive(columns)`) that custom placeholders compose themselves.
2. Does selection-preservation across data-cache rebuilds need to be a public guarantee (and if so, what's the contract)?
3. Should the helper handle `has_data = FALSE` automatically (e.g. render a placeholder "waiting for data" widget), or push that responsibility to the user?
4. Does the helper need to support the data-pipeline case where `data = mtcars |> filter(num > 0)` and the column list changes when `num` is edited?
5. Should built-in `var` be re-implemented on top of the new helper as proof-of-API and reference example?

## Out of scope

- Re-architecting `ptr_resolve_layer_data()` itself.
- Inventing new placeholder *types* (this is a packaging/ergonomics task, not a feature-add).
- The doc rewrite (covered by `03-docs-rewrite.md`).

## Success criteria

- A custom data-aware placeholder definition is materially shorter than today's vignette example (target: <30 lines including imports for a "filter to one column's distinct values" placeholder).
- Built-in `var` either uses the new helper, or there is a written reason it cannot.
- Test coverage in `tests/testthat/test-custom-placeholder-data-aware.R` exercises the helper.

## Recommended next-session approach

Invoke `brainstorming` with this file's contents. Resolve question 1 (API shape) first — the rest are downstream of that choice. If concern 01 has shipped first, fold in any new public surface it exposed.
