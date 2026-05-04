---
status: proposed
created: 2026-05-02
size: large
depends-on: 01-public-ui-api, 02-data-aware-placeholder-helper, 04-small-fixes
blocks: none
---

# Concern 03 — README and vignette rewrite

## Problem

User-facing documentation has fallen behind the feature surface. New capabilities are either undocumented or only mentioned tangentially:

- Data-pipeline placeholders (`data = mtcars |> dplyr::filter(num > 0)`)
- "Update data" button + the per-layer Data tab
- Picker-driven layer switcher (replacing the old stacked tabsets)
- Stale-input notice ("Unsaved data inputs" warning)
- Re-exported `ptr_resolve_layer_data()` and the L3 data-aware placeholder pattern
- Namespaced verbs in pipelines (`mtcars |> dplyr::filter(...)`)

Beyond freshness, the *organization* of the docs no longer fits what ggpaintr does. README presents a single quick-start and a placeholder table; vignettes are split by audience (workflow, extensibility, placeholder-registry, gallery, llm) but each was structured before data-pipeline placeholders existed. New users have no clear path from "this is what ggpaintr is" → "here is what I can do" → "here is how to extend it."

## Status quo (verified)

- `README.Rmd` (248 lines): quick-start, placeholder table, extensibility paragraph. No mention of pipelines, layer picker, or "Update data" flow.
- `vignettes/ggpaintr-workflow.Rmd`: covers placeholders + uploads, but nothing on data-pipeline placeholders.
- `vignettes/ggpaintr-extensibility.Rmd`: extensibility levels (L1/L2/L3) — predates data-aware helpers.
- `vignettes/ggpaintr-placeholder-registry.Rmd`: covers `ptr_resolve_layer_data()` and includes data-aware examples — partially current but boilerplate-heavy (will simplify after concern 02 ships).
- `vignettes/ggpaintr-gallery.Rmd`: examples — needs at least one data-pipeline example.
- `vignettes/ggpaintr-llm.Rmd`: ellmer integration — likely fine but verify.
- `dev/developer-notes.md` is human-maintained per CLAUDE.md and is out of scope.

## Open questions for the fresh session

1. Restructure scope: README rewrite only, vignette refresh only, or both?
2. Do we add a new vignette specifically for **data-pipeline placeholders**, or fold it into `ggpaintr-workflow.Rmd`?
3. README organization: keep current "intro / quick-start / placeholders / customization" flow, or pivot to a feature-tour structure ("here's what you get out of the box → here's the data flow → here's how to extend")?
4. Do we add a *concept* page (architecture / mental-model) at the front, or stay quick-start-first?
5. How do screenshots / animated GIFs factor in? (Some new features — picker, Update data button — are visual and hard to convey in prose.)
6. Vignette-level extensibility examples: keep the current 3-level model or restructure once concern 02 lands?

## Out of scope

- API or behavior changes — those happen in the prior concerns.
- pkgdown layout changes (already addressed: `_pkgdown.yml` updated to include `ptr_app_grid` and the gallery vignette).
- LLM-integration docs unless they reference outdated APIs.

## Success criteria

- README walks a new user from "what is this" → "first running app" → "what features matter" in under 5 minutes of reading.
- Every public-API change from concerns 01, 02, 04, 05 is reflected.
- At least one worked example of a data-pipeline placeholder formula appears in README + workflow vignette + gallery.
- Stale-input flow is documented (one paragraph).
- Existing extensibility examples are updated to use any new helper from concern 02.

## Recommended next-session approach

**Run this concern last.** The doc shape depends on what concerns 01, 02, 04, 05 settle. Invoke `brainstorming` with this file plus a summary of accepted decisions from the prior concerns. Resolve question 1 (scope) first, then 2 (data-pipeline vignette home), then draft outlines before prose.
