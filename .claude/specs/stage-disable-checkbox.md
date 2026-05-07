---
name: stage-disable-checkbox
type: decision
status: accepted
scope: [core-rewrite, P4, P7, P12, ui-bridge, resolve]
created: 2026-05-06
related: [p9-relax.md, core-rewrite.md]
---

# Stage-Disable Checkbox — Runtime Safety Valve

## Understanding Summary

- **What:** A per-data-manipulation-call checkbox that lets the end user mute a stage at runtime, stripping it from the data flow before substitute/prune/eval. The checkbox attaches to every call along a layer-or-call `data_arg`'s data-manipulation chain whose subtree contains ≥1 placeholder.
- **Why:** Even after P9 relax handles per-arg empty cases automatically (gate 1), a surviving stage may still error at eval — invalid `expr` placeholder content, malformed user input, runtime arity violations. Without an escape, the user must edit the formula. The checkbox is the gate-2 manual escape.
- **Who:** End users of rendered Shiny apps. Authors of formulas inherit the affordance for free; no new authoring surface.
- **Constraints (locked from `p9-relax.md` forward-compat check):**
  - Disable transform sits *before* substitute in `ptr_resolve_upstream`'s pipeline (subtree → `disable_walk` → substitute → prune → eval).
  - Reuse the existing strip mechanisms — pipeline-stage drop and call-replace-with-data-arg — no new marker class.
  - Stage-stable ids belong in P4 (id-encoding), not P9.
  - Cache key (`deparse(post-substitute-expr)`) naturally invalidates per toggle.
  - Additive on top of P9-static-only. Does not retroactively change P9 semantics.
- **Non-goals:**
  - Per-stage error attribution (deferred — generic error pane handles eval errors).
  - Refining multi-placeholder-per-stage UI placement (deferred as future UI design).
  - Expanding `default_drop_when_empty` membership (separate "walk the tidyverse" pass).
  - Verb-list-gated recursion (rejected; mechanical recursion accepted as user-responsibility design).

## Decision

### Pass placement

**Translate-time:** P1 → P5 → **P4 (now including stage-id assignment via `assign_stage_ids`)** → P2 → P3.

**Resolve-time** (per call to `ptr_resolve_upstream`): **`disable_walk`** → P8 substitute → P9 prune → P11 eval → optional column normalization.

### New module: `R/paintr-disable.R`

Owns:

```
is_data_chain_call(node, in_data_position)  # predicate
walk_data_chain(node, fn)                    # shared chain-walk helper
assign_stage_ids(tree)                       # P4 hook
disable_walk(tree, stage_enabled)            # S3 visitor
```

### `is_data_chain_call` predicate

A `ptr_call` node qualifies for a `stage_id` (and a checkbox) iff all hold:

1. The node is a `ptr_call`.
2. It sits in a "data-arg position":
   - A layer's `data_arg` slot, OR
   - A pipeline stage (any k ≥ 1 — stage k=1 qualifies iff it's a call, not a bare data symbol), OR
   - The data-arg position of an enclosing qualifying `ptr_call` — defined as the arg named `data` or `.data`, OR the first positional arg if no such name is bound.
3. Its subtree contains ≥1 `ptr_ph_*` node.

The recursion is mechanical — no verb-list gate. Users writing semantically nonsensical formulas (e.g., a predicate where data should go) inherit the user-responsibility framing from `p9-relax.md`'s edge-case handling.

### `assign_stage_ids(tree)` (P4 extension)

Recursive walk. At each node where `is_data_chain_call(node)` is TRUE, attach `node$stage_id <- "<layer_name>+<index-path>+stage_enabled"` (same scheme P4 uses for placeholder/derived ids). Then recurse into the data-arg position to potentially assign a `stage_id` to a nested call. Other children (non-data-arg-position) are not recursed into for stage-id purposes.

`R/paintr-ids.R` adds a one-line hook calling `assign_stage_ids()` at the end of its existing id-encoding pipeline. P4 stays the single owner of "what kinds of ids exist."

### `disable_walk(tree, stage_enabled)` S3 visitor

Bottom-up. Methods:

- `disable_walk.ptr_pipeline(node)`: iterate stages. **Stage k≥2 disabled → drop wholesale** (don't recurse into args, don't replace with first positional). **Stage k=1 falls through** to the call walker, which replaces it with its data-arg child. Surviving stages still recurse via `disable_walk(s, stage_enabled)` for any nested disables under their data-arg position.
- `disable_walk.ptr_call(node)`: recurse on args (rebuilding the call); if `node$stage_id` is FALSE in `stage_enabled`, replace this call with the data-arg child (already rewritten by recursion). If no data-arg position exists, return `ptr_missing()`.
- `disable_walk.ptr_layer`, `disable_walk.ptr_root`: recurse into children; rebuild.
- `disable_walk.default(node)`: return as-is.

Pipeline single-/zero-stage collapse is **not** done here — P9 prune retains ownership.

**Why pipeline stage k≥2 drops wholesale instead of replacing with first positional arg.** Pipeline form moves the data into the call implicitly (from the previous stage). The call's first positional arg in stage k≥2 is therefore a *predicate*, not data — e.g., in `mtcars |> filter(num)`, `num` is filter's predicate, not its data source. Replacing `filter(num)` with `num` would produce `mtcars |> num` (a placeholder where a stage should be) and break the pipeline. Dropping the stage wholesale leaves `mtcars`, the previous stage's output — semantically "skip this transformation."

The k=1 case is different: stage 1 IS the data source, so its first positional arg IS data (consistent with non-pipeline single-call form). E.g., `filter(head(mtcars, num1), num2) |> mutate(num3)` with the outer filter disabled → `head(mtcars, num1) |> mutate(num3)`. The same call-replace logic that handles `geom_point(data = filter(mtcars, num))` covers stage 1.

### P7 input-spec extension

When walking the tree, emit one row per call whose `stage_id` is set, with `role = "stage_enabled"`. Columns match the existing input-spec shape (`input_id`, `role`, `layer_name`, `keyword`, `param_key`, `source_id`, `shared`).

### P12 server-state extension

`state$stage_enabled <- reactiveVal(named_list)`. On translate (state$tree update):

```r
reconcile_stage_enabled <- function(old_list, new_tree) {
  new_keys <- collect_stage_ids(new_tree)
  out <- setNames(rep(TRUE, length(new_keys)), new_keys)
  carry <- intersect(names(old_list), new_keys)
  out[carry] <- old_list[carry]
  out
}
```

New ids default TRUE. Surviving ids carry forward. Vanished ids drop. Per-checkbox `observeEvent` writes back into the list (single `reactiveVal` updated atomically).

### `R/paintr-resolve.R` integration

`ptr_resolve_upstream(subtree, ...)`'s first line becomes:

```r
subtree <- disable_walk(subtree, stage_enabled())
```

`disable_walk` runs before substitute → prune → eval. Cache key (deparse of post-substitute expr) changes naturally on toggle.

### P6 build-ui extension

`build_ui_for.ptr_call` (and friends) checks `node$stage_id`; if set, wraps the rendered placeholder UI for that call's children with a tiny unlabeled `shiny::checkboxInput` adjacent to the first placeholder UI element within that stage. Disabled-stage placeholder UI receives the existing `ptr-layer-disabled` CSS class for greying.

### Apply timing

Stage-checkbox toggles are **immediate** for downstream widget refresh: `state$stage_enabled` invalidates `ptr_resolve_upstream` (per-position upstreams) and the layer UI (greying). The plot is **not** re-rendered until the user clicks the Update Plot button (replacing the existing Update Data gate, which is being removed in parallel).

### Coexistence rules

1. **`default_drop_when_empty` is independent.** Stage-checkbox always shows for predicate-matching stages, regardless of list membership. Auto-drop (gate 1) and manual disable (gate 2) cover disjoint failure modes.
2. **Layer-toggle is independent.** When the layer-toggle is OFF, stage-checkbox state is preserved but irrelevant; when ON, stage state reapplied. No grey-out cascade between the two checkboxes (matches existing pattern for `var` placeholders inside disabled layers).
3. **Disabled-stage placeholder UI greys** via the existing `ptr-layer-disabled` CSS class. Values are preserved; the input remains bound but visually dimmed.

## Future UI Work

Deferred to a follow-up UI iteration; not blocking the gate-2 functionality:

- **CSS greying-on-toggle for disabled stages.** The initial implementation renders a static checkbox alongside each placeholder UI but does not toggle the `ptr-layer-disabled` class on the surrounding `ptr-stage-row` div when the user unchecks it. Per Q3's "make the UI simple for now" directive, the visual affordance for a disabled-but-still-mounted placeholder is left as a future UI design pass. Wiring it requires the same JS-level reactivity the layer-toggle uses (e.g., `shinyjs::toggleClass` driven by an `observeEvent` on the stage-checkbox input). Until then, a disabled stage's placeholder UI is functionally muted (the disable transform strips it from the data flow) but visually identical to an enabled one. Users can still see the checkbox state itself.
- **Multi-placeholder-stage layout refinement.** Currently the stage-checkbox attaches to the first placeholder UI within a stage; subsequent placeholders in the same stage render alongside without their own checkbox. If multi-placeholder stages prove confusing, the placement could move to a stage-level header, a fieldset wrapping the stage's placeholders, or another design.
- **Per-stage error attribution.** Q3 deferred this — eval errors from a surviving stage surface in the generic error pane without naming the specific stage. A future iteration could attribute errors to the originating stage (red border on its checkbox row, inline error text), but it requires per-stage eval probing rather than the current "complete the whole expression once" path.

## Alternatives Considered

| # | Alternative | Why rejected |
|---|-------------|--------------|
| 1 | Per-arg checkboxes (one per placeholder-bearing arg of a stage) | Redundant with P9 relax — empty-placeholder arg already auto-drops. Checkbox is gate 2, where gate 1 has already failed. |
| 2 | Always-visible checkbox unchecked by default | Would render formulas in a degenerate state at app startup. |
| 3 | Hidden until stage errors, then reveal | Heavy coupling between eval state and UI tree shape; poor discoverability. |
| 4 | Per-stage error attribution (red border + inline error message on the checkbox row) | Requires per-stage eval probing — non-trivial. Deferred; generic error pane is sufficient. |
| 5 | Lazy disable as extension of P9 prune (instead of a separate pre-substitute pass) | Violates locked constraint — disable must run *before* substitute in `ptr_resolve_upstream`. |
| 6 | Skip P4 extension; derive stage_ids on the fly (Approach B in brainstorm) | Stage-id derivation logic would duplicate across disable visitor, build-ui, and input-spec. Loses P4's "single source of truth" property. |
| 7 | Dedicated "chain annotation" pass (Approach C in brainstorm) | YAGNI — no current feature beyond disable needs explicit chain annotation. |
| 8 | Verb-list-gated recursion (only recurse into known data-manipulation verbs) | Adds list maintenance for marginal safety. User-responsibility framing handles malformed formulas consistently with edge case (a). |
| 9 | Toggle gates behind Update Data (consistent with placeholder values) | Update Data is being removed in parallel. The new model: live dropdowns + Update Plot for plot eval. Toggle is a structural change, naturally lives on the live side. |
| 10 | Separate marker class `ptr_disabled` for disabled stages | Reuse of existing pipeline-stage drop and call-replace-with-data-arg mechanisms is sufficient. No new class needed. |

## Acceptance Criteria

### Code

- [ ] `R/paintr-disable.R` created with `is_data_chain_call`, `walk_data_chain`, `assign_stage_ids`, `disable_walk` (S3 generic + methods for `ptr_pipeline`, `ptr_call`, `ptr_layer`, `ptr_root`, `default`).
- [ ] `R/paintr-ids.R` calls `assign_stage_ids(tree)` at the end of its id-encoding pipeline.
- [ ] `R/paintr-input-spec.R` emits one row per `stage_id` with `role = "stage_enabled"`.
- [ ] `R/paintr-resolve.R` `ptr_resolve_upstream` runs `disable_walk(subtree, stage_enabled())` before substitute.
- [ ] `R/paintr-server.R` `ptr_server_state_v2` adds `state$stage_enabled <- reactiveVal(named_list)`; translate observer reconciles via `reconcile_stage_enabled`; per-checkbox observer wires inputs.
- [ ] `R/paintr-build-ui.R` `build_ui_for.ptr_call` (and dispatch siblings as needed) renders the unlabeled `checkboxInput` when `node$stage_id` is set; greys placeholder UI via `ptr-layer-disabled` when disabled.

### Tests (`tests/testthat/test-rewrite-disable.R`)

- [ ] `is_data_chain_call` predicate: layer `data_arg`, pipeline stages (k=1 call vs k=1 bare), nested data-arg position, predicates (not eligible), shared key with no placeholder (not eligible).
- [ ] `assign_stage_ids` annotates eligible calls deterministically; non-eligible nodes lack the field; re-translate produces identical ids for unchanged structure.
- [ ] `disable_walk` unit tests: single-stage drop, nested-call replace-with-data-arg, compose outer + inner, identity on empty `stage_enabled`.
- [ ] `ptr_resolve_upstream` integration: `mtcars |> filter(num) |> select(mpg, var)` with disabled `filter` → `var`'s upstream becomes `mtcars |> select(mpg, var)` → choices update.
- [ ] Server reconciliation: toggle updates `state$stage_enabled`; re-translate carries forward unchanged ids and resets new ones; plot does not re-render until Update Plot.
- [ ] UI: `build_ui_for.ptr_call` emits checkbox with correct `inputId`; disabled placeholder UI carries `ptr-layer-disabled` class; layer-toggle and stage-checkbox coexist independently.

### BDD (`core-rewrite-bdd.md`)

- [x] New §G11 "Stage-Disable Checkbox — Gate-2 Runtime Safety Valve" capturing: predicate scope, recursion rule, disable-strip semantics, immediate-vs-Update-Plot timing, coexistence with `default_drop_when_empty` and layer-toggle, edge cases (empty pipeline, all stages disabled, leftmost-stage strip, single-call data_arg).
- [x] Cross-link to `p9-relax.md` "Forward Compatibility" section (gate 1 vs gate 2 framing).

### Verification

- [ ] `devtools::document()` runs clean.
- [ ] `devtools::test()` passes (target: 0 fail, 1 skip — Phase 4a baseline).
- [ ] `devtools::check(--as-cran)` returns 0 errors, 1 pre-existing WARNING, 1 NOTE.

### Memory

- [ ] `current-status.md` updated with stage-disable feature landing.
- [ ] `core-rewrite-progress.md` updated to note the new module.

## Decision Log

| # | Decision | Alternatives | Why |
|---|----------|--------------|-----|
| 1 | Scope: stages along the data-manipulation chain whose subtree contains a placeholder; recursion through nested calls in data-arg position | Every verb / no recursion / per-arg | Matches motivating "manual escape for verbs that error at eval"; recursion makes pipeline form and nested non-pipeline form behave equivalently |
| 2 | Granularity: per-call (per-stage), not per-arg | Per-arg / both | P9 relax handles per-arg empty cases automatically; checkbox is gate 2 manual escape for surviving-stage eval errors |
| 3 | Always-visible, default checked, tiny unlabeled checkbox attached to placeholder UI | Hidden-until-error / labeled / per-arg layout | Predictable UX; zero hidden state; defers richer UX to future iteration if needed |
| 4 | Stage state follows existing P4 invariant (structural edits reset, value edits don't) | Content-hashed ids / position-flat numbering | Consistent mental model with placeholder ids; same code path as existing P4 |
| 5 | Independent of `default_drop_when_empty` and layer-toggle; disabled stages grey via existing CSS pattern | Hide if in drop-list / cascade-disable from layer | Disjoint failure modes; matches existing pattern for placeholders inside disabled layers |
| 6 | Apply immediately for dropdowns; plot waits for Update Plot | Gate behind Update Data | Update Data being removed; new model is live dropdowns + Update Plot for plot eval |
| 7 | Mechanical recursion through data-arg position; no verb-list gate | Verb-list-gated recursion | Avoids list maintenance; user-responsibility framing for malformed formulas matches existing edge-case philosophy |
| 8 | Implement stage-id assignment + disable visitor as one unit in `R/paintr-disable.R`; P4's `paintr-ids.R` adds a one-line hook | Two separate modules / inline in P4 | Tight coupling (visitor consumes annotation, share recursion rule); single concept ownership |

## Cross-references

- `.claude/specs/core-rewrite.md` — parent decision (typed AST + 12 visitor passes); §P4 id-encoding extended by this spec.
- `.claude/specs/p9-relax.md` — gate-1 sibling (positional-missing arg-drop); "Forward Compatibility" section validated this design's locked constraints.
- `.claude/specs/core-rewrite-bdd.md` — new §G11 (or §P13) to be added per Acceptance Criteria.
- Memory `core-rewrite-progress.md` — Phase 4a context (P12 server state, ptr_resolve_upstream, build_ui dispatch); this feature extends each.
