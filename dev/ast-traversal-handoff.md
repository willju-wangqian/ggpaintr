# AST Traversal Handoff

## Purpose

ggpaintr now has a typed AST rewrite core, but traversal logic is still duplicated across passes. This handoff proposes a small traversal kernel that keeps the current domain AST while making future scanning, substitution, pruning, UI discovery, and runtime transforms easier to extend.

## Current State

- The rewrite has the right high-level shape: `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, placeholder nodes, `ptr_literal`, `ptr_missing`, and `ptr_user_expr`.
- The main passes are now split across modules: translate, classify, ids, substitute, prune, render, eval, resolve, disable, input spec, and UI build.
- The rewrite-specific test suite passes, which means the new architecture is coherent for the rewritten internals.
- Full package tests still include old internals and fail on removed or changed functions such as `ptr_layer_checkbox_tag`, `generate_ui_var`, `ptr_get_layer_switcher_ui`, and old `ptr_complete_expr_safe()` arguments.
- The remaining design issue is not whether ggpaintr has an AST. It does. The issue is that each pass still hand-codes how to walk the same AST shape.

## Problem

Each pass independently knows that:

- `ptr_root` descends into `layers`.
- `ptr_layer` descends into `data_arg` and `children`.
- `ptr_call` descends into `args`.
- `ptr_pipeline` descends into `stages`.
- `upstream` should usually not be recursively walked, because it is a semantic pointer and may duplicate work or create cycles.

This works today, but it makes the system less generic than it appears. Adding a new structural node or changing a child slot requires edits across many visitors. Future work such as richer placeholder roles, better diagnostics, source mapping, optional comments, or pass instrumentation will repeat traversal mechanics again.

## Recommendation

Keep the ggpaintr typed AST. Do not replace it with a generic external R AST library. Add a small internal traversal protocol and gradually port passes onto it.

The goal is not to make ggpaintr a universal R code transformation framework. The goal is to make ggpaintr's domain-specific AST easy to scan and rewrite without duplicating recursion logic.

## Proposed Additions

### 1. Child Protocol

Add two internal generics:

```r
ptr_children <- function(node, include_upstream = FALSE) UseMethod("ptr_children")
ptr_replace_children <- function(node, children) UseMethod("ptr_replace_children")
```

`ptr_children()` returns a named list of child nodes plus metadata sufficient to put them back. `ptr_replace_children()` rebuilds the same node with rewritten children.

Expected behavior:

- `ptr_root`: child group `layers`.
- `ptr_layer`: child groups `data_arg` and `children`.
- `ptr_call`: child group `args`, preserving argument names.
- `ptr_pipeline`: child group `stages`.
- Placeholder, literal, missing, and user-expr nodes: no structural children by default.
- `upstream`: skipped by default; included only when `include_upstream = TRUE`.

How this helps:

- One place defines AST shape.
- New node classes require child methods, not edits across every visitor.
- Tests can verify traversal shape directly.
- Passes can opt into or out of walking semantic pointers like `upstream`.

### 2. Generic Rewrite Walker

Add an internal rewrite primitive:

```r
ptr_rewrite <- function(node, ctx = list(), pre = NULL, post = NULL,
                        descend = NULL, include_upstream = FALSE) {
  # pre(node, ctx, cursor) can rewrite node or update ctx before children.
  # descend(node, child, ctx, cursor) decides whether each child is visited.
  # post(node, ctx, cursor) can rewrite/drop node after children.
}
```

The exact signature can change, but the semantic split matters:

- `pre`: top-down context setup.
- child traversal: shared recursion.
- `post`: bottom-up replacement, pruning, or validation.
- `descend`: pass-specific traversal control.

How this helps:

- Substitute, prune, disable, find, collect, and validate passes can share traversal.
- Pass code becomes semantic: "what happens at this node" rather than "how do I recurse through every node class".
- The traversal kernel can be tested once.
- Instrumentation becomes easier: depth limits, cursor traces, node counts, timing, and debugging hooks.

### 3. Cursor Object

Every traversal callback should receive a cursor object:

```r
list(
  path = integer(),
  slot = "args",
  arg_name = "data",
  layer_name = "geom_point",
  pipeline_index = 2L,
  in_data_position = TRUE
)
```

Fields can be sparse; only populate what is known at that location.

How this helps:

- Id assignment can use cursor path instead of rebuilding paths manually.
- Stage-disable can use `in_data_position` instead of custom data-chain recursion.
- Data classification can distinguish layer data, inherited data, pipeline stages, and ordinary arguments.
- Error messages can report where a bad placeholder or unsafe expression lives.
- Future UI grouping can ask location-aware questions without re-parsing function shape.

### 4. Query Helpers

Build common queries on top of `ptr_rewrite()`:

```r
ptr_find <- function(tree, predicate, include_upstream = FALSE) ...
ptr_collect_placeholders <- function(tree, include_shared = TRUE) ...
ptr_contains_placeholder <- function(node) ...
ptr_collect_stage_ids <- function(tree) ...
```

How this helps:

- Replaces ad hoc collectors in input spec, UI build, ids, and disable.
- Makes tests cleaner and more declarative.
- Creates one standard way to search the tree.

### 5. Tree Validation

Add focused validators:

```r
ptr_validate_tree <- function(tree) ...
ptr_validate_no_placeholders_before_eval <- function(tree) ...
ptr_validate_no_missing_before_eval <- function(tree) ...
ptr_validate_no_cycles <- function(tree, include_upstream = FALSE) ...
```

How this helps:

- Converts confusing Shiny runtime errors into local AST errors.
- Guards pass-order mistakes.
- Makes future agent edits safer.
- Clarifies which passes are allowed to emit `ptr_missing`, `ptr_user_expr`, unresolved placeholders, or `upstream` pointers.

## How Existing Passes Could Use It

### Translate

Probably stays custom. Translation is where raw R language objects become ggpaintr nodes, so it naturally owns special cases: top-level `+`, pipes, placeholders, layer calls, and call-form `shared`.

### Classify Data

Would benefit from a top-down context walker. `pre` can update data context when entering a layer or pipeline stage. Placeholder consumer handling can attach `upstream = ctx$data`.

### Assign Ids

Would benefit heavily from cursor paths. Placeholder ids and stage ids are location-derived, so this pass should not need separate recursion.

### Substitute

Good fit for bottom-up rewrite. Placeholder nodes become literals, missing nodes, or user-expression wrappers. Calls, layers, and pipelines should only need default child traversal.

### Prune

Good fit for bottom-up rewrite. Most pass-specific logic lives in `post`: operator escalation, missing-arg dropping, drop-when-empty, pipeline collapse, layer drop.

### Disable

Could become a pre-substitute structural rewrite over data-position calls. The hard part is computing `in_data_position`; the cursor protocol should make this explicit rather than re-derived.

### Render And Eval

Could stay as dedicated visitors for clarity. They are conversions, not tree rewrites. They may still use `ptr_children()` for validation or generic fallbacks.

### UI And Input Spec

Use query helpers instead of bespoke collectors. This reduces drift between "which placeholders exist", "which widgets render", and "which inputs are expected".

## External Tools

Existing R tools are useful but not replacements:

- `rlang`: keep using it for parsing, symbols, calls, and executable language objects.
- `lobstr::ast()`: useful for visual debugging, not runtime transformation.
- `lintr`, `xmlparsedata`, `treesitter.r`: useful for source locations, concrete syntax, comments, and lint-like analysis.
- `astgrepr` and related source rewrite tools: useful for refactoring R source files, not for ggpaintr's runtime placeholder/data semantics.

ggpaintr's hard problem is not generic R AST traversal. It is semantic traversal with ggplot layer inheritance, pipe upstreams, placeholder roles, Shiny ids, shared bindings, data-source nodes, and pruning policy. That should remain in ggpaintr.

## Non-Goals

- Do not replace the typed AST rewrite with an external AST library.
- Do not rewrite every pass at once.
- Do not create or modify `.claude/specs/` from this handoff alone.
- Do not preserve comments or concrete source formatting in this design.
- Do not make this a public API unless there is a separate decision.

## Suggested Implementation Plan

1. Add `R/paintr-walk.R` with `ptr_children()`, `ptr_replace_children()`, `ptr_rewrite()`, and query helpers.
2. Add tests in `tests/testthat/test-rewrite-walk.R`.
3. Port low-risk collectors first: `collect_layer_placeholders()`, `collect_stage_ids()`, `walk_has_placeholder()`.
4. Port `ptr_assign_ids()` to use cursor paths.
5. Port `ptr_substitute()` after the traversal API is stable.
6. Port `ptr_prune()` only after substitute is stable, because prune has the most subtle drop semantics.
7. Leave `ptr_translate()`, `ptr_render()`, and `ptr_eval()` mostly as-is unless duplication becomes painful.

## Acceptance Criteria

- Traversal shape for every current node class is tested.
- `upstream` is skipped by default and can be explicitly included.
- `ptr_find()` can find all placeholders in the same order currently expected by input spec and UI tests.
- Porting collectors does not change `ptr_runtime_input_spec()` output.
- Porting ids does not change placeholder ids or stage ids.
- Porting substitute does not change rewrite-substitute test results.
- Porting prune does not change rewrite-prune test results.
- Rewrite tests remain green after each migration step.

## Risks

- A too-clever walker could hide important pass-specific behavior. Keep callbacks explicit.
- Cursor semantics can become overloaded. Start with only fields needed by ids and data-position traversal.
- Walking `upstream` accidentally can duplicate work or recurse forever. Default to skipping it.
- A big-bang migration would be risky. Port collectors first.

## Claude Prompt

Read `dev/ast-traversal-handoff.md`, `.claude/specs/core-rewrite.md`, `.claude/specs/p9-relax.md`, and `.claude/specs/stage-disable-checkbox.md`.

Task: evaluate adding a small internal traversal kernel for ggpaintr's typed AST. Do not replace the AST and do not rewrite all passes at once. First implement only `R/paintr-walk.R` plus focused tests, then port the lowest-risk collectors if the API is clean.

Constraints:

- Preserve all current ids, stage ids, input spec row order, render output, substitute behavior, prune behavior, and resolve behavior.
- `upstream` must be skipped by default.
- Keep traversal helpers internal.
- Prefer small mechanical migrations over broad refactors.
- Run `devtools::test(filter = "rewrite")` after changes.

Suggested first patch:

- Add `ptr_children()`.
- Add `ptr_replace_children()`.
- Add `ptr_find()`.
- Add `ptr_contains_placeholder()`.
- Add tests proving traversal shape and upstream skip behavior.
- Do not port substitute or prune in the first patch.

## Decision Log

- Keep ggpaintr's typed AST instead of adopting an external AST framework: ggpaintr needs domain semantics, not just R syntax traversal.
- Add a traversal protocol before rewriting passes: lowers migration risk and gives tests a stable target.
- Skip `upstream` by default: prevents accidental cycles and duplicate traversal.
- Port collectors before mutation passes: lower risk and validates API shape.
- Defer formal `.claude/specs/` decision: this handoff is design input, not yet an accepted architectural decision.
