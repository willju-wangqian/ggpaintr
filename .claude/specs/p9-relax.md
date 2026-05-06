---
name: p9-relax
type: decision
status: accepted
scope: [core-rewrite, prune, P9]
created: 2026-05-06
---

# P9 Relax — Positional Missing Drops Arg, Not Whole Call

## Understanding Summary

- **What:** Change P9 prune so a positional `ptr_missing` argument drops from the call instead of escalating the entire call to `ptr_missing()`. Reframe `default_safe_to_remove` as `default_drop_when_empty` with a unified semantic: "drop the call (anywhere in the AST) when it goes 0-arg AND the verb is in this list."
- **Why:** Strict P9 contradicts BDD P12.1. With strict P9, `mtcars |> head(num)` with empty `num` escalates the entire `head(num)` call, leaving the pipeline as `mtcars |> ggplot(...)` and rendering 32 rows. The user expects `mtcars |> head() |> ggplot(...)` and 6 rows (head's default `n = 6`). The original BDD doc had this contradiction baked in (P9.1/G6.1 vs P12.1); P12.1 reflects user intent and is preserved as the canonical reading.
- **Who:** ggpaintr formula authors (the developer who writes `mtcars |> head(num) |> ggplot(...)`); end users of the rendered Shiny apps see correct interactive behavior when they clear a placeholder.
- **Constraints:**
  - Operator escalation (G2) is preserved by a separate first branch in the prune visitor — `mpg > <missing>` still becomes `ptr_missing()`, independent of the positional-missing relax.
  - Static-only prune. No trial-evaluation, no resolve↔prune coupling.
  - Eval errors from surviving partial-arg calls (e.g. `filter(year)` after dropping a sibling) surface via Phase 4a safe wrappers (`ptr_complete_expr_safe_v2` etc.) as user-visible inline errors, not silent failures.
- **Non-goals (deferred to separate scopes):**
  - Stage-disable checkbox UX feature (forward-compat checked, see "Forward Compatibility").
  - Trial-eval pruning (rejected).
  - Populating `default_drop_when_empty` with the broader tidyverse verb set (filter, mutate, arrange, select, summarize, group_by, distinct, count, …) — the user's separate "walk the tidyverse" pass.

## Decision

### The new `prune_walk.ptr_call` rule (three branches, in order)

1. **Operator escalation (G2 — preserved).** If `node$fun` is a known operator (binary or unary) AND any operand is `ptr_missing` after recursive walk → return `ptr_missing()`. Operators escalate first so they never fall through to step 2.
2. **Drop missing args (relaxed P9 — new).** For each arg in `node$args`:
   - Named missing → drop (existing behavior).
   - Positional missing → drop (was: escalate whole call to `ptr_missing`).
3. **Drop-when-empty.** After step 2, if the call has zero remaining args AND `bare_call_name(node$fun)` is in `default_drop_when_empty()` → return the drop sentinel (`NULL`); the parent context (pipeline-stage walker, arg-list walker) treats `NULL` as "drop me." Otherwise the empty call survives and renders as `verb()`, relying on the verb's own defaults at eval time.

### Operator set (hard-coded internal helper `is_operator(fun)`)

```
binary: > >= < <= == != + - * / ^ %% %/% & | && || %in% : ::
unary:  ! - +
```

Pipe operators (`%>%`, `%ptrPipeNative%`) are special-cased earlier in the visitor (pipeline-stage walker handles pipeline collapse); the operator branch must not fire on them.

### Rename

`default_safe_to_remove()` (in `R/paintr-utils.R`) → `default_drop_when_empty()`. New name reflects the unified semantic. Internal helper, unexported, no NAMESPACE churn. Contents unchanged this session.

### Drop sentinel

`prune_walk.ptr_call` returns `ptr_missing()` for the drop-when-empty case. Reuses the existing visitor convention — parent walkers (`prune_walk.ptr_pipeline`, the call's own arg-walk under the relaxed P9 rule) already treat `ptr_missing` as drop-me. No protocol extension needed.

## Alternatives Considered

| # | Alternative | Why rejected |
|---|-------------|--------------|
| 1 | Strict positional-missing escalation (status quo) | Contradicts BDD P12.1 and the user's mental model for `head(num)` empty. |
| 2 | Two lists: shape-preserving vs shape-changing verbs | User's concrete case-walk (`head(num)` → `head()`, `filter(year>=num)` → drop, `select(var)` → drop) collapsed both shape categories to a single "drop or keep" decision. The two-bucket framing turned out to be a phantom distinction. |
| 3 | Per-verb arity hint (min-args metadata) | More metadata to maintain. Eval errors from arity violations are already caught by safe wrappers; the static check adds complexity without proportionate benefit. |
| 4 | Hybrid static + dynamic prune (trial-eval each call, drop on R error) | Couples prune to upstream data and reverses the resolve↔prune flow direction. "Error" is underspecified (R-error vs downstream-eval failure). Performance + ordering cost. Existing safe wrappers already cover the silent-recovery use case as user-visible errors. |
| 5 | Operator escalation via `default_drop_when_empty` membership | Operators have a different semantic ("any missing operand kills the expression") that doesn't compose with arg-drop. Mixing confuses what "in the list" means. |
| 6 | Reuse `ptr_missing` as drop sentinel (no new class, no NULL protocol extension) | (a) `NULL` sentinel; (b) `ptr_drop()` marker class | Under relaxed P9, positional `ptr_missing` already drops the arg; reusing it as the drop-when-empty return value composes naturally with arg-walk and pipeline-walk filtering — no walker changes. |
| 7 | Add tidyverse verbs to `default_drop_when_empty` in this same change | Out of scope; user is doing a separate pass to walk the tidyverse surface. The relax is correct standalone. |
| 8 | Design the stage-disable checkbox feature in this same session | Bigger scope; forward-compat already verified — P9-static-only does not foreclose. Better to land P9 and brainstorm checkbox separately with full attention. |

## Forward Compatibility — Stage-Disable Checkbox

The user surfaced a UX safety valve: a checkbox per pipeline-verb-with-placeholders that lets end users mute a stage that emits an eval error. Designed in detail separately, but the forward-compat check confirmed P9-static-only does not paint us into a corner:

- **Stable per-stage identity** is a P4 (id-assignment) extension, not a P9 concern.
- **Disable-as-prune** is an additive pass running *before* substitute in `ptr_resolve_upstream`, on the original AST. Idempotent with the empty+in-list drop.
- **Pre-prune view** of the formula tree already exists on `state$tree` (per Phase 4a).
- **Downstream data-aware placeholders recover** correctly: when a user unchecks a broken stage, `ptr_resolve_upstream`'s subtree gets the disabled-stage stripped before substitute/prune/eval, so `var` etc. see fresh upstream data.
- **Cache key** is `deparse(post-substitute-expr)`, so disabling a stage produces a distinct cache slot — no poisoning.

Constraint that falls out of this for the checkbox feature when we design it: the disable transform must sit before substitute in `ptr_resolve_upstream`, and stage-enabled state must be a reactive that invalidates upstream caches.

## Acceptance Criteria

### Code

- [ ] `R/paintr-prune.R` `prune_walk.ptr_call` implements the three-branch rule (operator-first, drop-missing-args, drop-when-empty).
- [ ] New internal helper `is_operator(fun)` covers the listed binary + unary operators; pipe ops excluded.
- [ ] `R/paintr-utils.R` renames `default_safe_to_remove` → `default_drop_when_empty`. All call sites updated.
- [ ] Drop sentinel (`ptr_missing`) propagates correctly through pipeline-stage and arg-list walkers (no walker changes required).
- [ ] `prune_walk.ptr_layer` simplified — drops missing children (named OR positional) without escalating the layer; empty + in-list non-standalone layers still drop.

### Tests

- [ ] `tests/testthat/test-rewrite-prune.R` — tests pinning strict positional-missing escalation flipped to assert arg-drop semantics. New tests covering: positional-missing-drop with surviving args, drop-when-empty triggering stage drop, drop-when-empty triggering arg drop in nested calls, operator escalation still firing.
- [ ] `tests/testthat/test-rewrite-resolve-upstream.R` — "missing placeholder prunes pipeline stage (G6.1 trim-to-root)" expectation flipped from `nrow(mtcars)` (32) to **6** with comment referencing P12.1.
- [ ] `tests/testthat/test-rewrite-server-state.R` — P12.1 test flipped from `nrow(mtcars)` to **6** with comment "Per P12.1 — head(mtcars) default 6 rows." P12.7 (NULL pipeline) verified unaffected.

### BDD spec (`.claude/specs/core-rewrite-bdd.md`)

- [ ] **P9.1**: clarify that escalation is via the operator path; surviving `filter(ptr_missing())` then drops the named-missing arg → `filter()` empty → drop-stage if `filter` is in the list.
- [ ] **P9.7, P9.8, P9.12, P9.13, P9.20, P9.21, P9.22**: re-read; confirm no semantic change (most are about pipeline-collapse / non-pipeline pruning, orthogonal to the positional-missing relax).
- [ ] **P12.1**: keep the existing "head(mtcars), 6 rows" reading; remove any "supersedes P12.1 / G6.1 wins" hedges.
- [ ] **G2.1–G2.4**: kept as-is; clarify operators have their own escalation path independent of `default_drop_when_empty`.
- [ ] **G6.1**: rewritten — under relaxed P9, `head()` survives empty (renders + evals to 6 rows); `filter()` empty drops only if `filter` is in the list (currently not). Old "trim to root, both verbs dropped — mtcars itself" claim was the strict-P9 reading; corrected.
- [ ] New §G10 (or §P9 appendix) documents the **drop-when-empty rule** as first-class: two-step prune (drop missing args, then drop-when-empty), operator escalation as separate first rule, list user-extensible, empty-not-in-list calls survive and rely on verb defaults.

### Verification

- [ ] `devtools::document()` runs clean.
- [ ] `devtools::test()` passes (target: 0 fail, 1 skip — same as Phase 4a baseline).
- [ ] `devtools::check(--as-cran)` returns 0 errors, 1 pre-existing WARNING (`ptr_define_placeholder.Rd`), 1 NOTE (`.git`).

### Memory

- [ ] `current-status.md` updated.
- [ ] `core-rewrite-progress.md` gotcha #5 inverted (P12.1 now correct, G6.1 rewritten).
- [ ] `p9-relax-decision.md` deleted.

## Decision Log

| # | Decision | Alternatives | Why |
|---|----------|--------------|-----|
| 1 | Relax P9 positional-missing: drop arg, keep call | Strict status quo | User mental model in P12.1; strict reading contradicts BDD. |
| 2 | Operator escalation as first branch in `prune_walk.ptr_call` | Operators in `default_drop_when_empty` | Operators have a different semantic (any-missing-operand kills) that doesn't compose with arg-drop. |
| 3 | Single `drop_when_empty` list, not two by shape | Two lists / per-verb shape flag | User's case-walk collapsed shape categories to one action. The "shape-preserving vs shape-changing" framing was a phantom distinction. |
| 4 | Drop-when-empty applies anywhere, not pipeline-stage-only | Pipeline-stage-only / args-only | User confirmed `aes()` empty inside `ggplot(mtcars, aes())` should drop. Uniform rule is simpler. |
| 5 | Static-only prune | Hybrid static+dynamic / per-verb arity | Trial-eval couples prune to upstream data, reverses flow direction. Eval errors already handled by safe wrappers as user-visible inline errors. |
| 6 | Reuse `ptr_missing` as drop sentinel | `NULL` / `ptr_drop()` marker class | Composes with relaxed P9 — no walker changes, no protocol extension. |
| 7 | Rename `default_safe_to_remove` → `default_drop_when_empty` | Keep name | Old name misleading under new semantic. |
| 8 | Don't expand the verb list this session | Add tidyverse verbs now | Separate scope per user; relax is correct standalone. |
| 9 | Stage-disable checkbox feature deferred | Design now | Forward-compat verified; checkbox additive on top of P9-static-only. Not blocking 4b. |

## Cross-references

- `.claude/specs/core-rewrite.md` — parent decision (typed AST + 12 visitor passes).
- `.claude/specs/core-rewrite-bdd.md` — sections P9, P12, G2, G6 require edits per Acceptance Criteria.
- Memory `core-rewrite-progress.md` — Phase 1–3 + 4a context; gotcha #5 is the inverted invariant this spec resolves.
- Memory `p9-relax-decision.md` — pre-decision pickup notes; delete on commit.
