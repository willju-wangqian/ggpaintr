# ADR 0027 — Implementation plans

Source: [dev/adr/0027-placeholder-arg-vocabulary-and-embellish-eval.html](../../adr/0027-placeholder-arg-vocabulary-and-embellish-eval.html)
Generated: 2026-05-31 by /decision-to-plan
Orchestrator entry point: `/exec-plan dev/plans/0027-placeholder-arg-vocabulary/` (run under `/goal`)

## Merge order

`{01, 02} → 03`

(Curly braces = parallel group, mergeable in numeric order once all members PASS in-worktree audit.)

## Parallel groups

- **G1**: 01, 02   (disjoint files — `R/paintr-embellish.R` vs `R/paintr-default-args.R`; safe to implement concurrently)
- **G2**: 03        (atomic rename + sweep; branches off HEAD after 01+02 merged)

## Plans

| #  | Slug | Group | Depends on | Status | One-line summary |
|----|------|-------|------------|--------|------------------|
| 01 | embellish-helpers       | G1 | —     | PASS  | Add `embellish_identity()` + `embellish_symbol_to_string()` (additive; usable via current `runtime=`). |
| 02 | arg-factories-vector    | G1 | —     | PASS  | Add `vector=`/`length=` to `ptr_arg_numeric/string/symbol_or_string`, new `ptr_arg_symbol`; keep `ptr_arg_numeric_vector` alias. |
| 03 | atomic-rename-and-sweep | G2 | 01,02 | PASS  | Atomic rename of the 4 public constructor args, wire `embellish_eval` default, remove the alias, bump version, sweep all call sites. |

All three: /implementable PASS (stamped) + per-plan drift audit PASS (D1–D7) + global worked-example coverage PASS.

## Blocked plans

(none yet — populated if a plan fails to reach PASS after 3 /implementable attempts or 3 drift cycles.)

## Worked-example coverage

Global coverage audit: **PASS** (2026-05-31). All four ADR worked examples map to a concrete BDD `Then`; both negatives assert an exact message substring.

| anchor | kind | summary | COVERED | plan:scenario |
|---|---|---|---|---|
| what becomes possible #1 | positive | column consumer works in both ptr_app and naked paths via `embellish_symbol_to_string` | COVERED | PLAN-01: "embellish_symbol_to_string makes a tidyselect consumer work in plain R" — Then: column names exactly `c("mpg","hp")` |
| what becomes possible #2 | positive | value placeholder whose plain-R meaning is a string via custom `embellish_eval` | COVERED | PLAN-03: "custom embellish_eval yields a string in plain R" — Then: result is the string `"1"` |
| what is rejected by design #1 | negative | bare multi-positional `colvars(mpg, hp)` stays rejected (one-positional cap) | COVERED | PLAN-03: "bare multi-positional still aborts" — Then: abort message contains "accepts at most one positional argument" |
| what is rejected by design #2 | negative | relying on the identity default for a tidyselect consumer breaks the naked path | COVERED | PLAN-01: "the identity default breaks the naked tidyselect path" — Then: error message contains "mpg" and "not found" |

PLAN-02 covers no ADR worked example directly — its vector-capable factories are enabling mechanism the examples' observables (asserted in 01/03) consume. Verified acceptable, not a gap.

## Uncovered worked examples

(none yet.)

## Notes for the orchestrator

- Each parallel group merges in numeric order once all its members have PASSed in-worktree audit.
- Worktree base: 01 and 02 branch off the same orchestrator HEAD; 03 branches off HEAD after both are merged.
- Definition-of-Done command (full suite, e2e RUNNING via `devtools::test()`) is in every plan; run it on the orchestrator branch after every merge. Plan 03 is shared-runtime/codegen-adjacent — full suite, not a filtered subset.
- The rename (Plan 03) is a hard atomic break; intermediate green is preserved by making 01+02 purely additive (embellish helpers + retained `ptr_arg_numeric_vector` alias).
