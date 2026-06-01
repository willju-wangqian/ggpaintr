# Red-first tests for ADR 0027 (staged for per-worktree distribution)

These three files were produced by `/plan-to-test` from the plans' Interface
Contracts + BDD and were **captured failing (red)** before any implementation
exists. They are staged here — NOT in `tests/testthat/` — on purpose: if all
three lived on the shared orchestrator HEAD, every plan's worktree would inherit
all three (all red) and no plan could satisfy its full-suite-green DoD.

## Distribution (one file per plan worktree)

When `/exec-plan` creates each plan's worktree, copy that plan's file into the
worktree's `tests/testthat/`, then implement until it (and the rest of the suite)
is green:

| file | plan | copy into |
|---|---|---|
| `test-embellish-eval.R`         | 01 embellish-helpers       | `<wt-01>/tests/testthat/` |
| `test-arg-factories-vector.R`   | 02 arg-factories-vector    | `<wt-02>/tests/testthat/` |
| `test-placeholder-arg-rename.R` | 03 atomic-rename-and-sweep | `<wt-03>/tests/testthat/` |

## Red status at staging time (devtools::test, registry env intact)

- `test-embellish-eval.R`        — FAIL 3 / PASS 0 (functions absent).
- `test-arg-factories-vector.R`  — FAIL 7 / PASS 2 (2 = green-now characterization guards).
- `test-placeholder-arg-rename.R`— FAIL 5 / PASS 3 (3 = green-now characterization guards).

The **characterization guards are green now and must stay green** — they pin
existing behavior the rename must preserve (scalar `ptr_arg_numeric`, the
`ptr_arg_numeric_vector` alias, the identity default runtime, built-ins resolving).
The red tests turn green as each plan's implementation lands.

## Caveats carried from /plan-to-test

- Message-substring assertions `"length 2"` and `"column name"` (PLAN-02) are
  derived from the contract prose, not a verified literal — if the implementer
  words the abort differently, adjust the substring, do not weaken the assertion.
  `"accepts at most one positional argument"`, `"object 'mpg' not found"` / `"not
  found"`, and `"unused argument"` ARE verified against real messages.
- Coverage gap to fill during PLAN-03: the `_source` constructor rename
  (`parse_positional_arg` on a source, which has no `validate_session_input`) has
  no test here — add one.
