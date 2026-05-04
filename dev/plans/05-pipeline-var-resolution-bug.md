---
status: done
created: 2026-05-03
completed: 2026-05-03
size: small
depends-on: none
blocks: 06-shared-and-pipeline-cohesion
outcome: per-iteration positional `var_column_map` inside `ptr_resolve_data_pipeline_expr` (R/paintr-placeholders.R); silent `tryCatch(error=NULL)` at the same loop's input/expr resolves now warn under `ptr_verbose()`. Audit at `dev/audits/placeholder-context-coverage.md` enumerates placeholder × site coverage; F3/F4 (latent contract gaps for custom placeholders) deferred to plan 06. Regression tests in `tests/testthat/test-data-pipeline-server.R` cover single-var pipeline narrowing and chained `select(var) |> select(var)` with positional upstream validation.
---

# Concern 05 — `var` placeholder inside data pipeline silently dropped

## Problem

Any formula whose `data = X |> verb(...)` chain contains a `var` placeholder evaluates as if the verb weren't there. Picking a column in the data tab and clicking "Update Data" leaves the cache identical to upstream. No error surfaces. The plot draws against the unmodified upstream data, so the user has no signal that their selection had no effect.

Minimal reproducer:

```r
ptr_app("mtcars |> select(var) |> ggplot(aes(x = var)) + geom_histogram()")
```

Picked column in the data tab → click Update Data → resolved cache stays at 11 columns of `mtcars`. `select(mtcars, picked)` is silently trimmed.

## Status quo (verified)

- `ptr_setup_data_pipeline_observers` builds its substitution context via `build_context` (`R/paintr-app.R:538-550`), which calls `ptr_define_placeholder_context()` and patches in `ns_fn`, `input`, and `shared_bindings`. **It does not populate `var_column_map`.**
- `ptr_resolve_data_pipeline_expr` (`R/paintr-placeholders.R:1051-1089`) iterates pipeline placeholders and calls `ptr_resolve_placeholder_input` → `ptr_resolve_placeholder_expr` for each.
- For `var`, `ptr_resolve_placeholder_expr` dispatches to `ptr_resolve_var_expr` (`R/paintr-placeholders.R:617`), which calls `ptr_validate_var_input` (`R/paintr-placeholders.R:1320-1340`).
- `ptr_validate_var_input` requires `context$var_column_map[[meta$layer_name]]` with `has_data = TRUE` and a `columns` vector containing the picked value. With `var_column_map = NULL` (the pipeline observer's case), it aborts with `"Input '...' cannot be resolved because no dataset information is available for layer '...'."`.
- The abort is swallowed by the `tryCatch(error = function(e) NULL)` at `ptr_resolve_data_pipeline_expr:1075-1078`. Replacement stays as the unset-data marker. `ptr_trim_and_eval` then drops the verb and returns the upstream value.

The aesthetics-side `var` does NOT have this bug because that resolution path runs through `ptr_complete_expr` at draw time, which builds a context with `var_column_map` populated from `resolved_data`. **Two different code paths compute the substitution context for the same placeholder type, with different invariants.**

Verified by direct probe at `/tmp/probe-select-var-runtime.R`: patching `ctx$var_column_map` manually with the upstream column set unblocks the resolution; `select(mtcars, mpg)` evaluates to a 1-column data frame as expected.

## Secondary issue exposed

The stale-flag observer at `R/paintr-app.R:588-602` snapshots `ptr_snapshot_data_placeholder_inputs`, which only calls `ptr_resolve_placeholder_input` (raw value), not `_expr`. So the snapshot tracks input changes correctly. On the click path, the snapshot updates *regardless* of whether `ptr_resolve_layer_data` succeeded — so the stale highlight clears even though the cache silently failed to refresh. Coherence break: button no longer highlighted = "data is current", but cache is actually still upstream.

## Impact

- Every `var(...)` reference inside `data = X |> verb(...)` is broken in `ptr_app()`, `ptr_app_bslib()`, `ptr_app_grid()`, `ptr_module_server()`, `ptr_server_state()` — pipeline-substitution path is shared.
- Silent failure: no warning, no error, plot just looks wrong.
- Vignette `ggpaintr-extensibility.Rmd` and any user-authored `select(var)` / `filter(var > num)` / `arrange(desc(var))` examples are affected.

## Open questions

1. Bootstrap source for `var_column_map` inside the pipeline observer — what's the right input?
   - **Previous resolved_data for that layer** (whatever was cached last). Coherent with current var validation rules; chicken-and-egg dissolves because validation uses *current* columns while the resolver produces the *next* set.
   - **Upstream-trimmed source data** (what `ptr_trim_and_eval` returns when all pipeline placeholders are unset). More general — gives the user the full source column set as choices, even after the pipeline narrows.
   - **Hybrid**: upstream for the *first* eval (no prior cache), previous resolved_data thereafter.
2. Should the silent `tryCatch(error = function(e) NULL)` at `ptr_resolve_data_pipeline_expr:1075` log instead of swallowing? Today it hides exactly this kind of bug.
3. Is the stale-flag coherence break (cache failed but snapshot updated) fixable in this plan, or does it belong with the larger shared-cohesion plan (06)?

## Audit recommendation

The root cause is structural: the pipeline-substitution context is built independently of the aesthetic-substitution context, and the two have diverging invariants. Same placeholder type, two contexts, one has a required field, one doesn't. **Other placeholder types may have similar latent bugs.**

Audit scope, before or alongside the fix:

- Enumerate every context field consumed by `ptr_resolve_placeholder_input` / `ptr_resolve_placeholder_expr` / `build_ui` / `bind_ui` for every built-in placeholder (`var`, `text`, `num`, `expr`, `upload`).
- Compare against fields populated by the three context-construction sites:
  - `build_context` in `ptr_setup_data_pipeline_observers` (`R/paintr-app.R:538`)
  - `ptr_define_placeholder_context` in the aesthetic-substitution path (called from `ptr_complete_expr` and `ptr_bind_placeholder_ui`)
  - any other call site that builds a context manually
- Flag every context field consumed by some path but not populated by another.
- Same audit for custom placeholders' contracts: document which context fields a custom `resolve_input`/`resolve_expr`/`bind_ui` may rely on, and assert them at context-construction time so missing fields fail loudly instead of silently.

The cleanest long-term fix is to **collapse the multiple context-construction call sites into one helper** that takes the role (`"pipeline"`, `"aesthetic"`, `"ui-bind"`) and returns a fully-populated context with all fields the role needs. That's a larger refactor; the audit determines whether it's warranted.

## Out of scope

- The shared-placeholder cohesion design (D5/D6/D7'/D8/D9/D10 + foldable shared section above picker). That work is concern 06.
- Restructuring `ptr_resolve_data_pipeline_expr` beyond the minimum needed to populate `var_column_map`.
- Replacing the silent `tryCatch` everywhere — only where it hides the kind of failure this concern surfaces.

## Success criteria

- `ptr_app("mtcars |> select(var) |> ggplot(aes(x = var)) + geom_histogram()")` works end-to-end: picking a column in the data tab + clicking Update Data narrows the cache to that column; the plot renders against the narrowed data.
- New automated test in `tests/testthat/test-data-pipeline-server.R` covering pipeline-`var` resolution end-to-end.
- Audit deliverable (a short markdown note in `dev/` or comments in code) listing every placeholder × context-construction-site combination with a verdict (OK / latent bug / needs context field).
- Decision recorded on whether the silent `tryCatch` at `ptr_resolve_data_pipeline_expr:1075` should log or remain silent.

## Recommended next-session approach

Invoke `brainstorming` with this file. Order: question 1 (bootstrap source) first — drives the implementation. Then question 2 (logging policy). Then run the audit (question 3 falls out of audit findings).
