# Data Pipeline Placeholders — Phase D: Plot Eval Substitution

**Owner:** willju · **Created:** 2026-05-01 · **Status:** planned, not started · **Depends on:** Phase A (in worktree), Phase B (UI), Phase C (server cache)

Self-contained brief for one fresh Claude Code session.

---

## 1. Background

ggpaintr is an R package that turns ggplot-like formula strings into Shiny apps. Placeholder symbols become input widgets. The data pipeline placeholder feature lets users write `mtcars |> head(num) |> ggplot(aes(x = var))` and have `num` and `var` both register as widgets.

Roadmap:

- **Phase A (landed):** detection + trim/eval resolver. Var dropdowns get column choices via the resolver.
- **Phase B (landed):** UI Data tabset + Update data button.
- **Phase C (landed):** server caches resolved data per layer in a reactiveVal; clicks refresh the cache; stale flag shown on the button.
- **Phase D (this file):** plot rendering reads the cached resolved data instead of re-running the data pipeline. Update plot and Update data become independent.

After Phase C, plot rendering still re-runs the data expression every Update plot click (Phase A leftover behavior). The user's design intent is: **the data pipeline only runs on Update data clicks**. Plot rendering must use whatever frame is currently in the cache.

This is the structural change that makes the whole feature consistent.

---

## 2. Phase A/B/C facts

- `ptr_obj$data_pipeline_info[[layer_name]]` exists for layers with placeholder-bearing data.
- The server holds `resolved_data[[layer_name]]` reactiveVals (Phase C). Each holds the latest cached data frame for that layer (or `NULL` on irrecoverable failure).
- `ptr_resolve_layer_data` is the single resolver entry point used for column choices.
- `ptr_exec` (or whatever the runtime entry is — verify name in `R/paintr-runtime.R`) is what runs when the user clicks Update plot. It evaluates the assembled layer expressions against `eval_env`.

Verify all of the above against the current source before depending on them.

---

## 3. Goal

For each data-pipeline layer, when the plot is being rendered:

1. Substitute the layer's `data` argument with the cached resolved data frame from `resolved_data[[layer_name]]`.
2. Evaluate the substituted layer expression. The data pipeline call (e.g. `head(mtcars, num)`) is **not** re-executed.

If the cache is `NULL` (the resolver was never able to produce a frame), surface a clear message and skip rendering, the same way the current code surfaces other runtime errors.

Effect: clicking Update plot without ever clicking Update data renders the plot using the initial-load cached data (the trim-result of empty inputs). Clicking Update data, then Update plot, renders the plot using the post-click cached data. Plot and data are decoupled.

---

## 4. Scope (what to do)

### 4.1 Files expected to change

- `R/paintr-runtime.R` — substitute the data argument before evaluating the layer expression.
- `R/paintr-app.R` — pass the `resolved_data` cache (Phase C) into the runtime.
- Tests: extend `tests/testthat/test-data-pipeline-server.R` (or a new file) for the substitution behavior; possibly extend `test-publication-loop.R` style end-to-end tests if those exist.

### 4.2 Substitution mechanics

When evaluating a layer that has `data_pipeline_info[[layer_name]]`:

1. Read the cached frame: `cached <- resolved_data[[layer_name]]()`.
2. If `is.null(cached)`: bail with `list(ok = FALSE, message = ...)` mirroring the existing failure path.
3. Otherwise: take the layer expression `layer_expr`, replace `layer_expr[[data_arg_index]]` with the literal frame, then proceed with the rest of the runtime path (placeholder substitution for non-data placeholders, then eval).

R supports holding a data frame literal directly inside a call object — `as.call(list(quote(ggplot), df, quote(aes(x = mpg))))` is a valid call and `eval()` will treat the frame as already-evaluated. Phase A's `ptr_trim_and_eval` already relies on this.

The replacement should happen **before** the existing per-placeholder substitution loop, and the data-pipeline placeholders themselves should be skipped during that loop (they are no longer present in the substituted expression). Implementation hint: build a substitution-skip set from `c(...placeholder_ids in data_pipeline_info...)` and filter the placeholder map accordingly during the runtime loop.

### 4.3 Where to read the cache

The runtime function (`ptr_exec` or equivalent) needs access to `resolved_data`. Options:

- Pass `resolved_data` as a new argument to the runtime call site. Keep it `NULL` by default so non-Shiny callers (tests) still work — `NULL` falls back to evaluating the live data expression as Phase A does.
- When `resolved_data` is supplied: for each data-pipeline layer, read `resolved_data[[layer]]()` once at the start of the eval pass and substitute as in §4.2.

This mirrors the pattern from Phase C where `ptr_build_var_column_map` gained an optional `resolved_data` argument.

### 4.4 Tests

1. **Plot uses cached data.** With the repro formula, set `num` to 3, click Update data (cache now has the 3-row frame), set `num` to 7 (cache stale, but not refreshed), click Update plot. Assert the rendered plot's underlying data has 3 rows, not 7.
2. **Initial-load plot.** Without clicking Update data, click Update plot. Assert the plot renders against the initial-load cached frame (full mtcars after trim, or `head(mtcars)` depending on what the resolver produced at empty input — whichever Phase A defines).
3. **Cache failure surface.** With `nonexistent_data |> head(num) |> ggplot(...)`, click Update plot. Assert a message surfaces (no silent failure, no thrown error past the runtime boundary).
4. **Per-layer data argument.** With `ggplot(mtcars, ...) + geom_point(data = iris |> filter(num > 0))`, click Update data on the `geom_point` tab, then Update plot. Assert the geom layer renders against the iris-based cache; the ggplot layer continues to use mtcars (which has no pipeline).
5. **Regression: plain pipe (no placeholders).** Plain `mtcars |> filter(mpg > 20) |> ggplot(...)` still resolves and renders correctly. (No `data_pipeline_info` entry, so `resolved_data` is empty for that layer; runtime falls through to the live-eval path.)
6. **Existing tests still pass.** `test-var-piped-data.R` and the broader suite should not regress.

---

## 5. Out of scope

- Any new placeholder types or ui_text keys.
- Changes to the trim algorithm.
- Changes to per-layer geom parameters that aren't data — the substitution is strictly about the data argument position.
- `R CMD check` baseline cleanup.

---

## 6. Risks worth flagging in chat before merging

- **Plot vs. data divergence after click.** Once the plot can use a stale cached frame while non-data placeholders move on, the var dropdown choices come from the cached frame, but a user-typed var column might no longer exist if they re-click Update data with a different filter. The existing column-validation path in `ptr_exec` already surfaces a message in this case (see `test-var-piped-data.R` "piped data still validates the column list against placeholder input"). Confirm the message still fires with the new substitution path.
- **eval_env hygiene.** Substituting the data frame as a literal in the call object means it shows up in the call's structure, not in `eval_env`. Make sure the surrounding code doesn't accidentally re-introduce the original data expression by reading `expr_list[[layer]]` directly elsewhere — the substitution must be made on the working copy of the expression, not on `ptr_obj` itself.
- **Tests calling runtime without a Shiny session.** Make sure non-Shiny callers (unit tests) still work with `resolved_data = NULL`. The runtime should detect that case and use the live data expression (Phase A path) so unit-test behavior does not regress.

---

## 7. Conventions

Read first: `CLAUDE.md`, `.claude/rules/serena-tools.md`, `.claude/rules/coding.md`, `.claude/rules/testing.md`.

- `ptr_` prefix only on exported functions.
- `rlang::abort()` for hard errors; `cli::cli_warn()` / `cli::cli_inform()` for soft messages.
- Test command: `Rscript -e 'devtools::test()'`.
- Check command: `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`.
- Pre-existing baseline: 1 warning (`ggbeeswarm`), 1 note (`.git`). Anything new is yours.

---

## 8. Definition of done

- Plot rendering for data-pipeline layers reads from the Phase C cache.
- Update plot does not re-run the data pipeline.
- All four phases now feel coherent: Update data refreshes the cached frame; Update plot renders against the cache.
- Existing tests pass; new substitution tests pass; no new check warnings.
- Update README/vignettes only if the user asks — do not write doc copy proactively in this phase.
