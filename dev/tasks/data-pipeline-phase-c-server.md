# Data Pipeline Placeholders — Phase C: Server Wiring & Stale Flag

**Owner:** willju · **Created:** 2026-05-01 · **Status:** planned, not started · **Depends on:** Phase A (in worktree) and Phase B (UI Data tab landed)

Self-contained brief for one fresh Claude Code session. Pick up cold.

---

## 1. Background

ggpaintr is an R package that turns ggplot-like formula strings into Shiny apps. Placeholder symbols (`var`, `text`, `num`, `expr`, `upload`) become input widgets. This work adds support for placeholders nested *inside* the data expression of a layer (e.g. `mtcars |> head(num) |> ggplot(aes(x = var))`).

Roadmap:

- **Phase A (already landed):** parse-time detection + a trim-arg/trim-verb resolver. Var dropdowns get column choices on every reactive cycle by re-running the trim algorithm with current input values.
- **Phase B (already landed):** new "Data" tabset page in the UI; one sub-tab per data-pipeline layer with the relevant placeholder controls plus an "Update data" `actionButton`. Button does nothing yet.
- **Phase C (this file):** wire the button. Cache the resolved data in a per-layer reactiveVal; observer on click re-runs the resolver and writes to the cache; var dropdowns read from the cache; a stale flag visually marks the button when the cached data is no longer in sync with the current data-placeholder inputs.
- **Phase D (separate plan):** divert plot rendering for data-pipeline layers to read from the same cache, so the data pipeline only runs on Update data clicks (not on Update plot).

Phase C does **not** change plot rendering. Plot rendering still uses the live data expression (Phase A behavior). The decoupling lands in Phase D.

---

## 2. Phase A facts (anchored on `ptr_obj`)

- `ptr_obj$data_pipeline_info[[layer_name]]` exists when a layer's data argument is a call expression containing one or more placeholder symbols. Fields: `data_arg_index` (int), `placeholder_ids` (chr).
- `ptr_resolve_data_pipeline_expr(layer_expr, data_arg_index, placeholder_ids, ptr_obj, layer_name, input, context, eval_env)` — substitutes placeholder values (or an unset marker) and returns `list(ok = <lgl>, value = <data frame or NULL>)`.
- `ptr_resolve_layer_data()` already calls the resolver when `data_pipeline_info[[layer]]` is present; you can keep using `ptr_resolve_layer_data` as the single entry point.
- `ptr_update_data_input_id(layer_name)` (added in Phase B) returns the button input id, e.g. `"ptr_update_data_ggplot"`.

Verify in the codebase before depending on names — Phase A and B may have been refined.

---

## 3. Goal

1. Each data-pipeline layer holds a `reactiveVal` with the cached resolved data frame, seeded at app start by running the resolver with empty input (initial-load behavior — placeholders unset → trimmed to root).
2. Clicking the per-layer Update data button re-runs the resolver with current input values and writes the result to that reactiveVal.
3. `ptr_build_var_column_map` (the column-choice computation feeding `var` dropdowns) reads from the reactiveVal instead of recomputing from the live formula on every input change.
4. A stale flag toggles a CSS class on the button when any of the layer's data-placeholder inputs has changed since the last successful click. Default class name is `ui_text$update_data_stale_class` (Phase B reserved this key; consume it now). If the class string is empty, render no class — effectively no visual indicator.

---

## 4. Scope (what to do)

### 4.1 Files expected to change

- `R/paintr-app.R` and/or `R/paintr-runtime.R` — server module: reactiveVals, observers, stale tracking.
- `R/paintr-placeholders.R` — `ptr_build_var_column_map` reads cached data when available; falls back to live resolver if no cache (e.g. when called outside a Shiny session by a test).
- `R/paintr-ui.R` — wire dynamic class on the button via `shinyjs::addCssClass`/`removeCssClass`, or by emitting an output that toggles a class. Pick the existing project pattern; ggpaintr does not currently use shinyjs unless added in Phase B — verify.
- Tests: new `tests/testthat/test-data-pipeline-server.R`.

### 4.2 ReactiveVal layout

In the server module, after `ptr_obj` is in scope:

- `resolved_data <- list()` — keyed by layer name; each value is a `reactiveVal()`.
- `last_click_inputs <- list()` — keyed by layer name; each value is a `reactiveVal()` storing a snapshot of the placeholder values at the time of the last click (used to detect stale state). Snapshot is a named list keyed by placeholder id.
- Initialize each layer:

```r
for (layer_name in names(ptr_obj$data_pipeline_info)) {
  resolved_data[[layer_name]] <- shiny::reactiveVal()
  last_click_inputs[[layer_name]] <- shiny::reactiveVal(NULL)
  # Seed with initial-load resolution.
  initial <- ptr_resolve_layer_data(
    ptr_obj, layer_name,
    input = list(),  # nothing set yet
    context = context,
    eval_env = eval_env
  )
  resolved_data[[layer_name]](if (isTRUE(initial$has_data)) initial$data else NULL)
}
```

If reactiveValues can be created in a loop more cleanly with `reactiveValues()` (a single object keyed by name), use that; the choice is taste, not correctness.

### 4.3 Click observer

For each `layer_name`:

```r
shiny::observeEvent(input[[ptr_update_data_input_id(layer_name)]], {
  res <- ptr_resolve_layer_data(
    ptr_obj, layer_name,
    input = input,
    context = context,
    eval_env = eval_env
  )
  if (isTRUE(res$has_data) && !is.null(res$data)) {
    resolved_data[[layer_name]](res$data)
    snapshot <- snapshot_data_placeholder_inputs(ptr_obj, layer_name, input, context)
    last_click_inputs[[layer_name]](snapshot)
  } else {
    # Failure path: surface a clear message via cli::cli_warn or the existing
    # error channel. Do not overwrite the cached data with NULL.
  }
}, ignoreInit = TRUE)
```

`snapshot_data_placeholder_inputs` is a new helper that returns a named list keyed by `placeholder_id` with the current resolved value (whatever `ptr_resolve_placeholder_input` returns).

### 4.4 Stale detection

A reactive that compares the current placeholder values to the snapshot:

```r
is_stale <- function(layer_name) {
  shiny::reactive({
    snap <- last_click_inputs[[layer_name]]()
    current <- snapshot_data_placeholder_inputs(ptr_obj, layer_name, input, context)
    !identical(snap, current)
  })
}
```

Then for each layer, an observer toggles the CSS class on the button:

```r
shiny::observe({
  cls <- ui_text$update_data_stale_class %||% ""
  if (!nzchar(cls)) return()
  if (is_stale(layer_name)()) {
    shinyjs::addCssClass(id = ptr_update_data_input_id(layer_name), class = cls)
  } else {
    shinyjs::removeCssClass(id = ptr_update_data_input_id(layer_name), class = cls)
  }
})
```

If `shinyjs` is not already a dependency, prefer an alternative that does not add one — e.g. render the class via a `uiOutput`/`renderUI` rebuild, or via a tiny custom message handler. Match the existing project pattern; if unsure, ask before adding `shinyjs`.

### 4.5 `ptr_build_var_column_map` integration

`ptr_build_var_column_map` currently calls `ptr_resolve_layer_data` for `"ggplot"` and for each var-bearing layer. Modify so that, **when called inside a Shiny session and a `resolved_data[[layer]]` reactiveVal exists**, it returns the cached frame instead. The cleanest path is:

- Add a new optional argument `resolved_data = NULL` (a function or named list of reactiveVals/reactives keyed by layer name).
- If `resolved_data[[layer_name]]` is supplied, prefer its current value.
- Else: existing behavior.

The runtime path (`R/paintr-runtime.R` or wherever `ptr_build_var_column_map` is called from the server) passes the reactiveVal collection.

Test-call sites that invoke `ptr_build_var_column_map` directly without a Shiny context (unit tests) keep working because `resolved_data = NULL` triggers the fallback.

### 4.6 Tests to add (`test-data-pipeline-server.R`)

Use `shiny::testServer` for module-level tests. Patterns to follow exist already in the test suite (grep `testServer` to find examples).

1. **Initial seed.** Spin up the server with the repro formula, assert `resolved_data[["ggplot"]]()` equals `head(mtcars)` (or whatever the trim algorithm returns at empty input).
2. **Click updates cache.** Set `input$num <- 3`, set `input[[ptr_update_data_input_id("ggplot")]] <- 1`, assert `resolved_data[["ggplot"]]()` is the 3-row frame.
3. **Stale flag.** After the click, change `input$num` to 5 (without clicking again). Assert `is_stale("ggplot")()` returns TRUE. Click again, assert FALSE.
4. **var dropdowns reflect the cache.** Resolve var column choices; assert columns come from the cached frame, not a fresh recomputation when the cache is set.
5. **Failure path.** Spin up with `nonexistent_data |> head(num) |> ggplot(...)`. Click. Assert: cache remains its initial value (likely `NULL` from a failed initial seed); a message is surfaced through whatever channel the rest of ggpaintr uses for runtime errors. Do not throw.

---

## 5. Out of scope

- Plot-eval substitution. Plot rendering for data-pipeline layers still re-runs the live data expression on every Update plot click. Phase D handles this.
- Any change to `ptr_resolve_layer_data` or the trim algorithm.
- New placeholder types, new ui_text keys beyond `update_data_stale_class`.
- `R CMD check` baseline cleanup.

---

## 6. Conventions

Read first: `CLAUDE.md`, `.claude/rules/serena-tools.md`, `.claude/rules/coding.md`, `.claude/rules/testing.md`.

- Public API: `ptr_` prefix only on exports.
- Errors: `rlang::abort()`. Warnings: `cli::cli_warn()`. User messages: `cli::cli_inform()`.
- Test command: `Rscript -e 'devtools::test()'`.
- Check command: `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`.
- Pre-existing baseline: 1 warning (`ggbeeswarm`), 1 note (`.git`). Anything new is yours.

---

## 7. Definition of done

- Server creates and maintains a per-layer cached resolved data frame.
- Update data button updates the cache; var column choices reflect the cache.
- Stale flag visually marks the button when inputs diverge from the snapshot.
- Plot rendering is unchanged from Phase A (still runs the live data expression on Update plot — that's intentional for this phase; Phase D will fix it).
- All tests pass; no new check warnings.
- Stop. Do not start Phase D.
