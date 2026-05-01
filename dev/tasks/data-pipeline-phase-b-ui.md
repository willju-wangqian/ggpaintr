# Data Pipeline Placeholders — Phase B: UI Data Tab

**Owner:** willju · **Created:** 2026-05-01 · **Status:** planned, not started

Self-contained brief for one fresh Claude Code session. Pick up cold; do not start Phase C/D in the same session.

---

## 1. Background — why this work exists

ggpaintr lets users write formulas like `ptr_app("ggplot(mtcars, aes(x = var)) + geom_point()")` where placeholder symbols (`var`, `text`, `num`, `expr`, `upload`) become Shiny widgets. Until recently, placeholders were only supported inside the *plot* layers (the `aes()`, `theme()`, `labs()`, etc.). Pipelines that contained placeholders inside the *data* expression — e.g. `mtcars |> head(num) |> ggplot(aes(x = var)) + geom_point()` — silently failed: var dropdowns came back empty and the app could not render.

The full feature is delivered in four phases:

- **Phase A (done, in worktree):** detection + resolver. Var dropdowns now populate at initial load via a trim-arg → eval → trim-verb fallback algorithm. Plot rendering still uses the live data expression on every reactive cycle.
- **Phase B (this file):** UI surface — a separate Data tabset page, one sub-tab per data-pipeline layer, with the relevant placeholder controls plus an "Update data" button per tab.
- **Phase C (separate plan):** server wiring — per-layer reactiveVal for the resolved data, observer on the Update data button, stale-flag tracking.
- **Phase D (separate plan):** plot-eval substitution — Plot rendering reads the cached resolved data instead of re-running the data pipeline; "Update plot" and "Update data" become independent.

Phase B is **UI only**. Do not touch server logic.

---

## 2. What Phase A already delivered (don't reimplement)

- `ptr_parse_formula()` populates `ptr_obj$data_pipeline_info`, a named list keyed by layer name. Each entry is `list(data_arg_index = <int>, placeholder_ids = <chr>)`. Only present for layers whose data argument is a call expression containing one or more placeholder symbols.
- `R/paintr-placeholders.R` has the resolver helpers: `ptr_data_arg_index`, `ptr_compute_data_pipeline_info`, `ptr_unset_data_marker`, `ptr_expr_contains_marker`, `ptr_trim_and_eval`, `ptr_resolve_data_pipeline_expr`. `ptr_resolve_layer_data()` already routes data-pipeline layers through the new resolver.
- `tests/testthat/test-data-pipeline-placeholders.R` covers detection, the trim algorithm, and the resolver path. Full suite is green (~1545 tests).

You do not need to change any of this.

---

## 3. Goal

Render a "Data" tabset page in the Shiny UI when the parsed formula contains at least one data-pipeline layer. Each sub-tab corresponds to one such layer and exposes:

1. The placeholder controls for `placeholder_ids` of that layer (already-rendered widgets reused via the existing per-placeholder UI builder; same input ids as on the Plot side, so no duplicate Shiny inputs).
2. An "Update data" `actionButton`.

The button does nothing yet — that's Phase C.

---

## 4. Scope (what to do)

### 4.1 Files expected to change

- `R/paintr-ui.R` — add the Data tabset page builder.
- `R/paintr-copy.R` (or wherever `ui_text` defaults are registered) — add `update_data_label` (default `"Update data"`).
- `tests/testthat/test-data-pipeline-ui.R` (new) — UI structure tests.
- `tests/testthat/test-copy-rules.R` — registry row for the new key.
- `man/`, `NAMESPACE` — only if you add or change exported functions; otherwise leave alone.

### 4.2 UI structure

- The Data page is a sibling of the existing layer tabset page. Render it only when `length(ptr_obj$data_pipeline_info) > 0`. When it is rendered, it should appear before the layer tabset (data flows first; plot follows). Confirm placement against existing patterns; if there is precedent for tab order, match it.
- Each entry in `data_pipeline_info` becomes one sub-tab. Tab label = layer name as stored on `ptr_obj` (e.g. `"ggplot"`, `"geom_point"`, `"geom_point_2"` for duplicates). No additional formatting.
- Inside each sub-tab:
  - Render the placeholder controls for that layer. Reuse the existing per-placeholder UI builder (search `R/paintr-ui.R` and `R/paintr-placeholders.R` for how layer placeholder controls are currently rendered; the same builder takes a meta and returns a tag). Each control uses the same Shiny input id it already has — when the placeholder also appears in the Plot tabset, both tabs render the same widget by id (Shiny syncs them automatically).

    **Note:** in practice a single placeholder occurrence has exactly one id, and that id appears at exactly one place in the formula AST. So a placeholder used in the data pipeline will appear only in the Data tab — the "shared between tabs" case is a theoretical concern, not a practical one. Build the UI as if there is no overlap and rely on Shiny's natural id semantics.
  - Below the controls, a `shiny::actionButton`:
    - Input id: derive from a stable helper, e.g. `ptr_update_data_input_id(layer_name)` returning `paste0("ptr_update_data_", layer_name)`. Add this helper alongside other id helpers; export it only if other id helpers are exported.
    - Label: `ui_text$update_data_label` if set, else `"Update data"`.

### 4.3 `ui_text` plumbing

- Add `update_data_label` to the copy registry. Default `"Update data"`.
- Reserve `update_data_stale_class` as well (default empty string), but do not use it yet — Phase C will consume it. Document in the function header that the latter is reserved.
- Follow the exact pattern used by other `ui_text` keys; do not invent new validation.

### 4.4 Tests to add (new file `test-data-pipeline-ui.R`)

Use `htmltools` and `rvest`/`xml2` query helpers if the codebase already uses them; otherwise, walk the tag tree directly. Match the style of existing UI tests (`grep -l 'ptr_app_ui\|ptr_ui' tests/testthat/`).

1. **Renders Data tabset when present.** Parse `mtcars |> head(num) |> ggplot(aes(x = var, y = var)) + geom_point()`, build the UI, assert there is a tabset/tab labeled "Data" (or whatever wrapper is used) containing exactly one sub-tab labeled `"ggplot"`. Inside that sub-tab, assert: at least one widget for `num`, and an `actionButton` with input id matching `ptr_update_data_input_id("ggplot")` and label `"Update data"`.
2. **Absent when no data pipeline.** Parse `ggplot(data = mtcars, aes(x = var)) + geom_point()`, build the UI, assert the Data tabset is not rendered.
3. **Custom label via `ui_text`.** Pass `ui_text = list(update_data_label = "Refresh dataset")`. Assert the button reflects it.
4. **Multiple layers.** Parse a formula with both a head-layer pipeline and a per-layer `geom_point(data = mtcars |> filter(num > 0))`. Assert two sub-tabs: `"ggplot"` and `"geom_point"`. Each has its own button id.

### 4.5 Tests to update (`test-copy-rules.R`)

Add a row asserting the new `update_data_label` default. If the file iterates a known list of valid keys, extend it. Match the existing pattern; do not refactor.

---

## 5. Out of scope (do NOT do)

- Server-side observer on the Update data button (Phase C).
- Stale-flag logic / `update_data_stale_class` wiring (Phase C).
- Any change to `ptr_exec`, `ptr_resolve_layer_data`, `ptr_build_var_column_map`, or the runtime path (those belong to Phase A or Phase D).
- `R CMD check` baseline cleanup (`ggbeeswarm` warning, `.git` note are pre-existing and stay).

---

## 6. Conventions

Read these before editing:

- `CLAUDE.md` (root) — package conventions
- `.claude/rules/serena-tools.md` — prefer Serena symbolic tools over Read/Grep
- `.claude/rules/coding.md` — `ptr_` prefix only on exports, `rlang::abort()`, `cli::cli_warn()`
- `.claude/rules/testing.md` — testthat edition 3 patterns

Commands:

- Test: `Rscript -e 'devtools::test()'`
- Check: `Rscript -e 'devtools::check(document = FALSE, manual = FALSE, args = c("--as-cran", "--no-manual"))'`
- Pre-existing baseline: 1 warning (`ggbeeswarm` unstated dep in tests), 1 note (`.git` hidden dir). Anything new is yours.

---

## 7. Definition of done

- New Data tabset page renders correctly under the conditions described in §4.4.
- All existing tests still pass.
- New tests in `test-data-pipeline-ui.R` and the registry assertion pass.
- No new `R CMD check` warnings or notes beyond the pre-existing two.
- Stop. Do not start Phase C.

Confidence target: leave a short note in chat if any step diverged from this plan and why.
