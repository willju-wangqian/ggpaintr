# Browser E2E test report — feature-coverage example apps

Date: 2026-05-12 (run continued 2026-05-13)
Source under test: `dev/scripts/feature-coverage-examples.R` (`ex1_*`, `ex2_*`, `ex3_*`)
Driver: Claude in Chrome, manual launchers under `/tmp/ggp-launchers/`, background `Rscript` on port 4321.
Scope: exploratory bug-hunting. No source files were edited.

## Apps launched

| # | App | Launcher | Launched? | Usable? |
|---|-----|----------|-----------|---------|
| A | `ptr_app(ex1_formula, …)` turn-key | `appA.R` | Yes | **No** — broken (BUG-1) |
| B | `ptr_app_bslib(ex1_formula, …)` | `appB.R` | Yes | **No** — broken (BUG-1, same as A) |
| C | `ptr_app(ex3_formula)` upload formula | `appC.R` | Yes | Partial — see BUG-4/5 |
| D | `ptr_app_grid(plots = list(ex2_grid_a, ex2_grid_b), …)` | `appD.R` | Yes | **No** — broken (BUG-3) |
| E | Module embed (`ptr_module_ui`/`ptr_module_server`/`ptr_gg_extra`, formula `ex2_simple`) | `appE.R` | Yes | **Yes — fully working** |

All five apps started without an R launch error. The custom-placeholder registrations, static `ptr_translate()` / `ptr_runtime_input_spec()` calls, the `testServer()` headless-extraction blocks, and the translate-time denylist checks in the script all ran clean on `source()` (verified in `appA.log` etc.).

---

## Bugs / anomalies

### BUG-1 (severity: high) — `ptr_define_placeholder_source()` without `companion_id_fn` produces a `source_companion` node with `input_id = NA`, which crashes all downstream consumer/var widgets

**Repro:** Launch App A or App B (formula uses `pick_ds |> head(num) |> dplyr::select(colvars) |> dplyr::mutate(metric = var + log(var)) |> dplyr::filter(var > num) |> ggplot(aes(x = var, y = var, color = var)) + …`). `pick_ds` is defined via `ptr_define_placeholder_source()` with **no** `companion_id_fn`.

**Observed:** On startup the R console floods with, repeatedly, for `output$ggplot_3_1_colvars_NA_ui`, `output$ggplot_4_1_1_var_NA_ui`, `output$ggplot_4_1_2_1_var_NA_ui`, `output$ggplot_5_1_1_var_NA_ui` (and the `ggplot_1_*_var` aes pickers):

```
Warning: Error in map$get: key must be not be "" or NA
  144: map$get
  143: .values$get
  142: .subset2(x, "impl")$get
  141: [[.reactivevalues
  139: <reactive> [/Users/willju/Research/ggpaintr-rewrite/R/paintr-server.R#822]
  117: entry_reactive
  116: renderUI [/Users/willju/Research/ggpaintr-rewrite/R/paintr-server.R#829]
  115: func
   99: output$ggplot_3_1_colvars_NA_ui
    1: shiny::runApp
```

and also (once each) at `observe [paintr-server.R#352]` and `observeEvent(input[[src_id]]) [paintr-server.R#386]`.

Root cause (traced): `ptr_runtime_input_spec(ex1_tree)` lists a `role = source_companion` row for `pick_ds` with `input_id = NA` and `source_id = ggplot_1_pick_ds_NA`. In `ptr_setup_pipelines()` (`paintr-server.R` ~L340-373) `comp_id <- if (!is.null(node$companion_id)) ns(node$companion_id) else NULL` — `node$companion_id` is `NA` (not `NULL`), so the guard passes and a bogus namespaced id is used; `ptr_bind_source_autoname()` then does `observeEvent(input[[src_id]])` / `input[[comp_id]]` with `comp_id` derived from `NA`. The consumer-UI reactive (`paintr-server.R` ~L821-822, `for (cmp in upstream_source_companion_ids) val <- input[[ns(cmp)]]`) hits the same `NA`/`""` reactiveValues key. The `!is.null()` checks need to also reject `NA` / non-`nzchar` companion ids — or a source with no `companion_id_fn` shouldn't emit a `source_companion` node at all.

**Effect:** Every column picker downstream of the pipeline (`colvars`, the `var` transforms in `mutate`/`filter`, and the `ggplot(aes(x=var, y=var, color=var))` x/y/color pickers) stays permanently in the `recalculating` state — the `<div id="…_ui">` elements never get content. The "Data" sub-tab shows the `select()` enable checkbox with no widget under it; nothing can be selected; clicking "Update plot" silently does nothing (`#ptr_code` stays empty, no plot, no error pane message). **App A and App B are unusable for building a plot.**

Expected: `pick_ds` source resolves to a `ggplot2` dataset, the consumer pickers populate with that dataset's columns, and switching the `pick_ds` dropdown refreshes the downstream column choices.

---

### BUG-2 (severity: medium) — `ui_text` `shell$draw_button$label` override is ignored by `ptr_app()` / `ptr_app_bslib()`

**Repro:** App A / App B launched with `ui_text = ex1_ui_text`, which sets `shell = list(… draw_button = list(label = "Render plot"))`.

**Observed:** the draw button reads **"Update plot"** (the package default). The sibling override `shell$title$label = "Feature-coverage demo"` *does* take effect (page header + `document.title`), so the `ptr_ui_text()` rules are being parsed; only the draw-button leaf is dropped.

Root cause (traced): `paintr-app.R:191` builds the button with `label = shell_copy$update_plot_label %||% "Update plot"`, but `layer_panel_default_shell_copy()` (`paintr-build-ui.R:404-411`) only populates `data_subtab_label`, `controls_subtab_label`, `layer_checkbox_label`, `layer_picker_label` — there is no `update_plot_label` key, so it's always `NULL`. The copy registry maps the override to `c("shell", "draw_button")`, i.e. `shell_copy$draw_button$label`, which is never read. Same code path is used by the bslib shell. (`draw_all_label` for the grid is a separate function arg and is unaffected — see App D, which correctly showed "Render both".)

Expected: draw button reads "Render plot".

---

### BUG-3 (severity: high) — `ptr_app_grid()` aborts in `ptr_validate_shared_bindings()` when a `shared_ui` key is used in a later plot formula but not the first

**Repro:** Launch App D — `ptr_app_grid(plots = list(ex2_grid_a, ex2_grid_b), shared_ui = list(cv = …, pt = …), …)`. `ex2_grid_a` uses only `colvars(shared = 'cv')`; `ex2_grid_b` uses `colvars(shared = 'cv')` **and** `num(shared = 'pt')`.

**Observed:** R console:

```
Warning: Error in ptr_validate_shared_bindings: `shared` references key "pt" which is not used in any plot formula. Available formula keys: "cv".
  ...
  57: ptr_validate_shared_bindings [/Users/willju/Research/ggpaintr-rewrite/R/paintr-utils.R#92]
  56: ptr_init_state [/Users/willju/Research/ggpaintr-rewrite/R/paintr-server.R#108]
  55: ptr_server [/Users/willju/Research/ggpaintr-rewrite/R/paintr-server.R#266]
  54: module [/Users/willju/Research/ggpaintr-rewrite/R/paintr-app.R#420]
  ...
  40: server [/Users/willju/Research/ggpaintr-rewrite/R/paintr-app.R#628]
   1: shiny::runApp
```

The validation runs per-plot-module against the *full* `shared` list, so when initialising the `plot_1` module (formula `ex2_grid_a`, which only has key `cv`) the presence of `pt` in the shared list trips the "not used in any plot formula" abort. Result: **both** plot panels render with the title bar / layer picker / "Update plot" button but the `*-ptr_plot` outputs are stuck `recalculating` and "Render both" / "Update plot" produce no plot, no code, no error. **App D is non-functional.**

`pt` *is* used (in `ex2_grid_b`'s `geom_point(alpha = num(shared = 'pt'))`), so the diagnostic ("not used in any plot formula") is also factually wrong from the user's point of view — the validator only saw one formula's keys. The grid wiring should validate `shared` against the union of all plot formulas (and a shared widget unused by a given plot should be allowed when initialising that plot's module).

---

### BUG-4 (severity: medium) — custom `ptr_define_placeholder_consumer()` widget (`colvars`) renders with **zero column options** after a successful upload, while built-in `var` consumers in the same pipeline get the columns

**Repro:** App C (`ex3_formula = "upload |> head(num) |> select(colvars) |> mutate(new_var = var + var) |> filter(var > num) |> ggplot(aes(x = var, y = var, color = var)) + geom_point(alpha = var(shared = 'v'), size = var(shared = 'v')) + geom_line(data = upload, aes(x = var, y = var)) + facet_wrap(expr) + labs(title = text)"`). Upload `tests/testthat/fixtures/penguins.csv` into the `upload` fileInput (auto-name companion correctly fills in "penguins"). Set the `head()` num to 50.

**Observed:** After the upload completes ("Upload complete" shown, `_name` companion auto-filled), the built-in `var` pickers downstream — `ggplot_4_1_1_var_NA` / `ggplot_4_1_2_var_NA` (the `mutate(new_var = var + var)` pickers), `ggplot_5_1_1_var_NA` (`filter(var > num)`), and the `ggplot_1_{1,2,3}_var_NA` aes pickers — all populate with the 8 penguins columns. But the **`colvars` widget for `select(colvars)` (`ggplot_3_1_colvars_NA`) renders with an empty `<select>` (0 options)** — `selectInput(node$id, choices = cols, …)` is getting `cols = character()`. So the custom-consumer `cols` plumbing isn't delivering the upstream column vector for that stage even though stages further down resolve fine. (App A/B couldn't be checked here because of BUG-1.)

Expected: `colvars` widget offers the 8 penguins columns.

---

### BUG-5 (severity: low / UX) — stale "shared picker cannot be resolved" error stays in the Error pane after a successful plot render

**Repro:** App C. Before any input the `var(shared = 'v')` widget shows (both in the Shared-controls panel and in the `#ptr_error` pane): `Shared \`var(shared = "v")\` cannot be resolved. This shared picker has no dataset to list columns from — its data is produced entirely by other inputs that aren't filled in yet. …` — which is reasonable as an *advisory*. Then upload `penguins.csv`, set x = `bill_length_mm`, y = `body_mass_g`, color = `species`, `head()` num = 50, facet `~ species`, title = "Penguins upload test", click "Update plot".

**Observed:** the plot renders fine and `#ptr_code` shows a valid program (`penguins |> head(50) |> ggplot(aes(x = bill_length_mm, y = body_mass_g, color = species)) + geom_point() + geom_line(data = penguins) + facet_wrap(~species) + labs(title = "Penguins upload test")`), but the **`#ptr_error` pane still shows the "Shared `var(shared = "v")` … cannot be resolved" error**. Expected: a successful draw clears the error pane (or the unresolvable-shared-picker condition shouldn't be surfaced in the error pane as an error when the plot still renders by dropping that aesthetic).

---

### BUG-6 (severity: low / UX) — misleading pipeline-stage enable-checkbox labels

**Repro:** App A / App B, "Data" sub-tab.

**Observed:** the per-stage enable/disable checkboxes are labelled by the innermost call that wraps the stage's first placeholder rather than the pipeline verb: `head(num)` → "head()" (fine), `dplyr::mutate(metric = var + log(var))` → "+()" (label of the `+` call), `dplyr::filter(var > num)` → ">()" (label of `>`), and `dplyr::select(colvars)` → **no label at all** (the `dplyr::select` call is `::`-namespaced so no name is derived). Expected: each stage checkbox is labelled with its verb, e.g. "mutate()", "filter()", "select()".

---

### Minor / cosmetic observations (not filed as bugs)

- App B (bslib): the `ptr_app_bslib(… title = "Feature-coverage demo (bslib)")` window title appears to be overridden by the `ui_text` shell title — both the navbar and `document.title` read "Feature-coverage demo", not "… (bslib)". Could be intended precedence; flagging only because the script comment treats the bslib `title=` as a distinct argument.
- App D: `document.title` for the grid app is the default "ggpaintr Plot Builder", not the grid `title` ("Two iris views, shared column pick"). The `title` arg sets only the on-page header.
- All `num` widgets render as `<input type="number" value="NA">` when unset. `value="NA"` is not a valid HTML number value so it displays blank — harmless, but it's literally the string "NA" in the DOM.
- Browser console only ever showed Chrome-extension noise ("A listener indicated an asynchronous response by returning true, but the message channel closed before a response was received"); no app-originated JS errors anywhere.

---

## Verified working

- **App E (module embed)** — fully functional end to end:
  - `ptr_module_ui("m", ex2_simple)` + `ptr_module_server("m", ex2_simple)` render and wire up; the namespaced inputs (`m-…`) work.
  - `geom_point(size = num)` and `labs(title = text)` widgets work; "Update plot" produces `ggplot(data = mtcars, aes(x = mpg, y = hp)) + geom_point(size = 3) + labs(title = "Module embed test")` and renders the plot in `#m-ptr_plot`.
  - The user-owned `output$my_plot` pane (reading `state$runtime()$plot + theme_minimal()`) renders on the first "Update plot" click and re-renders on subsequent clicks.
  - **`ptr_gg_extra(state, ggplot2::scale_x_log10())`** — clicking "Toggle log-x" then "Update plot" appends `ggplot2::scale_x_log10()` to the generated code and the user pane re-renders. ✔
- **App C (upload formula)** — partially verified:
  - File upload via the `upload` fileInput works; the `_name` companion auto-fills from the filename ("penguins"); the dataset-name companion observer fires correctly.
  - Pipeline `var` widgets (`mutate`/`filter` transforms) and the `ggplot(aes(x/y/color = var))` pickers refresh from the uploaded frame's columns (8 penguins columns) — confirming lazy upstream resolution through `|>`.
  - `head(num)` applied as `head(50)`; `facet_wrap(expr)` applied as `facet_wrap(~species)`; `labs(title = text)` applied; `geom_line(data = upload, …)` rewrote to `geom_line(data = penguins)`.
  - Plot rendered; generated code matches the formula minus the dropped empty-arg pieces; no R errors in `appC.log` after the upload.
  - Pre-upload state correctly shows an advisory for the unresolvable shared `var` picker rather than crashing.
- **App A** — `checkbox_defaults = list(geom_smooth = FALSE)` honoured (`geom_smooth_checkbox` starts unchecked; the other layer checkboxes start checked). `ui_text` shell title override ("Feature-coverage demo") applied to header and `document.title`. `coord_cartesian` `range` sliders render with the custom 0–100 slider from `ptr_define_placeholder_value()` and default to `c(0, 100)`. Layer picker lists all 7 layers. (Most other App-A interactions are blocked by BUG-1.)
- **App D** — grid shell renders: `title` header "Two iris views, shared column pick", "Render both" button (so the grid-specific `draw_all_label` arg is wired, unlike BUG-2), shared `cv` multi-select (pre-selected `Sepal.Length`, `Sepal.Width`) and `pt` slider (default 0.6, step 0.05), two per-plot panels each with a layer picker + "Update plot". (Plot rendering blocked by BUG-3.)
- **Translate-time denylist** (`appA.log` / `appC.log`): `ptr_translate("… labs(title = system('id'))")` aborts with the expected `\`system\` is not allowed in an \`expr\` input …` message; `expr_check = FALSE` bypasses it; `expr_check = list(deny_list = …)` swaps the list and `eval` is then blocked. The *runtime* widget denylist could not be exercised because every reachable app's `expr` widget is either absent (`ex2_simple`) or blocked behind BUG-1 (`ex1_formula`'s `facet_wrap(expr)` / `geom_smooth(method = expr)`).
- `ptr_options()` / `ptr_resolve_ui_text()` / `ptr_runtime_input_spec()` / the `testServer()` + `ptr_server()` headless-extraction blocks / `ptr_llm_*()` topics — all ran clean on `source()` (output captured in the launch logs).

## Summary

**6 bugs** (2 high, 3 medium, 1 low) + a handful of cosmetic notes.

Headlines:
1. **BUG-1 (high):** a `ptr_define_placeholder_source()` placeholder created without `companion_id_fn` emits a `source_companion` node with `input_id = NA`, which throws `map$get: key must be not be "" or NA` in `ptr_setup_pipelines()` and in every downstream consumer-UI reactive — bricking the whole pipeline + all column pickers. Breaks App A and App B entirely.
2. **BUG-3 (high):** `ptr_app_grid()` aborts in `ptr_validate_shared_bindings()` if a `shared_ui` key is used only in a non-first plot formula ("`shared` references key "pt" which is not used in any plot formula") — breaks App D entirely.
3. **BUG-2 (medium):** `ui_text` `shell$draw_button$label` is silently ignored by `ptr_app()`/`ptr_app_bslib()` (reads a non-existent `shell_copy$update_plot_label`).
4. **BUG-4 (medium):** a custom `ptr_define_placeholder_consumer()` widget gets `cols = character()` (empty option list) after an upload while built-in `var` consumers in the same pipeline get the columns.
5. **BUG-5 / BUG-6 (low):** stale shared-picker error left in the error pane after a successful draw; misleading pipeline-stage checkbox labels (`+()`, `>()`, blank for `dplyr::select`).

App E (module embed + `ptr_gg_extra`) is the one fully-working app.
