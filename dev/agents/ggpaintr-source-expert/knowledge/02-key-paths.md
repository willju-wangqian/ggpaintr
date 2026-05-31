# Key paths — annotated traces with citations

The highest-leverage knowledge file. Each path is the sequence of functions a value flows through, with `file:line` for every handoff. All citations were verified against source on **2026-05-27** via `mcp__serena__find_symbol` + `grep -n`. Re-verify before using as the basis for code changes — line numbers drift.

Paths covered:

- **A.** Formula string → `ptr_root` tree
- **B.** Tree → Shiny UI (controls + outputs)
- **C.** Update click → reactive runtime trigger
- **D.** Snapshot → `{code_text, plot, error}` (the pure pipeline)
- **E.** Substitute hook — how one placeholder's input becomes a literal
- **F.** Output bindings — `state$runtime()` → Shiny outputs
- **G.** L3 custom render — extracting from `state$runtime()`
- **H.** Shared placeholder resolution (multi-instance)
- **I.** Upload flow + source mutex (ADR 0025)

## Path A — formula → `ptr_root` tree (translate)

The pure-functional intake. Runs once per `ptr_server`/`ptr_app` instance at boot.

1. **L1 entry** `ptr_app(formula, …)` — `R/paintr-app.R:134`. Captures the formula expression, then calls `ptr_make_app_server`.
2. **Formula capture** `ptr_capture_formula(formula_captured, envir)` — `R/paintr-app.R:196`. Unwraps a top-level `{ … }` block so `|> ptr_app()` works (regression test added in commit `d5dbd2c`).
3. **App wiring** `ptr_make_app_server(formula, tree, envir, ui_text, …)` — `R/paintr-app.R:302`. Glues UI + server together; calls `ptr_translate` on the captured formula.
4. **Translate (the parser)** `ptr_translate(formula, expr_check, max_depth, …)` — `R/paintr-translate.R:21`. Builds the typed `ptr_root` tree. Inside translate:
   - pipeline lift (`try_lift_to_pipeline` / `desugar_pipes_to_nested` / `resugar_pipeline_stages`)
   - per-call translate (`translate_call` → `translate_layer` → `translate_node`)
   - placeholder detection (`is_placeholder_call` → `build_placeholder_node`)
   - structural-keyword unwrap (`unwrap_pp_layer_off`, `unwrap_pp_verb_switch_stage`)
   - shared-role validation (`ptr_validate_shared_roles`)
5. **Classify** `ptr_classify_data(node)` — `R/paintr-classify.R:6-11` → dispatches `classify_walk` S3 methods to label `ptr_ph_data_consumer` vs `ptr_ph_data_source` roles.
6. **Assign ids** `ptr_assign_ids(node, ns_fn)` — `R/paintr-ids.R:19`. Every placeholder/consumer gets a deterministic id. **These ids are the contract between rendered UI bindings and runtime reactives.**
7. **Safety pass** `ptr_validate_tree_safety(node, expr_check)` — `R/paintr-safety.R:8`. Walks the AST with `walk_ptr_safety` (per-class S3 methods); composes with `validate_expr_safety` + `unsafe_expr_denylist` from `R/paintr-utils.R`.
8. **State stash** the verified `ptr_root` is stored in `state$tree()` (a reactive) by `ptr_init_state` — `R/paintr-server.R:84`. Every runtime observer reads from `state$tree()`.

**Invariant (ADR 0012):** `|>` / `%>%` / nested-call surfaces all produce the *same canonical tree*. The pipe shape is preserved as metadata, not as tree shape.

## Path B — tree → Shiny UI

Runs at UI-build time (the L2 user calls `ptr_ui(formula, id)` in their UI; L3 user calls bare `ptr_ui_*` pieces). The tree is rebuilt from the same formula (UI side cannot share the server's `state$tree()` reactive).

1. **L2 entry** `ptr_ui(formula, id, ui_text, …)` — `R/paintr-app.R:861`. Renders the default layout (controls + plot + error + code) wired together by the `ptr_ui_inline_error` / `ptr_ui_toggle_code` combinators.
2. **Controls panel** `ptr_ui_controls(formula, id, ui_text, …)` — `R/paintr-app.R:951`. The controls piece that takes the formula and renders all input widgets (plus the formula-local shared section in single-instance use).
3. **Tree → UI dispatch** `build_ui_for(node, …)` — `R/paintr-build-ui.R:32` is the S3 generic. Methods recursively descend the tree.
4. **Layer panel** `build_ui_for.ptr_layer(node, …)` — `R/paintr-build-ui.R:110`. Builds one `wellPanel`-style block per layer.
5. **Value placeholder shell** `build_ui_for.ptr_ph_value(node, …)` — `R/paintr-build-ui.R:47-64`. **Important:** emits *only* a `uiOutput(value_output_id(node$id))` placeholder shell. The actual `textInput`/`numericInput`/etc. is mounted server-side later (Path C/D).
   - Reason (ADR 0012 / PLAN-01 Bug B): late mounting via `invoke_build_ui` routes through one uniform `extra$selected` precedence hook that seeds initial values from `state$spec_seed[[raw_id]]`. The consumer placeholder has worked this way since day one; ADR 0012 generalised it to all placeholder kinds.
6. **Shared partition** `collect_shared_placeholders(tree)` — `R/paintr-build-ui.R:459`. Identifies which placeholders are shared vs formula-local so the controls piece can render only the formula-local ones.

**Invariant:** UI emits *shells*, not widgets. The widget identity is decided server-side, where `state$spec_seed` and the registry's `build_ui` hook live.

## Path C — Update click → reactive runtime trigger

The reactive boundary. The only entry point that runs the pure pipeline on each click.

`ptr_setup_runtime(state, input, output, session)` — `R/paintr-server.R:1743-1852`.

1. **Trigger gate** the inner `triggered()` reactive — `R/paintr-server.R:1754-1764`. Reads **three** sources every call (no `||` short-circuit, by design):
   - `clicked(input[[ns("ptr_update_plot")]])` — per-instance Update button
   - `clicked(state$draw_trigger())` — host-supplied (e.g. grid app's "Draw all")
   - `length(state$extras()) > 0L` — `ptr_gg_extra()` changes
   - **Why no short-circuit:** if the first is `TRUE`, `||` would short-circuit and the enclosing `observe` never establishes a dependency on the other two. Once a panel's button has fired, "Draw all" would stop redrawing it. Comment at line 1758-1762 explains this in source.
   - **Why not `observeEvent(..., ignoreInit=TRUE)`:** under `shiny::testServer`, the first `setInputs()` IS the observer's creation flush; `ignoreInit` would swallow it.
2. **Bail-if-no-trigger** `if (!triggered()) return(invisible())` — `R/paintr-server.R:1768`. Reads invalidate the observer; the bail returns without doing work.
3. **Snapshot loop** `shiny::isolate({ … for (i in seq_len(nrow(spec))) { snapshot[[raw_id]] <- input[[ns(raw_id)]] … } })` — `R/paintr-server.R:1770-1813`. Copies every widget's current value into a plain list. Inside the loop:
   - **Source-companion round-trip fallback** — `R/paintr-server.R:1777-1812` (comment 1777-1787, fallback if-block at `if (identical(spec$role[i], "source_companion")) { … }` 1788-1812). ADR 0025 §6: when the shortcut textInput is empty but a file is bound under an auto-name, overwrite the snapshot with the bound name so the spec dump records the auto-name. The textbox wins when non-empty (no override).
4. **Upstream cols** `upstream_cols <- runtime_upstream_cols(state, snapshot)` — `R/paintr-server.R:1818`. Deeply state-coupled (reads `state$tree()`, `state$resolved_data`, `state$upstream_cache`), so it stays here and is passed into the otherwise-pure headless step.
5. **Dispatch to pure pipeline** `res <- ptr_exec_headless(tree, snapshot, …)` — `R/paintr-server.R:1825-1838`. See Path D.
6. **Attach snapshot** `res$snapshot <- snapshot` — `R/paintr-server.R:1846`. So preserve-mode rendering can stamp `current_pick` from the same values final-mode substitute saw (ADR 0009 bug-1 follow-up, 2026-05-21).
7. **Publish to state** `state$runtime(res)` — `R/paintr-server.R:1849`. Writes into the reactiveVal that every output binding (Path F) reads.

**Invariant:** typing in a `ppText` widget does **not** invalidate `state$runtime()`. Only Update / Draw-all / `ptr_gg_extra()` changes do.

## Path D — snapshot → code + plot (the pure pipeline)

The deterministic core. Called by Path C step 5; can be called directly from R for unit tests (not exported).

1. **Entry** `ptr_exec_headless(tree, snapshot, …)` — `R/paintr-headless.R:22`. Threads its args into the two pure stages.
2. **Stage A — substitute + prune + render** `ptr_complete_expr_safe(node, snapshot, …)` — `R/paintr-safe.R:17-67`. Sequence inside the `tryCatch`:
   - `subbed <- ptr_substitute(node, input_snapshot = snapshot, …)` — `R/paintr-safe.R:25-32` → dispatches `substitute_walk` (`R/paintr-substitute.R:16`). See Path E.
   - `pruned <- ptr_prune(subbed, safe_to_remove, is_standalone)` — `R/paintr-safe.R:33-37` → dispatches `prune_walk` (`R/paintr-prune.R:18`). Drops stages where `isFALSE(stage_enabled[[sid]])`.
   - `code_text <- if (is_ptr_missing(pruned) || empty-root) "" else ptr_render(pruned)` — `R/paintr-safe.R:38-43` → calls `ptr_render` (`R/paintr-render.R:27-29`).
   - Returns `list(ok=TRUE, stage="complete", code_text, pruned, eval_env, plot=NULL, error=NULL, condition=NULL)` — `R/paintr-safe.R:44-53`.
   - On error: `ok=FALSE`, populated `error` (formatted by `ptr_format_runtime_message`), other fields zeroed — `R/paintr-safe.R:54-65`.
3. **Stage B — eval the plot** `ptr_assemble_plot_safe(result, expr_check)` — `R/paintr-safe.R:93`. Calls `ptr_eval` (`R/paintr-eval.R:9`) to turn the pruned tree into a language object, then `eval(…, eval_env)` to get a ggplot.
4. **Stage C — validate the plot can render** `ptr_validate_plot_render_safe(result)` — `R/paintr-safe.R:115`. Runs `ggplot_build` + `ggplot_gtable` to surface print-time errors (loess failures, `seq_len(-1)`, etc.) that wouldn't fire until the device tries to draw.
5. **Return** `res` — a single list with both `$code_text` and `$plot` populated from the **same** `pruned` tree.

**Invariant (code-panel parity):** the code shown in the Code panel and the plot shown in the Plot panel come from the same `pruned` tree on the same call. They cannot diverge by construction. See `04-invariants.md`.

## Path E — substitute hook (one placeholder's value → tree node)

The per-placeholder bridge between the Shiny input snapshot and the rendered/eval'd tree.

`substitute_walk.ptr_ph_value(node, ctx)` — `R/paintr-substitute.R:90-129`.

1. **Read value from snapshot** `value <- read_placeholder_value(node, ctx)` — `R/paintr-substitute.R:90`. Pulls `ctx$input_snapshot[[node$input_id]]`.
2. **Missing → drop** `if (is_missing_value_input(node, value)) return(ptr_missing())` — `R/paintr-substitute.R:91`. Returns a `ptr_missing` so the prune stage can drop the containing arg/layer.
3. **Registry lookup** `entry <- ptr_registry_lookup(node$keyword)` — `R/paintr-substitute.R:92`. Looks up the placeholder definition by keyword (`ppText`, `ppNum`, custom keyword, …). Errors with a helpful "register it with `ptr_define_placeholder_value`" if missing.
4. **Validate input** `if (!is.null(entry$validate_input)) entry$validate_input(value, hook_ctx)` — `R/paintr-substitute.R:101-118`. Accepts `TRUE` or `NULL` as valid. A character vector is the error message. Fails closed for any other return.
5. **Resolve to expression** `resolved <- entry$resolve_expr(value, node)` — `R/paintr-substitute.R:121`. For a built-in:
   - `ppText` → `ptr_builtin_text_resolve_expr` (`R/paintr-builtins.R:44-49`), which strips a matched outer-quote pair so the user typing `"hello"` and `hello` both produce the same string.
   - `ppNum`/`ppExpr`/`ppVar`/`ppUpload` each have their own `_resolve_*` hook.
6. **Wrap & emit**:
   - If `node$keyword == "ppExpr"` → `ptr_user_expr(resolved)` — `R/paintr-substitute.R:124-126`. Preserves "user wrote this, validate it before eval".
   - Otherwise → `ptr_literal(resolved)` — `R/paintr-substitute.R:127`. Plain value, no further safety needed.

**Custom placeholders plug in here.** A registry entry registered via `ptr_define_placeholder_value(keyword, build_ui, resolve_expr, …)` shows up at step 3 and 5 above; everything else is identical.

## Path F — output bindings (`state$runtime()` → DOM)

The reactive sinks. Each `ptr_register_*` registers one `output[[…]]` and reads from `state$runtime()`.

1. **Last-ok cache** `ptr_register_last_ok_cache(output, state)` — `R/paintr-server.R:3047`. Observes `state$runtime()`; whenever `res$ok` is `TRUE`, writes `res` into `state$last_ok_runtime` (a reactiveVal). Read by every output's retain-on-error fallback.
2. **Plot** `ptr_register_plot(output, state)` — `R/paintr-server.R:3058`. Sets `output[[ns("ptr_plot")]] <- shiny::renderPlot({ … })`:
   - If `res$ok && !is.null(res$plot)` → return `res$plot`.
   - Else fall back to `state$last_ok_runtime()$plot`.
   - Else `graphics::plot.new()` (blank device). The error panel shows the diagnostic separately.
3. **Error** `ptr_register_error(output, state)` — `R/paintr-server.R:3081`. Sets `output[[ns("ptr_error")]] <- shiny::renderUI({ … })`. Reads `state$runtime()$error`.
4. **Code** `ptr_register_code(output, state)` — `R/paintr-server.R:3138`. Sets `output[[ns("ptr_code")]] <- shiny::renderText({ … })`. Calls `format_code_with_extras(chosen, …, prologue = emit_upload_prologue(active_uploads))` on `state$runtime()$code_text` (or last-ok fallback). The upload prologue is what gives the code panel its `<name> <- read.csv("…")` header lines.

**Invariant (retain on error):** outputs never blank when a transient error fires; the user keeps the prior ok-result while the error panel shows the diagnostic.

## Path G — L3 custom render (escape hatch)

ADR 0006: there is one public server entry. To customize render, the L3 user does:

```r
state <- ptr_server(formula, id = "foo")               # paintr-app.R:1092
output$my_plotly <- plotly::renderPlotly({
  state$runtime()$plot                                  # reactive read of res$plot
})
```

`ptr_server` is at `R/paintr-app.R:1092`. The returned `state` is the same list described in `01-architecture.md`. Public helpers for the most common extractions (all three are one-liners wrapping `shiny::isolate(state$runtime()$…)`):

- `ptr_extract_plot(state)` — `R/paintr-server.R:3226`
- `ptr_extract_error(state)` — `R/paintr-server.R:3230`
- `ptr_extract_code(state)` — `R/paintr-server.R:3234-3239` (multi-line: also formats with `format_code_with_extras` + `emit_upload_prologue` to match what the Code panel shows)

**Don't do** the old "wrap a bare engine in your own moduleServer" pattern. The "bare engine" is now `ptr_server_internal` (`R/paintr-server.R:788`) which is **unexported** by design (ADR 0006). The L3 escape hatch is reading `state$runtime()`, not wrapping the internal engine.

## Path H — shared placeholder resolution (multi-instance only)

Single-instance apps never run this path (every shared key is formula-local by definition; the instance's own `ptr_server` binds it). Only multi-instance with `obj <- ptr_shared(formulas)` triggers it.

1. **Pure coordinator** `obj <- ptr_shared(formulas, shared_ui, ui_text, …)` — `R/paintr-shared-coordinator.R:220`. Returns a `ptr_shared_spec` (non-reactive).
2. **Partition** `shared_partition(trees)` — `R/paintr-shared-coordinator.R:28`. For each shared key, counts how many formulas reference it; returns `list(panel = …, formula_local = list(per_formula = …))`. Rule: ≥2 formulas → panel; exactly 1 → formula-local section.
3. **Id rewrite (P3)** `ptr_shared_bind(node)` — `R/paintr-shared.R:6-11`. Collects every placeholder with `shared = "<key>"`, groups by key, and rewrites every member's `id` to `canonical_shared_id(key)` (= `paste0("shared_", key)`) — `R/paintr-shared.R:35`. After this pass, sibling placeholders for the same shared key all bind to the same `input[[…]]`.
4. **Host-side reactive bundle** `ptr_shared_server(obj, …)` — `R/paintr-shared-ui.R:232`. Builds `ptr_shared_state` (reactives for the panel-shared sources/values), wires the top-level consumer pickers, and returns the bundle for the embedder to thread into each `ptr_server(..., shared_state = …)`.
5. **Host-side panel-source/value setup** `ptr_setup_panel_sources` / `ptr_setup_panel_values` — `R/paintr-shared-ui.R` (search those names). The host's `ptr_shared_server` binds **only** panel keys; formula-local consumers' column pickers remain owned by each instance's own `ptr_server` (ADR 0023).
6. **Per-instance shared-consumer UI** `ptr_bind_shared_consumer_uis(output, input, ns, …)` — `R/paintr-server.R:2578`. Wires the shared consumer's column-picker `uiOutput` shells (under the host's namespace for panel keys, under the instance for formula-local keys).
7. **Consumer-resolution at substitute time** `ptr_resolve_shared_consumers(trees)` — `R/paintr-shared.R:320-324`. Resolves shared consumers' column-space to the upstream source they reference.

**Invariant (ADR 0023, partition decides ownership):** the host server owns *only* panel keys; the instance owns *only* its formula-local keys. No double-binding, no cross-talk.

## Path I — upload flow + source mutex (ADR 0025)

The most subtle reactive choreography in the package: a `ppUpload` placeholder is a *source* (it provides a data frame for consumers downstream), and the user has two ways to fill it — pick a file via `fileInput` or type a name via the sibling shortcut `textInput`.

1. **UI shell** `ptr_builtin_upload_build_ui(node, label, copy, …)` — `R/paintr-builtins.R:229`. Emits the `fileInput` plus a sibling shortcut `textInput` (the source-companion row).
2. **Mutex wiring** `ptr_bind_source_mutex(src_id, shortcut_input_id, input, session, shortcut_r)` — `R/paintr-server.R:1547-1585`. Two observers:
   - **File picked → wipe textbox** `observeEvent(input[[src_id]], { updateTextInput(session, shortcut_input_id, value = "") })` — `R/paintr-server.R:1550-1552`.
   - **Text typed → reset fileInput** via `session$sendCustomMessage("ptr_reset_file_input", …)` (custom JS in `inst/www/ggpaintr-layer.js`) — `R/paintr-server.R:1578-1582`.
   - **The race fix (ADR 0025 §7 A2)**: when `shortcut_r` (the 400ms-debounced reactive used by `resolve_upload_source`) is supplied, the text observer gates on `shortcut_r()` instead of `input[[shortcut_input_id]]`. Both observers then fire on the same debounced tick, letting `resolve_upload_source` bind the uploaded df under `<name>` **before** the JS file-reset round-trip blanks `input[[src_id]]`. Without this, the bind sees `file=NULL` and falls back to walking the parent chain (`inherits=TRUE`) — which could bind `palmerpenguins::penguins` instead of the uploaded df. Comments at `R/paintr-server.R:1562-1573` document the race in detail.
3. **Resolve upload** `ptr_builtin_upload_resolve_data(value, node, …)` — `R/paintr-builtins.R:254-256`. The registry hook called by `substitute_walk.ptr_ph_data_source`. Dispatches the extension to a reader:
4. **Read uploaded file** `ptr_read_uploaded_data(file_info)` — `R/paintr-upload.R:28`. Routes to `ptr_read_csv_upload`, `ptr_read_tsv_upload`, `ptr_read_rds_upload`, `ptr_read_excel_upload`, or `ptr_read_json_upload` via `reader_fn_name_for_ext`.
5. **Register active upload** `register_active_upload(state, key, node, file_info)` — `R/paintr-server.R:912`. Caches the resolved frame in `state$resolved_data[[key]]` so the eval step can skip re-reading. Also publishes a name into `state$bound_names[[key]]` so consumers (and the Code panel prologue) can refer to the upload by name.
6. **Vacate on un-pick** `vacate_source_binding(state, key)` — `R/paintr-server.R:985`. Cleans up `state$resolved_data` / `state$bound_names` when the file is reset.
7. **Code-panel prologue** `emit_upload_prologue(active_uploads)` — `R/paintr-server.R:1010`. Builds the `<name> <- read.csv("…")` header lines that `ptr_register_code` prepends to the rendered code.

**Invariant:** one source key has at most one bound name at a time. The textbox is the source of truth when non-empty; the file's auto-name fills in when the textbox is empty (ADR 0025 §6 round-trip fallback at `R/paintr-server.R:1781-1813`).

## Re-verification commands

Any of these traces stale? Re-anchor with one command:

```sh
grep -nE "^(<fn_name>) <- function" R/paintr-*.R
```

For symbol bodies:

```r
# In the orchestrator session (Serena bound to this project root):
mcp__serena__find_symbol(name_path_pattern = "<fn>", relative_path = "R/paintr-<file>.R", include_body = TRUE)
```

Last full re-anchor: **2026-05-27**.
