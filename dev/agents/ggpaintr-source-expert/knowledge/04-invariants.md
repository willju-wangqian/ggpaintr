# Invariants — load-bearing assumptions

The things the code *depends on being true*. Violating any of these breaks something user-visible. Each is grounded in source (with `file:line`) or in a locked ADR. When a question asks "can I rely on X?" — this is the file to consult.

The first section is the highest-stakes group: violating these silently breaks correctness rather than throwing an error.

## I. Determinism & code-panel parity

### I.1 — Code panel and plot derive from the *same* pruned tree
`ptr_complete_expr_safe` (`R/paintr-safe.R:17-67`) builds `pruned` once via `ptr_substitute` → `ptr_prune`, then renders `code_text <- ptr_render(pruned)` on that same node. `ptr_assemble_plot_safe` (`R/paintr-safe.R:93`) then takes the same `result$pruned` and `eval()`s it. **They cannot diverge by construction** — there is no separate render path for the code panel.

Implication for L3: if you read `state$runtime()$code_text` and `state$runtime()$plot` from the same reactive value, they describe the same render. If you read them in two separate reactive contexts that re-fire at different times, you may observe one tick of skew but not a structural disagreement.

### I.2 — Tree canonicality (ADR 0012)
`|>` / `%>%` / nested-call surface syntaxes all produce the *same* canonical `ptr_root`. The translator preserves pipe shape as metadata (`is_pipe_head`, stage indices) on tree nodes, not as the tree's structural shape. `ptr_tree_structural_equal` (in `R/paintr-nodes.R`) is the comparator that proves this — it excludes `stage_id` / `default_active` / `default_stage_enabled` / `has_user_control` / `stage_label` / `op` / `expr` (locked 2026-05-24 per ADR 0021).

### I.3 — `res$snapshot` enables preserve-mode parity
`ptr_setup_runtime` attaches `res$snapshot <- snapshot` at `R/paintr-server.R:1846` before `state$runtime(res)`. Preserve-mode rendering (`stamp_current_pick_walk` in `paintr-render.R`) reads this so it stamps `current_pick` from the same values final-mode substitute saw. **Don't strip the snapshot field** when manipulating `res` (ADR 0009 bug-1 follow-up, 2026-05-21).

## II. Runtime triggering

### II.1 — Update click is the sole trigger of the pure pipeline
`ptr_setup_runtime` (`R/paintr-server.R:1743-1852`) is the *only* observer that runs `ptr_exec_headless` and writes `state$runtime()`. Its `triggered()` reactive reads three sources (`R/paintr-server.R:1754-1764`):

- per-instance Update button `input[[ns("ptr_update_plot")]]`
- host-supplied `state$draw_trigger()` (e.g. grid app's "Draw all")
- `state$extras()` (changes to `ptr_gg_extra()`)

**Typing in a widget changes `input[[id]]` but does NOT invalidate `state$runtime()`.** The runtime is a debounced batch, not a live trace.

### II.2 — Three triggers, NO short-circuit
The `triggered()` function reads all three reactives every call. **Do not** rewrite as `update_clicked || draw_clicked || extras_present` with naive short-circuit. Once one trigger is `TRUE`, `||` short-circuits and the enclosing `observe` never establishes a reactive dependency on the others — so once a panel's Update button has fired, subsequent "Draw all" clicks (and `ptr_gg_extra()` changes) stop redrawing it. Source comment at `R/paintr-server.R:1758-1762`.

The current code keeps `||` for readability but reads each input *before* the chain (`update_clicked <- …`, `draw_clicked <- …`, `extras_present <- …` at lines 1761-1763), so the reactivity registers on all three.

### II.3 — `ptr_setup_runtime` cannot be rewritten as `observeEvent(..., ignoreInit = TRUE)`
Under `shiny::testServer`, the first `setInputs()` IS the observer's creation flush — `ignoreInit` would swallow it. The current `observe` + explicit `triggered()` guard is the working pattern. Source comment at `R/paintr-server.R:1750-1753`.

### II.4 — Outputs retain on error
`ptr_register_plot` (`R/paintr-server.R:3058`), `ptr_register_code` (`R/paintr-server.R:3138`), and `ptr_register_error` (`R/paintr-server.R:3081`) all fall back to `state$last_ok_runtime()` when `res$ok` is `FALSE`. The plot does NOT blank on a transient error; the error panel surfaces the diagnostic separately. The last-ok cache is owned by `ptr_register_last_ok_cache` (`R/paintr-server.R:3047`).

## III. Tree semantics

### III.1 — `stage_id` is a UI-routing key, NOT execution-shape (ADR 0021)
`disable_walk` (`R/paintr-disable.R`) drops a stage iff `isFALSE(stage_enabled[[sid]])`. **Unbound reactives are runtime no-ops** — a `ptr_call` with a `stage_id` that has no bound `stage_enabled[[sid]]` reactive runs unconditionally. `stage_id` differs between two trees only when "does this stage have a UI checkbox?" differs. Tree structural equality (`ptr_tree_structural_equal`) therefore excludes `stage_id`.

This invariant is the basis for Plan-03 SC7's verb-side structural-equality invariant: a tree with a UI affordance and a tree without one are structurally equal for canonicality purposes.

### III.2 — Partition decides ownership, not just placement (ADR 0023)
For multi-instance apps with `obj <- ptr_shared(formulas)`:

- a shared key referenced in exactly **one** formula → that formula's inline shared section; owned by **that one instance's `ptr_server`**, bound at the instance namespace
- a shared key referenced in **≥2** formulas → the one standalone `ptr_shared_panel`; owned by the **host's `ptr_shared_server(obj)`**, bound at the coordinator namespace
- the host server never reaches into a formula-local key
- the instance never reaches across formulas

This applies to **every** shared placeholder kind — value, consumer, source. A formula-local `var(shared = "x")` consumer's column picker is bound by its owning instance; the panel server does not touch it. (This was a real bug fixed via partition discipline; see auto-memory `project-shared-section-binder`.)

### III.3 — Single-instance has no coordinator
With exactly one `ptr_app` / `ptr_server`, no `ptr_shared` is built and no panel is rendered. Every shared key is formula-local by definition; the inline shared section in the controls piece renders them all. **Promote-to-panel for prominence was considered and rejected** (CONTEXT.md, locked 2026-05-16).

### III.4 — Tree contract between phases
After `ptr_translate` the tree is unannotated. After `classify_walk` it has role labels. After `ptr_assign_ids` every placeholder/consumer has a deterministic `id`. After `walk_ptr_safety` it is verified. **Every runtime observer assumes the tree has been through all four passes.** `ptr_walk.R` exposes assertions (`ptr_assert_ids_assigned`, `ptr_assert_classified`, etc.) so a downstream phase can prove its precondition.

## IV. Safety

### IV.1 — Two safety layers, walker is primary
ggpaintr accepts user-written R expressions inside `ppExpr`. Two independent defenses:

- **Tree-level safety** — `walk_ptr_safety` (`R/paintr-safety.R:8`) runs at translate time on the AST.
- **Expression-level safety** — `validate_expr_safety` (`R/paintr-utils.R`) runs at substitute time on each user expression. It uses an *AST walker* that recurses through call heads, pairlist arg names, lambda bodies, and **string literals** (`eval(parse(text="…"))` smuggling), checking each symbol/string against `unsafe_expr_denylist`.

**The walker is the primary defense, not the denylist.** The denylist is ~151 entries (auto-memory `project-denylist-complete`); it is considered complete. **Do not propose adding more entries unless you have a concrete, reproducible bypass.** The walker's recursive descent into all AST node kinds is what makes the safety scheme work — the denylist is just the manifest. R is too open to enumerate exhaustively; trust the walker.

### IV.2 — `eval()` runs in a sealed env
`state$eval_env` (created in `ptr_init_state`, `R/paintr-server.R:84`) is scoped to ggplot2 + the snapshot + the package, not `globalenv()`. The constant-fold registry (`R/paintr-default-args.R`) handles the special case of validating `pp*(default = …)` args at translate time without invoking the runtime eval env.

### IV.3 — UI emits shells, widgets mount server-side (ADR 0012 / PLAN-01 Bug B)
`build_ui_for.ptr_ph_value` (`R/paintr-build-ui.R:47-64`) emits **only** a `uiOutput(value_output_id(node$id))` container. The actual `textInput`/`numericInput`/etc. is mounted by `ptr_setup_value_uis` (`R/paintr-server.R:2067`) via `invoke_build_ui`. This routes all initial-value seeding through one uniform `extra$selected` hook that reads from `state$spec_seed[[raw_id]]`. **Custom placeholders that emit a widget directly in `build_ui` bypass spec-apply** — they won't honor a boot-time `spec=` snapshot. Always emit the widget via the registry hook + uiOutput shell.

## V. Public API surfaces

### V.1 — One public server entry (ADR 0006)
`ptr_server` (`R/paintr-app.R:1092`) is the **single public server function**. It is used at L2 *and* L3. The 4-arg internal engine is `ptr_server_internal` (`R/paintr-server.R:788`), **unexported by design** and not part of the L1/L2/L3 surface. Do not recommend `ptr_server_internal` to users; do not wrap it in your own `moduleServer`.

### V.2 — L3 is UI-side only (ADR 0006)
There is no L3 server pattern. L3 = compose bare `ptr_ui_*` pieces in the UI, then optionally extract from `state$runtime()` for custom render. The custom-render escape hatch is reading the reactive value, not authoring server code.

### V.3 — No headless / non-Shiny path
`R/paintr-headless.R` exists, but none of its symbols are in `NAMESPACE`. Despite the suggestive name, `ptr_exec_headless` is **internal plumbing**, not a public batch-execution API. The post-rewrite ggpaintr has no public non-Shiny path (auto-memory `project-headless-removed`).

### V.4 — Naked-R semantics
Every `pp*` placeholder function is **identity on its first arg**. A formula like `mpg ~ wt + ppText("y_lab")` reads identically in or out of ggpaintr — outside, `ppText("y_lab")` returns `"y_lab"`; inside, the translator special-cases the call. The same applies to `ppLayerOff(...)` (returns the inner expression; hidden layers drop) and `ppVerbSwitch(verb, switch_on=FALSE, label="…")` (drops the verb when off-by-default and ignores the UI-only `label` arg).

### V.5 — `state$server_ns_fn` and `state$ui_ns_fn` are internal
These fields exist on the `state` return value of `ptr_server`, but they are **not** documented as user-facing escape hatches (ADR 0006). The intended L3 escape is `state$runtime()`. Do not recommend `state$server_ns_fn` in user-facing prose.

## VI. Registry & extensibility

### VI.1 — `.ptr_registry` is process-global
`.ptr_registry` (`R/paintr-registry.R`) is an environment at the **package level**, populated at `.onLoad`. It is **not** per-session, per-`ptr_app`, or per-`ptr_server`. A custom placeholder registered with `ptr_define_placeholder_value("myKw", …)` persists across every subsequent app launch in the same R process. To remove one: `ptr_clear_placeholder("myKw")`. (Auto-memory `project-placeholder-registry-global`.)

### VI.2 — Registry init order matters (ADR 0014)
`ensure_registry_initialized` (`R/paintr-registry.R`) is the shim that handles the case where `.ptr_registry` is consulted *before* `.onLoad` has populated it (e.g. during the package's own load cycle). Custom placeholders defined inside other packages' `.onLoad` may race with ggpaintr's — the shim resolves it.

### VI.3 — Custom placeholder hook signature uses `node$id`
A registry hook (`build_ui`, `resolve_expr`, `validate_input`, `resolve_data`) receives `node` as a `ptr_ph_*` object. **The id lives at `node$id`** (after `ptr_assign_ids` runs). The hook signature changed during the API rewrite — older custom-placeholder code that expected a flat `id` arg needs to be updated. (Auto-memory `project-placeholder-registry-global`.)

### VI.4 — `pp*` keyword names obey ADR 0009 / 0010
The user-facing wrappers are `ppText`, `ppNum`, `ppExpr`, `ppVar`, `ppUpload`, `ppLayerOff`, `ppVerbSwitch`. The pre-rename keywords (`var`, `text`, `num`, `expr`, `upload`) no longer parse — they were superseded on the `vignette-review` branch (commit `2ee656b`, 2026-05-21; auto-memory `project-adr9-merged`).

## VII. Subtle gotchas

### VII.1 — Pipe at the top of a formula needs unwrapping
`ptr_capture_formula` (`R/paintr-app.R:196`) unwraps a top-level `{ … }` block so `formula |> ptr_app()` works. Without the unwrap, the captured expression is `{ formula |> ptr_app() }`, and the translator sees the whole pipe instead of the formula. Regression test added in commit `d5dbd2c`.

### VII.2 — Source-companion round-trip fallback (ADR 0025 §6)
Inside `ptr_setup_runtime`'s snapshot loop (`R/paintr-server.R:1777-1812` — comment + fallback if-block), source-companion rows (the shortcut textInput sibling of a `ppUpload` source) get overwritten with the file's auto-name when the textbox is empty *and* a name is bound under `state$bound_names[[key]]()`. **The textbox wins when non-empty.** A boot-2 with `spec=` then seeds the textbox with this name; the consumer's `<name> <- read.csv("…")` prologue (Plan 04) binds the frame under it; the env-shortcut resolves and reproduces the rendered plot.

### VII.3 — Source mutex requires the debounced reactive (ADR 0025 §7 A2)
`ptr_bind_source_mutex` (`R/paintr-server.R:1547-1585`) takes an optional `shortcut_r` arg — the 400ms-debounced reactive that drives `resolve_upload_source` in `ptr_setup_pipelines`. **Without it**, the text-observer fires on every keystroke; the JS file-reset round-trip blanks `input[[src_id]]` *before* the bind observer runs; the bind sees `file=NULL` and walks the parent chain (`inherits=TRUE`) — which can bind `palmerpenguins::penguins` instead of the uploaded df. The race is documented in detail at `R/paintr-server.R:1562-1573`. Always pass `shortcut_r` when wiring the mutex on an upload-bound source.

### VII.4 — htmlDependency does NOT auto-register `addResourcePath`
The asset bundle (`core_assets_dep` in `R/paintr-build-ui.R`) uses `htmlDependency`, but `htmlDependency` does NOT register the `addResourcePath("ggpaintr", …)` prefix that legacy code expects. **Absolute-path asset refs (e.g. the header logo at `/ggpaintr/header-logo.svg`) break** unless an explicit `addResourcePath` call is kept. The current code preserves the explicit registration; do not "simplify" it away. (Auto-memory `htmldependency-resource-prefix`.)

### VII.5 — `shiny::debounce` + paired-observer race
Wrapping an `input[[id]]` in `shiny::debounce` inverts flush order vs. any sibling raw-input observer (e.g. a mutex's JS round-trip). When you debounce an input, **audit every observer on the same id** — if a sibling observer reads the raw input synchronously, you have a race. (Auto-memory `shiny-debounce-mutex-race`; this is the lesson behind ADR 0025 §7 A2.)

### VII.6 — `lapply(x, bare-namespace-generic)` skips S3 dispatch
Calling `lapply(nodes, paintr:::prune_walk)` (or any bare-namespace reference) **does not dispatch on the node's class** — the bare name is the generic, not the method, so S3 lookup misses. Use `lapply(nodes, function(n) prune_walk(n))` or call methods explicitly. (Auto-memory `pipeline-head-data-source`; this bit the `paintr-prune.R` mid-pipeline `upload` resolution in 2026-05.)

### VII.7 — Registry env can split under raw `test_file`/`test_dir`
`devtools::load_all()` can split `.ptr_registry` across package/namespace envs when used in some test contexts. Probes that mirror fixtures' `pkgload::load_all()` workflow avoid this. **For the test gate, use `devtools::test()`** — it heals the split by running each file in a clean process. Bare `Rscript -e 'testthat::test_file(…)'` may show registry-state failures that don't reproduce under `devtools::test()`. (Auto-memory `feedback-harness-test_file-vs-devtools-test`.)

### VII.8 — Test gate: `NOT_CRAN=true` is required for browser e2e
`devtools::test()` sets `NOT_CRAN=true` itself, so the browser fixtures run. Bare `Rscript -e 'testthat::test_dir(…)'` *without* `NOT_CRAN` silently SKIPs every shinytest2 test (the `skip_on_cran()` guard fires). The harness gate is `NOT_CRAN=true Rscript -e 'devtools::load_all(…); testthat::test_dir(…)'`; the only acceptance is **FAIL 0 / ERROR 0 / SKIP 0 / PASS N**. (CLAUDE.md "Authoritative gate"; auto-memory `shinytest2-appdir-pkgload`.)

## How to add a new invariant

Promote a finding to this file when **all three** are true:

1. It is durable (would still be true after a major refactor).
2. It is non-obvious (a reader of the code would have to chase several files to see why).
3. Violating it would silently break something user-visible (not just throw an error).

Findings that are "just bugs" go to the issue tracker, not here. Findings that are specific to a vignette or test go to `06-pitfalls.md` (when that file exists).
