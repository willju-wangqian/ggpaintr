# Architecture — how the pieces fit together

This file is the prose anchor: read it once and you know how ggpaintr is wired. It complements (not duplicates) `00-module-map.md` (per-file roles) and `02-key-paths.md` (annotated traces with line numbers). When a claim is load-bearing, it is grounded in `CONTEXT.md`, an ADR, or the module map; verify against source on demand.

## The two halves

ggpaintr is two halves stitched together by one data structure (the `ptr_root` tree):

```
┌────────────────────────────┐    ┌──────────────────────────────┐
│  Half A: Formula machinery │    │  Half B: Shiny runtime       │
│  (pure functions on trees) │    │  (reactives, observers, IO)  │
│  R/paintr-{translate,      │    │  R/paintr-server.R           │
│  nodes, walk, classify,    │    │  (+ app, app-bslib,          │
│  ids, safety, substitute,  │    │   build-ui, shared-*)        │
│  render, prune, eval,      │    │                              │
│  resolve, default-args}.R  │    │                              │
└──────────────┬─────────────┘    └──────────────┬───────────────┘
               │                                 │
               └────────────────┬────────────────┘
                          ptr_root tree
                  (annotated with stage_id, ids,
                   classifications, safety verdict)
```

Half A is referentially transparent: same formula in → same tree out. Half B owns all Shiny state: a `state` list holding reactives, observers, and the **runtime reactive** that recomputes when the user clicks Update.

This split is what makes ggpaintr testable: Half A is pure and unit-testable; Half B's pure work delegates to `ptr_exec_headless` → `ptr_complete_expr_safe` → `ptr_assemble_plot_safe`, which take a snapshot and a tree and return a list. The reactives are just plumbing around that pure core.

## L1 / L2 / L3 — the embedding model

Locked in `CONTEXT.md` (ADR 0005 amended by ADR 0006). Three levels of API surface, layered from convenient → flexible:

- **L1 — all-in-one.** `ptr_app(formula)` / `ptr_app_grid(formulas)`. User writes a formula, calls one function, gets a running Shiny app. Internally these compose the same pieces an L3 user would compose by hand.
- **L2 — embed with the default layout.** Drop ggpaintr into your own Shiny app via the self-contained pair `ptr_ui(id, formula, …)` + `ptr_server(formula, id)` (formerly `ptr_module_ui`/`ptr_module_server`). For multi-plot pages: `ptr_shared(…)` + `ptr_shared_ui(…)` + `ptr_shared_server(obj)` + `ptr_shared_panel(obj)`. L2 owns its own asset bundle and `.ptr-app` scope.
- **L3 — own the layout (UI-side only).** Compose bare `ptr_ui_*` pieces (`ptr_ui_plot`, `ptr_ui_code`, `ptr_ui_error`, `ptr_ui_controls`, `ptr_ui_header`, `ptr_ui_shared_panel`) yourself; wire combinators (`ptr_ui_inline_error`, `ptr_ui_toggle_code`) for the standard chrome behaviors. **The server is still `ptr_server` — there is no L3 server.** Custom render = extract `state$runtime()$plot` (or `$code_text`, `$error`) from `ptr_server`'s return value and route it into your own widget (e.g. `plotly::renderPlotly`).

**Naming convention** (LOCKED 2026-05-16): `ptr_<x>` = L2 self-contained; `ptr_ui_<x>` = L3 bare piece. The `ptr_embed_*` idea is dead. There is **no headless / non-Shiny path** — L3 custom render is the only escape hatch, and it still runs inside Shiny.

## Half A — formula → tree → code/plot

The translation pipeline is a sequence of tree-walks on the `ptr_root` AST. Each walk has one job; the AST shape is the contract between them. Node types are defined in `paintr-nodes.R` (see module map).

```
formula string
   │
   │  ptr_capture_formula (paintr-app.R)
   │      • unwraps top-level `{ … }` so `|> ptr_app()` works (commit d5dbd2c)
   ▼
language object (substituted, not yet typed)
   │
   │  ptr_translate (paintr-translate.R) — the parser
   │      • lift to ptr_pipeline if any `|>` / `%>%` / native-pipe sentinel found
   │      • desugar pipes to nested calls; resugar stages; dedupe layer names
   │      • detect pp* placeholder calls → ptr_ph_value/consumer/source nodes
   │      • unwrap structural keywords (ppLayerOff, ppVerbSwitch) into metadata
   │        on the carrier ptr_layer / ptr_call (wrapper never appears in tree)
   │      • validate shared roles (collect_shared_occurrences)
   ▼
ptr_root            ← initial AST, not yet annotated
   │
   │  classify_walk (paintr-classify.R)
   │      • classifies ptr_ph_data_consumer vs ptr_ph_data_source roles
   │      • walks ptr_root → ptr_layer → ptr_pipeline → ptr_call children
   ▼
ptr_root classified
   │
   │  ptr_assign_ids (paintr-ids.R)
   │      • every placeholder/consumer gets a deterministic id
   │      • these ids are the contract between rendered UI and runtime reactives
   ▼
ptr_root classified + id-bearing
   │
   │  walk_ptr_safety (paintr-safety.R) — tree-level safety
   │      • composes with the AST-walker denylist in paintr-utils.R
   │      • verifies no smuggled unsafe constructs in user-supplied AST
   ▼
ptr_root (safety-verified) — stored on `state`, used by every observer
```

### The pure render pipeline (called by every Update click)

`ptr_exec_headless(tree, snapshot, …)` in `paintr-headless.R` is the pure-functional entry from the runtime into Half A. Despite the file name, it is **not a public headless API** (see `project-headless-removed` memory and `00-module-map.md`). It is the runtime's deterministic core:

```
ptr_exec_headless(tree, snapshot, expr_check, safe_to_remove, is_standalone, …)
   │
   │  ptr_complete_expr_safe (paintr-safe.R)
   │      • ptr_substitute (paintr-substitute.R): replace ph_value/consumer/source
   │                                              with ptr_literal nodes, using
   │                                              snapshot[input_id] → resolve_expr hook
   │      • ptr_prune (paintr-prune.R): drop stages disabled by isFALSE(stage_enabled[[sid]])
   │      • ptr_render (paintr-render.R): tree → code string (with preserve-mode
   │                                       stamp_current_pick_walk so displayed code
   │                                       matches displayed plot)
   ▼
res = list(ok, stage, code_text, pruned, eval_env, …)
   │
   │  ptr_assemble_plot_safe (paintr-safe.R)
   │      • ptr_eval (paintr-eval.R): pruned tree → language object
   │      • eval(lang_obj, state$eval_env) → ggplot
   │      • ptr_validate_plot_render_safe: ggplot_build/gtable to surface print-time errors
   ▼
res with $plot field populated
```

**Key invariant:** the `code_text` shown in the Code panel and the `plot` shown in the Plot panel come from the **same** `pruned` tree on the **same** call. They cannot diverge. (See `04-invariants.md`.)

## Half B — Shiny runtime

The runtime is one big function, `ptr_server_internal` in `paintr-server.R` (called by every public `ptr_server` and by `ptr_app`/`ptr_app_grid` via `ptr_make_app_server`). It wires up the reactive graph and returns `state`.

### State shape (built by `ptr_init_state`)

`state` is a list of reactives, reactiveVals, observers, and namespacing helpers. The fields the rest of the system depends on:

- `state$tree` — the safety-verified `ptr_root` (Half A's output, frozen for this `ptr_server` instance)
- `state$snapshot` — captured by `ptr_setup_runtime` on each Update click
- `state$runtime` — the **single reactiveVal holding the result of the most recent pure run** (`{ok, code_text, plot, error, …}`). This is the L3 escape hatch.
- `state$last_ok_runtime` — retain-on-error cache so a transient error doesn't blank the plot
- `state$eval_env` — sealed env in which `eval()` runs
- `state$server_ns_fn` / `state$ui_ns_fn` — internal namespacing (NOT public — see ADR 0006)
- `state$stage_enabled[[sid]]` — per-stage reactive checked by `is_stage_disabled` during prune

### Observer choreography

`ptr_server_internal` sets up observers in a specific order. Each `ptr_setup_*` function owns one slice:

```
ptr_setup_pipelines           → source-mutex (ADR 0025), producer inputs
ptr_setup_producer_inputs     → upstream-data producers feeding consumer pickers
ptr_setup_stage_enabled       → per-stage UI checkboxes (ppVerbSwitch)
ptr_setup_shared_stage_enabled
ptr_setup_runtime             ← the Update-click observer; runs the pure pipeline
ptr_setup_value_uis           → text/num/expr placeholder widgets (mounted via renderUI)
ptr_setup_source_uis          → upload/data-source widgets
ptr_setup_consumer_uis        → var(shared=) and consumer pickers
ptr_bind_shared_consumer_uis  → host-scoped shared-consumer widgets
ptr_setup_layer_picker        → "which layer is in front" picker
ptr_setup_layer_panel_classes → highlight active layer panel
ptr_register_last_ok_cache    → snapshot the most recent ok-result
ptr_register_plot             → output[[ns("ptr_plot")]] <- renderPlot
ptr_register_error            → output[[ns("ptr_error")]] <- ...
ptr_register_code             → output[[ns("ptr_code")]] <- ...
```

Two important properties:

1. **Update click is the only trigger that runs the pure pipeline.** Typing in a `ppText` widget changes `input[[id]]` but does *not* invalidate `state$runtime()` directly. `ptr_setup_runtime` reads `input[[ns("ptr_update_plot")]]` (and a small set of always-redraw triggers like `draw_trigger`/`extras`); only those invalidate. This is by design (debounced redraw → less flicker / less unfinished-state error spam).
2. **Output bindings retain on error.** When the most recent `res$ok` is FALSE, the plot output falls back to `state$last_ok_runtime()$plot` (so the user keeps a stable comparison while the error panel shows the diagnostic). Same for code.

### The L3 escape hatch (custom render)

ADR 0006: there is one public server entry. To customize render, the L3 user does:

```r
state <- ptr_server(formula, id = "foo")            # inside their host server
output$my_plotly <- plotly::renderPlotly({          # in their UI module
  state$runtime()$plot                              # reactive read
})
```

That's the entire L3 server-side story. `state$server_ns_fn` and `state$ui_ns_fn` are internal plumbing, **not** documented as user-facing escape hatches.

## Shared placeholders & the shared coordinator

ADRs 0005 / 0006 / 0023 / 0025 govern this surface. The partition rule (locked 2026-05-16):

> A shared key referenced in **exactly one** formula → that formula's **shared section** (inline, formula-local, no coordinator).
> A shared key referenced in **≥2** formulas → the one **shared panel** (standalone, cross-formula, coordinator-namespaced).

Two paths:

- **Single instance** (one `ptr_app` / one `ptr_server`). Every shared key is formula-local by definition. **No coordinator, no panel.** The instance's own `ptr_server` binds every shared widget; the controls piece renders them in an inline section.
- **Multiple instances.** The user *must* build `obj <- ptr_shared(formulas = list(f1, f2, …))`. The coordinator computes the partition and is the single source of truth. Each instance's own server binds its formula-local keys; the host's `ptr_shared_server(obj)` binds **only** the panel keys.

**The partition decides ownership, not just placement** — including for a `var(shared = …)` consumer's column picker. The host server never reaches into a formula-local key; the formula-local owner never reaches across formulas. One key, one owner. (See `feedback ledger` in `04-invariants.md`.)

**File-naming pitfall.** The three `shared` files are misleadingly named — go by what they hold, not by the suffix:
- `paintr-shared.R` owns the **P3 AST rewrite** (`ptr_shared_bind`, `canonical_shared_id`, `ptr_resolve_shared_consumers`) — id rewriting that maps every shared occurrence to one canonical id.
- `paintr-shared-coordinator.R` owns the **pure coordinator + panel UI**: `ptr_shared` (constructor), `shared_partition` (rule), `ptr_shared_panel` / `ptr_ui_shared_panel` (panel UI).
- `paintr-shared-ui.R` owns the **host-side server** (despite the `-ui` suffix): `ptr_shared_server`, the `ptr_shared_state` value type, `ptr_setup_panel_sources`, `ptr_setup_panel_values`.

## The `pp*` keyword family — two roles, same registry

The translator routes `pp*` calls two different ways, but both come from the same process-global registry (`.ptr_registry` in `paintr-registry.R`, populated at `.onLoad` via `ptr_register_builtins`):

- **Placeholders** (`role = "value" | "consumer" | "source"`): `ppText`, `ppNum`, `ppExpr`, `ppVar`, `ppUpload`. Each becomes a `ptr_ph_*` node in the tree and a Shiny widget on the screen. Naked-R semantics: each `pp*` function is **identity** on its first arg, so the formula renders out-of-ggpaintr too.
- **Structural keywords** (`role = "structural"`): `ppLayerOff`, `ppVerbSwitch`. The translator's *special-unwrap* branches recognise the wrapper, translate the inner expression as a normal layer/stage, and stamp metadata on the resulting carrier node. The wrapper itself never appears in the typed tree.

The **formula is the canonical record** of what the app shows at boot — placeholder initial values (`ppText(initial = "hi")`), layer-checkbox boot state (`ppLayerOff(..., hide = TRUE)`), stage-checkbox existence + boot state + label (`ppVerbSwitch(verb, switch_on = FALSE, label = "…")`). The same formula reads the same in or out of ggpaintr.

ADR 0021 (2026-05-24): `stage_id` is a UI-routing key, not execution-shape. `disable_walk` drops a stage iff `isFALSE(stage_enabled[[sid]])` — unbound reactives are no-ops. So `stage_id` differs between two trees only on "does this stage have a UI checkbox?" — pure UI affordance metadata. `ptr_tree_structural_equal` excludes it.

## Safety model — two layers

ggpaintr accepts user-written expressions inside `ppExpr` and (formerly) user-supplied tree fragments. Two independent layers defend against malicious input:

1. **Tree-level safety** (`walk_ptr_safety` in `paintr-safety.R`). Walks the AST; every node class has a method that asserts the node's contents are within the allowed shape. Run once per translate.
2. **Expression-level safety** (`paintr-utils.R::validate_expr_safety` + `unsafe_expr_denylist`). When `ppExpr` resolves a user-supplied string into a language object, an AST walker recurses through every node — call heads, pairlist arg names, lambda bodies, string literals — and blocks anything on the ~151-entry denylist. **The walker is the primary defense; the denylist is the manifest.** (See `project-denylist-complete` memory.)

`eval()` happens inside `state$eval_env`, a sealed env scoped to the package + ggplot2 + the snapshot — no `globalenv()` access. The constant-fold registry (`paintr-default-args.R`) handles the special case of validating `pp*(default = …)` args at translate time.

## What the runtime does NOT do

A short list of behaviors that surprise people:

- **It does not redraw on every keystroke.** Update click is the gate. (`paintr-server.R::ptr_setup_runtime`.)
- **It does not re-run the safety pass per click.** The safety walk runs once at translate time; the runtime reuses `state$tree`.
- **It does not have a public bare engine.** `ptr_server_internal` is plumbing. The "internal version of `ptr_server`" surface is closed.
- **It does not auto-publish a shared widget in the host panel for single-instance apps.** Single-instance always uses the inline section. No coordinator is built.
- **It does not allow `<` or `>` in user-supplied formula text by default.** The denylist + AST walker enforce this; see `paintr-utils.R::unsafe_expr_denylist` for the list.
