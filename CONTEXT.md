# ggpaintr — Domain Context

ggpaintr turns ggplot-like formula strings into Shiny apps: `pp*` placeholder tokens in the formula (`ppVar`, `ppText`, `ppNum`, `ppExpr`, `ppUpload`) become Shiny input widgets automatically, and `pp*` structural keywords (`ppLayerOff`, `ppVerbSwitch`) configure UI behavior of layer/stage controls. This file records the domain language for the **L1/L2/L3 embedding model**, the **UI-piece taxonomy**, and the **`pp*` keyword family** — the terms an embedder must get right to use the public API correctly.

> Status: **design LOCKED 2026-05-16**, amended **2026-05-18** (grill-with-docs) and **2026-05-24** (grill-with-docs, ADR 0021). The 2026-05-18 amendment (single public server entry; L3 = UI-side only; `ptr_module_server`→`ptr_server`, `ptr_module_ui`→`ptr_ui`, old engine `ptr_server`→internal `ptr_server_internal`) is recorded in `dev/adr/0006-single-server-entry-l3-ui-only.html`, which supersedes `dev/adr/0005-l2-l3-redesign.html` **in part** (0005's L3-render-path definition, the sealed-bundle name exception, and the `ptr_server`-inside-`moduleServer` canonical pattern). The 2026-05-24 amendment (`ppVerbSwitch` replaces `ppVerbOff`; `stage_id` reclassified from "runtime-semantic" to "UI-routing key" and added to the comparator-exclusion list; the verb-side structural-equality invariant is restored via wrapping, not siblings) is recorded in `dev/adr/0021-pp-verb-switch.html`, which supersedes ADR 0020 on those two specific points (the rest of ADR 0020 — `ppLayerOff`, `default_active` / `default_stage_enabled` reader sites, `checkbox_defaults =` deprecation, precedence ladder — stands). The symbol renames are **implemented**; CONTEXT.md below reflects the post-amendment names. Remaining 0005 implementation (shared coordinator/partition) is still pending per `dev/plans/api-audit.md`. Every section header marked *LOCKED* is decided.

## Language

### Embedding levels

**L1 — All-in-one**:
The standalone entry points that build *and* run the whole app: `ptr_app()` (single formula), `ptr_app_grid()` (multiple formulas). The user writes a formula, gets a running Shiny app.
_Avoid_: "the simple API", "quick mode".

**L2 — Embed with the default layout**:
"Plug ggpaintr into my own Shiny app, accept the default layout (fluidPage → sidebar controls + main plot/code/error), optionally restyle via CSS." The L2 entry points are the self-contained pair `ptr_ui()` / `ptr_server()` (formerly `ptr_module_ui` / `ptr_module_server` — renamed by ADR 0006; `ptr_server()` is the **single public server entry**, used at L2 *and* L3) **and** the shared pair `ptr_shared_ui()` / `ptr_shared_server()` (for multi-plot cross-module sharing). The non-`ptr_ui`/`ptr_server` split (`ptr_controls_ui`/`ptr_outputs_ui`) is **removed**; the shared pair is orthogonal and **stays L2 public API**.
_Avoid_: "the split", "embedding API" (ambiguous between L2 and L3); "the module pair" / "`ptr_module_*`" (renamed — say `ptr_ui`/`ptr_server`); reading the trimmed split as excluding the shared pair.

**L3 — Own the layout (UI-side split)** — **LOCKED 2026-05-16, amended 2026-05-18 (ADR 0006)**:
L3 is a **UI-side split only**: anything beyond the default layout — break `ptr_ui`'s layout, place built-in panes individually, replace a pane with your own widget, or own the render path (your own `plotly::renderPlotly()` + `plotlyOutput()` instead of `ptr_ui_plot()`). Composed from **bare pieces**; the L3 user supplies their own Shiny (optionally `ptr_ui_page`). **L3 is not a server-side concept.** The server is **always the single public `ptr_server(formula, id = NULL, …)`** — identical at L2 and L3; everything is registered server-side; **a piece with no corresponding UI is a no-op**; built-in plot/code/error are read from `state$runtime()`.

- **Custom render = extract components from the `state` returned by `ptr_server()`** (ADR 0006; supersedes the vignette's "needs nothing beyond L2", the `project-headless-removed` memory's "demoted to L2 subsection" clause, *and* ADR 0005's "custom render is owning the render path" framing — the no-headless / no-`testServer`-as-feature prohibition still stands). It is still **L3** (you place your own output widget) but is **not a separate level and not a server pattern**: there is one server entry; you read `state$runtime()$plot` from its return value.
- **Canonical custom-render pattern**: `state <- ptr_server(formula, id)` (inside the host server, with the user's widget at `shiny::NS(id)("my_plot")` in the UI), then `output$my_plot <- plotly::renderPlotly(state$runtime()$plot)`. There is **no user-authored `moduleServer` wrapping a ggpaintr engine** and **no public bare engine** — the namespacing is handled by `ptr_server()` itself.
- **`state` is server-side only** — never reachable from UI code. `state$server_ns_fn`/`ui_ns_fn` are **internal plumbing, not public** — never documented as a user escape hatch. (Reason under ADR 0006: there is a single server entry and custom render extracts from its returned `state`; the old `ptr_server`-inside-your-own-`moduleServer` justification is withdrawn but the internal-only conclusion stands.)
- **`ptr_ui` is rebuilt on the L3 API**: internal composition of bare pieces + its own self-contained shell + combinators + (single-formula) the auto inline shared section.
- **`ptr_server_internal`** (the former exported `ptr_server` 4-arg engine) is **unexported plumbing** — not L1/L2/L3 surface, never recommended to users.

_Avoid_: "headless" (there is no non-Shiny path — see CLAUDE.md / project memory), "custom rendering level" / "L3 server pattern" (custom render is UI-side L3 fed by the one server entry, not a level or a server pattern), "render path" as a *server* concept, calling `ptr_server` a "module" server or implying a sibling non-module server, telling users to wire custom outputs off `state$server_ns_fn` or to wrap a bare engine in their own `moduleServer`.

### Shared placeholders — two distinct UI surfaces

**Shared placeholder**:
A placeholder annotated `… (shared = "<key>")`. Every occurrence of the same key — across aes, pipeline stages, and (in multi-plot) across formulas — is driven by **one** widget in lockstep. Valid in the **single-formula** case (one key used in multiple spots of one plot) and the **multi-formula** case (one key across plots).
_Avoid_: "linked input", "global parameter".

The **shared section** vs **shared panel** choice is decided **per key** by how many formulas reference it (the *partition rule* — target design, not yet implemented):

> A shared key referenced in **exactly one** formula → that formula's **shared section**.
> A shared key referenced in **≥2** formulas → the one **shared panel**.
>
> _Example_: `f1 = sharedA + sharedA + sharedB`, `f2 = sharedC + sharedC + sharedB` ⟹ f1's section holds `sharedA`, f2's section holds `sharedC`, the standalone panel holds `sharedB`.

**Shared section** _(inline, formula-local)_:
The block of shared-key widgets rendered **inside one formula's control panel**, for keys that occur **only in that formula**. **Namespaced via the controls' `ns`** (`<id>-shared_<key>`); driven by that formula's own `ptr_server()` (the single public server entry) — **no top-level `ptr_shared_server()`**. Automatic (a formula with formula-local shared keys gets one; not user-triggered). Today governed by the internal `render_shared_section` flag (whole-section on/off, no per-key partition).
_Avoid_: calling this a "panel"; assuming it knows about other formulas.

**Shared panel** _(standalone, cross-formula)_:
The page-level `shiny::wellPanel()` built by `ptr_shared_ui(formulas, …)`, holding only keys referenced by **≥2 formulas**. `ptr_shared(..., id = ...)` namespaces every shared id under the coordinator's `id` (`<id>_shared_<key>`, `<id>_ptr_shared_draw_all`, `<id>_ptr_shared_errors`) — see `R/paintr-shared-coordinator.R:220–234, 292`. **Multiple coordinators on one page are supported**, each with its own `id`; their server counterparts `ptr_shared_server(obj)` all run at the top level, never inside `moduleServer(id)`. (Caveat: an upstream `ptr_setup_panel_sources` fix is still owed for shared *sources* whose `var(shared = ...)` consumer references a literal symbol name — tracked as a separate follow-up.)
_Avoid_: calling this a "section"; assuming a single coordinator id is mandatory; assuming the panel currently filters to cross-formula keys (today it takes *all* keys).

### Shared coordinator _(in flux — new design)_

**Shared coordinator** (`obj`) — **LOCKED 2026-05-16**:
A single **pure, non-reactive** object built once from the full formula set, consolidating *all* shared config:
`ptr_shared(formulas, shared_ui = list(), ui_text = NULL, expr_check = TRUE, draw_all_label = "Draw all")` → `obj`.
(`expr_check` is needed because it translates the formulas to find shared keys.) It computes the **partition** (per key: formula-local → that formula's section; ≥2 formulas → the panel) and is the *single source of truth* so UI and server can never disagree. Consumed by exactly three functions, which take **only `obj`** (no duplicated config):
- `ptr_shared_panel(obj, css = NULL)` — L2 self-contained panel (`css` = the L2 restyle hook).
- `ptr_ui_shared_panel(obj)` — L3 bare panel (no `css`; the L3 shell owns assets).
- `ptr_shared_server(obj)` — the reactive `ptr_shared_state` bundle, unchanged downstream into `ptr_server(shared_state=)`.
_Avoid_: deriving the partition independently anywhere else; treating `obj` as reactive; putting `shared_ui`/`ui_text` config on the panel functions.

**Shared usage by instance count** — **LOCKED 2026-05-16**:
- **One ggpaintr instance** ⟹ no `shared=obj`; *all* shared widgets auto-render in that instance's inline shared section. **No coordinator, no panel — ever.** (Single-instance "promote to standalone panel for prominence" was considered and **rejected**.)
- **Multiple instances** ⟹ user *must* build `obj <- ptr_shared(formulas=list(…))`; cross-formula keys (≥2) go to one `ptr_shared_panel(obj)`; formula-local keys still render in each instance's inline section.
- Therefore `ptr_shared` / `ptr_shared_panel` / `ptr_shared_server` are **strictly multi-instance API**; single-instance embedders never see them. The old `ptr_ui(shared_panel=)` boolean is **removed** — routing is governed solely by presence of `shared=obj` + the count-based partition.

**Shared coordinator consumers** — exactly two roles, never more:
- `ptr_ui_controls(id, formula, shared = obj)` — renders layer controls **plus** that formula's formula-local shared section, **excluding** `obj`'s panel-owned keys. (Decision 2026-05-16: the section is folded into the controls piece, not a separate placeable piece.)
- the **shared-panel piece** — renders exactly `obj`'s ≥2-formula keys.
- All other pieces (`ptr_ui_plot`/`ptr_ui_error`/`ptr_ui_code`/header) are **shared-agnostic** — they never take `obj`.
- Single-formula L3, no panel → omit `obj`; the controls piece renders all the formula's shared keys (every key is formula-local by definition).

### Layer and stage toggles — `pp*` structural-keyword family

The `pp*` family has two roles, registered through the same registry but routed differently by the translator:

- **Placeholders** (`role = "value"` / `"consumer"` / `"source"`): `ppText`, `ppNum`, `ppExpr`, `ppVar`, `ppUpload`. Become `ptr_ph_*` nodes; each is a UI widget bind-point. Naked-R semantics: identity (return their arg unchanged) so the formula renders out-of-ggpaintr too.
- **Structural keywords** (`role = "structural"`): `ppLayerOff`, `ppVerbSwitch`. Never become `ptr_ph_*` nodes — the translator's *special-unwrap* branches recognise the wrapper, translate the inner expression as a normal layer/stage, and stamp metadata on the resulting carrier node. The wrapper itself never appears in the typed tree.

_Avoid_: calling structural keywords "placeholders" — they don't bind to widgets, they configure UI behavior of other nodes; "off-by-default" wrappers as if there's only one such family — `ppLayerOff(layer, hide=)` toggles an *already-existing* layer checkbox; `ppVerbSwitch(verb, switch_on=, label=)` *creates* a stage checkbox (with custom label) that wouldn't otherwise exist.

**Formula-as-source-of-truth principle** (ADR 0020, refined by ADR 0021): the formula is the canonical record of what the app shows at boot for *every* first-class UI control — placeholders' initial values (`ppText(initial = "hi")`), layer-checkbox boot state (`ppLayerOff(..., hide = TRUE)`), and stage-checkbox existence + boot state + label (`ppVerbSwitch(verb, switch_on = FALSE, label = "…")`). The same formula reads the same in or out of ggpaintr — naked-R semantics drop hidden layers, skip switched-off verbs, and ignore UI-only `label` args.

**`stage_id` is a UI-routing key, not execution-shape** — **LOCKED 2026-05-24 (ADR 0021)**: a `ptr_call` carrying `stage_id = "<layer>_<path>_stage_enabled"` is *addressable* by a UI checkbox; without a stage_id (or with one whose `stage_enabled[[sid]]` reactive is unbound or non-`FALSE`) the stage runs unconditionally. `disable_walk` drops a stage iff `isFALSE(stage_enabled[[sid]])` — NULL/missing reactives are runtime no-ops. Therefore `stage_id` differs between two trees only when they differ on "does this stage have a UI checkbox?" — pure UI-affordance metadata. **`ptr_tree_structural_equal` excludes `stage_id`** alongside `default_active` / `default_stage_enabled` / `has_user_control` / `stage_label` / `op` / `expr`. This supersedes ADR 0020's *Out-of-Scope* note that called `stage_id` "runtime-semantic"; that framing was based on what `disable_walk` *can* do given a stage_id, not what it *does* in the unwired case.
_Avoid_: treating `stage_id` presence as an execution-shape difference; assuming `stage_id` stamping changes runtime behavior on its own (it doesn't — only a bound `FALSE` reactive does); "stage_id is runtime-semantic" (the ADR-0020 framing this overrides).

**Stage-checkbox gating** (`R/paintr-disable.R::is_data_chain_call`): a `ptr_call` qualifies for a `stage_id` iff its subtree contains a placeholder **OR** it carries `has_user_control = TRUE` (stamped by `ppVerbSwitch`). Single gate, two triggers — the post-translate stamping pass `stamp_default_stage_enabled_ids` that Plan 01 introduced as a workaround is removed; the gate does the work in one place.

### Naming convention — **LOCKED 2026-05-16**

> **`ptr_<x>` = L2 self-contained** (owns its own `.ptr-app` + asset bundle; drop into the host app; no shell to remember).
> **`ptr_ui_<x>` = L3 bare** (emits only its widgets; the L3 user supplies their own Shiny / shell).

This rule governs the entire api-audit rename. The earlier `ptr_embed_*` idea is **dead** (superseded). The L2 self-contained pair is **`ptr_ui`/`ptr_server`** (renamed from `ptr_module_ui`/`ptr_module_server` by ADR 0006, which **voids** ADR 0004/0005's sealed-bundle name exception). They are now regular `ptr_<x>` self-contained names; `ptr_server` is the single public server entry (no sibling "module" server exists, so the "module" qualifier was dropped as misleading).

**L2 public surface** (self-contained, no `ptr_ui_` prefix):
`ptr_ui` / `ptr_server` (the L2 entry; `ptr_server` is also the single public server entry at L3), `ptr_shared_panel` (cross-formula panel UI), `ptr_shared` (coordinator) / `ptr_shared_server`. The non-`ptr_ui`/`ptr_server` split `ptr_controls_ui`/`ptr_outputs_ui` is **removed**.

**L3 public surface** (bare): `ptr_ui_controls`, `ptr_ui_plot`, `ptr_ui_error`, `ptr_ui_code`, `ptr_ui_header`, `ptr_ui_shared_panel`, plus the **combinators** (below). Pieces are **truly orthogonal** — `ptr_ui_plot(id)` has no `error=`/`code_toggle=` flags; behavior is added by combinators only. `ptr_ui_page(…, page, css)` is an **optional, L3-only** shell convenience — never on the L2 path; L3 users may instead write their own bare Shiny. It owns the one `.ptr-app` scope + (deduped htmlDependency) asset bundle via internal `ptr_assets()`.

**Combinator** — **LOCKED 2026-05-16**:
An `ptr_ui_*` function that takes already-built bare pieces and returns them wired into a behavior. Pure DOM-structure helpers; **no server coupling** (the server registers `ptr_plot`/`ptr_error`/`ptr_code` regardless). The bundled JS (`inst/www/ggpaintr-ui.js`) drives the toggle purely DOM-locally off `.ptr-output` / `.ptr-code-toggle` / `.ptr-code-window` classes — combinators just emit that markup.
- `ptr_ui_inline_error(plot, error)` — wraps a plot piece + error piece in the `.ptr-output` card so the error renders inline.
- `ptr_ui_toggle_code(plotish, code)` — wraps a plot-ish tag + code piece so the code hides behind the `</>` toggle.
- **Nestable**: `ptr_ui_toggle_code(ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)), ptr_ui_code(id))`.
- **Removed/superseded** by this model: the composites `ptr_ui_code_toggle`, `ptr_ui_plot_code`, `ptr_ui_outputs`; the `error=`/`code_toggle=` flags on `ptr_ui_plot`; ADR 0004's locked `ptr_ui_outputs ≡ ptr_ui_plot_code(error=TRUE)` contract; the earlier-this-session `include_error_panel` rename + default-flip. ADR 0005 records the supersession. The default L2 layout (plot + inline error + toggle code) lives **only** inside `ptr_ui`, composed from these combinators.

**Asset escape hatch**:
Exported `ptr_ui_assets(css)` — manual asset injector, *only* for out-of-`ptr_ui_page`-contract roots (`navbarPage`, bslib/BS5). Not part of normal L2/L3 use.

## Relationships

- An **L1** entry point internally composes the same pieces an **L3** user composes by hand; **L2** is the default-layout middle.
- A **shared placeholder** surfaces as a **shared section** (single-formula, inline in controls) *or* a **shared panel** (multi-formula, standalone, global ids) — never both for the same key on the same page.
- A **shared panel** pairs 1:1 with a top-level `ptr_shared_server()`; **bare pieces** / the **`ptr_ui` bundle** pair with the single `ptr_server()` by matching `id`.
- The **partition rule decides *ownership*, not just placement**: the *one widget* for a key — including a `var (shared = …)` consumer's column picker — is **rendered and server-bound by whoever owns its surface**. A **formula-local** key is owned by **that one instance's own server**; a **panel** key (≥2 formulas) is owned by the **standalone panel + top-level `ptr_shared_server()`**. The host server owns **only** panel keys; it never reaches into a formula-local key. Single-instance has no panel, so the instance owns every shared key.
- The **page shell** contains **bare pieces**; an **embed composite** is (conceptually) a bare piece already wrapped in its own shell.

## Flagged ambiguities

- **"shared panel"** was used for *both* the inline single-formula section and the standalone multi-formula wellPanel. **Resolved**: these are distinct — **shared section** (inline, `render_shared_section`) vs **shared panel** (standalone, global ids, top-level server). Any design or doc must say which.
- **"L2 split" vs "L3 pieces"**: `ptr_controls_ui`/`ptr_outputs_ui` (self-wrapping composites) vs `ptr_ui_*` (bare). Earlier audit notes mislabeled cross-layer renames as mechanical. The redesign may delete the composite layer from L2 entirely.
- **"custom render path"** — **Resolved (ADR 0006, 2026-05-18)**: custom render is **L3 (UI-side)**, done by extracting components from the `state` returned by the single public `ptr_server()`. It is not a server-side concept, not a separate level, and there is no user-wrapped engine.
- **"empty shared panel"** — a multi-instance app where every shared key is formula-local (no ≥2-formula key) leaves the panel with nothing to show. **Resolved (grill 2026-05-17)**: this is a **valid configuration, not an error** — the panel must render **nothing** (no empty "Shared controls" box, no orphan Draw-all); each instance still renders its own inline section, and the top-level `ptr_shared_server()` stays a safe no-op.
- **"who binds a formula-local `var (shared=)`"** — the standalone-panel server was found also binding *formula-local* consumer pickers (global ids), contradicting "the owning instance binds it". **Resolved (grill 2026-05-17)**: the host server binds **only** panel keys; the owning instance binds every formula-local key — value, consumer, and source alike. One partition, one owner, no double-bind.
- **"is `stage_id` runtime-semantic?"** — ADR 0020's *Out-of-Scope* note classified `stage_id` as "runtime-semantic — `disable_walk` reads it to drop stages" and on that basis forbade `stage_id` from the comparator-exclusion list, blocking Plan-03 SC7's verb-side structural-equality invariant. **Resolved (ADR 0021, 2026-05-24)**: source inspection (`R/paintr-disable.R::is_stage_disabled`) shows `disable_walk` drops a stage iff `isFALSE(stage_enabled[[sid]])` — unbound reactives are no-ops. `stage_id` is a *UI-routing key*: it enables a UI-driven gate without changing baseline runtime behavior. It joins the exclusion list alongside `default_active` / `default_stage_enabled` / `has_user_control` / `stage_label`. SC7 is restored as a side effect of the `ppVerbOff` → `ppVerbSwitch` replacement.
- **"`ppVerbOff` vs `ppVerbControl` vs `ppVerbSwitch` naming and shape"** — ADR 0020's *Out-of-Scope* note proposed a future `ppControl` placeholder with a *sibling-node* shape (control-only node parallel to the verb in the tree) to dodge the `stage_id` asymmetry. **Resolved (ADR 0021, 2026-05-24)**: the sibling-node proposal is rejected — the user-facing surface for any verb-stage UI control is wrapping (`ppVerbSwitch(verb, switch_on=, label=)`), not siblings (which cannot be expressed in surface R without inventing a non-pipe operator). One keyword (`ppVerbSwitch`) strictly subsumes `ppVerbOff`; `ppVerbOff` is hard-removed in the same change.

## Example dialogue

> **Embedder:** "I'll drop `ptr_shared_ui()` into each module's sidebar so every plot gets the shared control."
> **Maintainer:** "No — the **shared panel** uses coordinator-namespaced ids built by `ptr_shared(..., id = ...)`; two copies of the *same* coordinator collide because they share an `id`. If you genuinely need two independent shared groups on one page, build two coordinators with different `id`s and emit a `ptr_shared_panel(obj)` for each. If you only have *one* formula, you don't want the panel at all — you want the inline **shared section** (`render_shared_section = TRUE`)."
