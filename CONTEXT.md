# ggpaintr — Domain Context

ggpaintr turns ggplot-like formula strings into Shiny apps: placeholder tokens in the formula (`var`, `text`, `num`, `expr`, `upload`) become Shiny input widgets automatically. This file records the domain language for the **L1/L2/L3 embedding model** and the **UI-piece taxonomy** — the terms an embedder must get right to use the public API correctly.

> Status: **design LOCKED 2026-05-16** (grill-with-docs session), recorded in `dev/adr/0005-l2-l3-redesign.html`. **Implementation pending** — the code still reflects the pre-redesign API; this file is the target contract. Implementation plan + per-symbol diff: `dev/plans/api-audit.md`. Any remaining _(in flux)_ tags are residual prose, not open decisions — every section header marked *LOCKED 2026-05-16* is decided.

## Language

### Embedding levels

**L1 — All-in-one**:
The standalone entry points that build *and* run the whole app: `ptr_app()` (single formula), `ptr_app_grid()` (multiple formulas). The user writes a formula, gets a running Shiny app.
_Avoid_: "the simple API", "quick mode".

**L2 — Embed with the default layout** _(in flux)_:
"Plug ggpaintr into my own Shiny app, accept the default layout (fluidPage → sidebar controls + main plot/code/error), optionally restyle via CSS." Target design: the L2 entry points are the module pair `ptr_module_ui()` / `ptr_module_server()` **and** the shared pair `ptr_shared_ui()` / `ptr_shared_server()` (for multi-plot cross-module sharing). The non-module split (`ptr_controls_ui`/`ptr_outputs_ui`) is **removed** — *that* is the scope of "only `ptr_module_*` at L2"; the shared pair is orthogonal and **stays L2 public API**.
_Avoid_: "the split", "embedding API" (ambiguous between L2 and L3); reading "only `ptr_module_*`" as excluding the shared pair.

**L3 — Own the layout / render path** — **LOCKED 2026-05-16**:
Anything beyond the default layout: break `ptr_module_ui`'s layout, replace built-in panes with your own widgets, or own the render path (your own `plotly::renderPlotly()` + `plotlyOutput()` instead of `ptr_ui_plot()`). Composed from **bare pieces**; the L3 user supplies their own Shiny (optionally `ptr_ui_page`). The server side is **unchanged from L2** — everything is registered server-side; **a piece with no corresponding UI is a no-op**; built-in plot/code/error are read from `state$runtime()`.

- **Custom render is L3** (reclassified 2026-05-16; supersedes the vignette's "needs nothing beyond L2" and the `project-headless-removed` memory's "demoted to L2 subsection" clause — *only* that clause; the no-headless / no-`testServer`-as-feature prohibition still stands). Rationale: the *enabling return value* (`state`) lives at the L2 server boundary, but *using it to replace a pane* is definitionally owning the render path = L3.
- **Canonical custom-render pattern**: `moduleServer(id, \(i,o,s){ state <- ptr_server(i,o,s,formula); o$my_plot <- renderPlotly(state$runtime()$plot) })` + your widget at `shiny::NS(id)("my_plot")` in the UI. Namespacing is then automatic and plain-Shiny-idiomatic. **`ptr_module_server` is the scope-hidden path** (for the bundled module ± a captured secondary view); replacing panes uses `ptr_server` inside your own `moduleServer`.
- **`state` is server-side only** — never reachable from UI code. `state$server_ns_fn`/`ui_ns_fn` are **internal plumbing, not public** — never documented as a user escape hatch (the canonical pattern makes them unnecessary).
- **`ptr_module_ui` is rebuilt on the L3 API**: internal composition of bare pieces + its own self-contained shell + combinators + (single-formula) the auto inline shared section.

_Avoid_: "headless" (there is no non-Shiny path — see CLAUDE.md / project memory), "custom rendering level" (it is L3, not a separate level), telling users to wire custom outputs off `state$server_ns_fn`.

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
The block of shared-key widgets rendered **inside one formula's control panel**, for keys that occur **only in that formula**. **Namespaced via the controls' `ns`** (`<id>-shared_<key>`); driven by that formula's own `ptr_server()`/`ptr_module_server()` — **no top-level `ptr_shared_server()`**. Automatic (a formula with formula-local shared keys gets one; not user-triggered). Today governed by the internal `render_shared_section` flag (whole-section on/off, no per-key partition).
_Avoid_: calling this a "panel"; assuming it knows about other formulas.

**Shared panel** _(standalone, cross-formula)_:
The single page-level `shiny::wellPanel()` built by `ptr_shared_ui(formulas, …)`, holding only keys referenced by **≥2 formulas**. Uses **global, un-namespaced ids** (`shared_<key>`, `ptr_shared_draw_all`, `ptr_shared_errors`); **exactly one per page**; its server counterpart `ptr_shared_server()` **must run at the top level**, never inside `moduleServer(id)`.
_Avoid_: calling this a "section"; assuming it can be namespaced or duplicated per module; assuming it currently filters to cross-formula keys (today it takes *all* keys).

### Shared coordinator _(in flux — new design)_

**Shared coordinator** (`obj`) — **LOCKED 2026-05-16**:
A single **pure, non-reactive** object built once from the full formula set, consolidating *all* shared config:
`ptr_shared(formulas, shared_ui = list(), ui_text = NULL, expr_check = TRUE, draw_all_label = "Draw all")` → `obj`.
(`expr_check` is needed because it translates the formulas to find shared keys.) It computes the **partition** (per key: formula-local → that formula's section; ≥2 formulas → the panel) and is the *single source of truth* so UI and server can never disagree. Consumed by exactly three functions, which take **only `obj`** (no duplicated config):
- `ptr_shared_panel(obj, css = NULL)` — L2 self-contained panel (`css` = the L2 restyle hook).
- `ptr_ui_shared_panel(obj)` — L3 bare panel (no `css`; the L3 shell owns assets).
- `ptr_shared_server(obj)` — the reactive `ptr_shared_state` bundle, unchanged downstream into `ptr_module_server(shared_state=)`.
_Avoid_: deriving the partition independently anywhere else; treating `obj` as reactive; putting `shared_ui`/`ui_text` config on the panel functions.

**Shared usage by instance count** — **LOCKED 2026-05-16**:
- **One ggpaintr instance** ⟹ no `shared=obj`; *all* shared widgets auto-render in that instance's inline shared section. **No coordinator, no panel — ever.** (Single-instance "promote to standalone panel for prominence" was considered and **rejected**.)
- **Multiple instances** ⟹ user *must* build `obj <- ptr_shared(formulas=list(…))`; cross-formula keys (≥2) go to one `ptr_shared_panel(obj)`; formula-local keys still render in each instance's inline section.
- Therefore `ptr_shared` / `ptr_shared_panel` / `ptr_shared_server` are **strictly multi-instance API**; single-instance embedders never see them. The old `ptr_module_ui(shared_panel=)` boolean is **removed** — routing is governed solely by presence of `shared=obj` + the count-based partition.

**Shared coordinator consumers** — exactly two roles, never more:
- `ptr_ui_controls(id, formula, shared = obj)` — renders layer controls **plus** that formula's formula-local shared section, **excluding** `obj`'s panel-owned keys. (Decision 2026-05-16: the section is folded into the controls piece, not a separate placeable piece.)
- the **shared-panel piece** — renders exactly `obj`'s ≥2-formula keys.
- All other pieces (`ptr_ui_plot`/`ptr_ui_error`/`ptr_ui_code`/header) are **shared-agnostic** — they never take `obj`.
- Single-formula L3, no panel → omit `obj`; the controls piece renders all the formula's shared keys (every key is formula-local by definition).

### Naming convention — **LOCKED 2026-05-16**

> **`ptr_<x>` = L2 self-contained** (owns its own `.ptr-app` + asset bundle; drop into the host app; no shell to remember).
> **`ptr_ui_<x>` = L3 bare** (emits only its widgets; the L3 user supplies their own Shiny / shell).

This rule governs the entire api-audit rename. The earlier-this-session `ptr_embed_*` idea is **dead** (superseded). `ptr_module_ui`/`ptr_module_server` keep their established names as the deliberate **sealed-bundle exception** (ADR 0004), read as the L2 module bundle, not as a generic `ptr_<x>`.

**L2 public surface** (self-contained, no `ptr_ui_` prefix):
`ptr_module_ui` / `ptr_module_server` (the L2 entry), `ptr_shared_panel` (cross-formula panel UI), `ptr_shared` (coordinator) / `ptr_shared_server`. The non-module split `ptr_controls_ui`/`ptr_outputs_ui` is **removed**.

**L3 public surface** (bare): `ptr_ui_controls`, `ptr_ui_plot`, `ptr_ui_error`, `ptr_ui_code`, `ptr_ui_header`, `ptr_ui_shared_panel`, plus the **combinators** (below). Pieces are **truly orthogonal** — `ptr_ui_plot(id)` has no `error=`/`code_toggle=` flags; behavior is added by combinators only. `ptr_ui_page(…, page, css)` is an **optional, L3-only** shell convenience — never on the L2 path; L3 users may instead write their own bare Shiny. It owns the one `.ptr-app` scope + (deduped htmlDependency) asset bundle via internal `ptr_assets()`.

**Combinator** — **LOCKED 2026-05-16**:
An `ptr_ui_*` function that takes already-built bare pieces and returns them wired into a behavior. Pure DOM-structure helpers; **no server coupling** (the server registers `ptr_plot`/`ptr_error`/`ptr_code` regardless). The bundled JS (`inst/www/ggpaintr-ui.js`) drives the toggle purely DOM-locally off `.ptr-output` / `.ptr-code-toggle` / `.ptr-code-window` classes — combinators just emit that markup.
- `ptr_ui_inline_error(plot, error)` — wraps a plot piece + error piece in the `.ptr-output` card so the error renders inline.
- `ptr_ui_toggle_code(plotish, code)` — wraps a plot-ish tag + code piece so the code hides behind the `</>` toggle.
- **Nestable**: `ptr_ui_toggle_code(ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)), ptr_ui_code(id))`.
- **Removed/superseded** by this model: the composites `ptr_ui_code_toggle`, `ptr_ui_plot_code`, `ptr_ui_outputs`; the `error=`/`code_toggle=` flags on `ptr_ui_plot`; ADR 0004's locked `ptr_ui_outputs ≡ ptr_ui_plot_code(error=TRUE)` contract; the earlier-this-session `include_error_panel` rename + default-flip. ADR 0005 records the supersession. The default L2 layout (plot + inline error + toggle code) lives **only** inside `ptr_module_ui`, composed from these combinators.

**Asset escape hatch**:
Exported `ptr_ui_assets(css)` — manual asset injector, *only* for out-of-`ptr_ui_page`-contract roots (`navbarPage`, bslib/BS5). Not part of normal L2/L3 use.

## Relationships

- An **L1** entry point internally composes the same pieces an **L3** user composes by hand; **L2** is the default-layout middle.
- A **shared placeholder** surfaces as a **shared section** (single-formula, inline in controls) *or* a **shared panel** (multi-formula, standalone, global ids) — never both for the same key on the same page.
- A **shared panel** pairs 1:1 with a top-level `ptr_shared_server()`; **bare pieces** / **embed composites** pair with `ptr_server()` / `ptr_module_server()` by matching `id`.
- The **partition rule decides *ownership*, not just placement**: the *one widget* for a key — including a `var (shared = …)` consumer's column picker — is **rendered and server-bound by whoever owns its surface**. A **formula-local** key is owned by **that one instance's own server**; a **panel** key (≥2 formulas) is owned by the **standalone panel + top-level `ptr_shared_server()`**. The host server owns **only** panel keys; it never reaches into a formula-local key. Single-instance has no panel, so the instance owns every shared key.
- The **page shell** contains **bare pieces**; an **embed composite** is (conceptually) a bare piece already wrapped in its own shell.

## Flagged ambiguities

- **"shared panel"** was used for *both* the inline single-formula section and the standalone multi-formula wellPanel. **Resolved**: these are distinct — **shared section** (inline, `render_shared_section`) vs **shared panel** (standalone, global ids, top-level server). Any design or doc must say which.
- **"L2 split" vs "L3 pieces"**: `ptr_controls_ui`/`ptr_outputs_ui` (self-wrapping composites) vs `ptr_ui_*` (bare). Earlier audit notes mislabeled cross-layer renames as mechanical. The redesign may delete the composite layer from L2 entirely.
- **"custom render path"** — current vignette/memory place it at L2 ("needs nothing beyond L2"); the redesign moves it back to **L3**. Unresolved; pending ADR 0005.
- **"empty shared panel"** — a multi-instance app where every shared key is formula-local (no ≥2-formula key) leaves the panel with nothing to show. **Resolved (grill 2026-05-17)**: this is a **valid configuration, not an error** — the panel must render **nothing** (no empty "Shared controls" box, no orphan Draw-all); each instance still renders its own inline section, and the top-level `ptr_shared_server()` stays a safe no-op.
- **"who binds a formula-local `var (shared=)`"** — the standalone-panel server was found also binding *formula-local* consumer pickers (global ids), contradicting "the owning instance binds it". **Resolved (grill 2026-05-17)**: the host server binds **only** panel keys; the owning instance binds every formula-local key (consumer or value). One partition, one owner, no double-bind.

## Example dialogue

> **Embedder:** "I'll drop `ptr_shared_ui()` into each module's sidebar so every plot gets the shared control."
> **Maintainer:** "No — the **shared panel** uses global ids and one top-level `ptr_shared_server()`; N copies collide. One standalone panel per page. If you only have *one* formula, you don't want the panel at all — you want the inline **shared section** (`render_shared_section = TRUE`)."
