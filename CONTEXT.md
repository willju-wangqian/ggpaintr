# ggpaintr — Domain Context

ggpaintr turns ggplot-like formula strings into Shiny apps: `pp*`
placeholder tokens in the formula (`ppVar`, `ppText`, `ppNum`, `ppExpr`,
`ppUpload`) become Shiny input widgets automatically, and `pp*`
structural keywords (`ppLayerOff`, `ppVerbSwitch`) configure UI behavior
of layer/stage controls. This file records the domain language for the
**L1/L2/L3 embedding model**, the **UI-piece taxonomy**, and the **`pp*`
keyword family** — the terms an embedder must get right to use the
public API correctly.

> Status: **design LOCKED 2026-05-16**, amended **2026-05-18**
> (grill-with-docs) and **2026-05-24** (grill-with-docs, ADR 0021). The
> 2026-05-18 amendment (single public server entry; L3 = UI-side only;
> `ptr_module_server`→`ptr_server`, `ptr_module_ui`→`ptr_ui`, old engine
> `ptr_server`→internal `ptr_server_internal`) is recorded in
> `dev/adr/0006-single-server-entry-l3-ui-only.html`, which supersedes
> `dev/adr/0005-l2-l3-redesign.html` **in part** (0005’s L3-render-path
> definition, the sealed-bundle name exception, and the
> `ptr_server`-inside-`moduleServer` canonical pattern). The 2026-05-24
> amendment (`ppVerbSwitch` replaces `ppVerbOff`; `stage_id`
> reclassified from “runtime-semantic” to “UI-routing key” and added to
> the comparator-exclusion list; the verb-side structural-equality
> invariant is restored via wrapping, not siblings) is recorded in
> `dev/adr/0021-pp-verb-switch.html`, which supersedes ADR 0020 on those
> two specific points (the rest of ADR 0020 — `ppLayerOff`,
> `default_active` / `default_stage_enabled` reader sites,
> `checkbox_defaults =` deprecation, precedence ladder — stands). The
> symbol renames are **implemented**; CONTEXT.md below reflects the
> post-amendment names. The remaining 0005 work (shared coordinator +
> partition) — pending at the 2026-05-18 amendment — has since
> **landed**:
> [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
> computes the partition, the inline shared section applies it via
> `host_owned_keys`, and panel-owned shared *sources* are wired
> host-side through the bundle’s `panel_sources` field
> (`ptr_setup_panel_sources()`). Four later ADRs post-date the
> 2026-05-24 amendment and refine behavior without changing the L1/L2/L3
> or `pp*` language here: **0023** (panel-owned shared sources
> end-to-end + the parse-time *panel-owned consumer requires panel-owned
> source* invariant — landed), **0024** (the `ppUpload` companion
> textbox is a first-class data-loading entry point — *accepted*),
> **0025** (source-shortcut rename + deterministic upload auto-name
> `df_<hash(id)>` + UI mutex — landed), **0022** (the code panel’s
> second view is now “Spec”, replacing the preserve-mode formula
> round-trip — landed). The `shared_ui` argument to
> [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
> was **removed** (commit `7b0297c`; widgets now render from each
> placeholder’s own `build_ui`). Amended **2026-05-30** (additive, no
> ADR):
> [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)’s
> `formulas` now accepts quoted ggplot expressions (`expr()` /
> [`quote()`](https://rdrr.io/r/base/substitute.html)) per element
> alongside strings — parity with the string-or-expression input of
> [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
> /
> [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md);
> the signature is unchanged and existing all-string calls are
> byte-for-byte unaffected. Amended **2026-06-10** (grill-with-docs):
> new **Plotly interop — linked selection** section (drawn data / row
> key / linked selection / selection projections / reserved channels)
> and the **Draw** term — the Update button is demoted from core
> invariant to default trigger (`ptr_options(gate_draw = FALSE)` runs
> the same render body live, which is what makes selection-fed formulas
> redraw per brush); `draw_trigger`’s implemented contract is a
> click-counter reactive, not a general invalidation hook. Every section
> header marked *LOCKED* is decided.

## Language

### Embedding levels

**L1 — All-in-one**: The standalone entry points that build *and* run
the whole app:
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
(single formula),
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
(multiple formulas). The user writes a formula, gets a running Shiny
app. *Avoid*: “the simple API”, “quick mode”.

**L2 — Embed with the default layout**: “Plug ggpaintr into my own Shiny
app, accept the default layout (fluidPage → sidebar controls + main
plot/code/error), optionally restyle via CSS.” The L2 entry points are
the self-contained pair
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
(formerly `ptr_module_ui` / `ptr_module_server` — renamed by ADR 0006;
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
is the **single public server entry**, used at L2 *and* L3) **and** the
shared coordinator API
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
(coordinator) /
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
(panel UI) /
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
(for multi-plot cross-module sharing). The non-`ptr_ui`/`ptr_server`
split (`ptr_controls_ui`/`ptr_outputs_ui`) is **removed**; the shared
pair is orthogonal and **stays L2 public API**. *Avoid*: “the split”,
“embedding API” (ambiguous between L2 and L3); “the module pair” /
“`ptr_module_*`” (renamed — say `ptr_ui`/`ptr_server`); reading the
trimmed split as excluding the shared pair.

**L3 — Own the layout (UI-side split)** — **LOCKED 2026-05-16, amended
2026-05-18 (ADR 0006)**: L3 is a **UI-side split only**: anything beyond
the default layout — break `ptr_ui`’s layout, place built-in panes
individually, replace a pane with your own widget, or own the render
path (your own
[`plotly::renderPlotly()`](https://rdrr.io/pkg/plotly/man/plotly-shiny.html) +
`plotlyOutput()` instead of
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md)).
Composed from **bare pieces**; the L3 user supplies their own Shiny
(optionally `ptr_ui_page`). **L3 is not a server-side concept.** The
server is **always the single public
`ptr_server(formula, id = NULL, …)`** — identical at L2 and L3;
everything is registered server-side; **a piece with no corresponding UI
is a no-op**; built-in plot/code/error are read from `state$runtime()`.

- **Custom render = extract components from the `state` returned by
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)**
  (ADR 0006; supersedes the vignette’s “needs nothing beyond L2”, the
  `project-headless-removed` memory’s “demoted to L2 subsection” clause,
  *and* ADR 0005’s “custom render is owning the render path” framing —
  the no-headless / no-`testServer`-as-feature prohibition still
  stands). It is still **L3** (you place your own output widget) but is
  **not a separate level and not a server pattern**: there is one server
  entry; you read `state$runtime()$plot` from its return value.
- **Canonical custom-render pattern**:
  `state <- ptr_server(formula, id)` (inside the host server, with the
  user’s widget at `shiny::NS(id)("my_plot")` in the UI), then
  `output$my_plot <- plotly::renderPlotly(state$runtime()$plot)`. There
  is **no user-authored `moduleServer` wrapping a ggpaintr engine** and
  **no public bare engine** — the namespacing is handled by
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  itself.
- **`state` is server-side only** — never reachable from UI code.
  `state$server_ns_fn`/`ui_ns_fn` are **internal plumbing, not public**
  — never documented as a user escape hatch. (Reason under ADR 0006:
  there is a single server entry and custom render extracts from its
  returned `state`; the old `ptr_server`-inside-your-own-`moduleServer`
  justification is withdrawn but the internal-only conclusion stands.)
- **`ptr_ui` is rebuilt on the L3 API**: internal composition of bare
  pieces + its own self-contained shell + combinators + (single-formula)
  the auto inline shared section.
- **`ptr_server_internal`** (the former exported `ptr_server` 4-arg
  engine) is **unexported plumbing** — not L1/L2/L3 surface, never
  recommended to users.

*Avoid*: “headless” (there is no non-Shiny path — see CLAUDE.md /
project memory), “custom rendering level” / “L3 server pattern” (custom
render is UI-side L3 fed by the one server entry, not a level or a
server pattern), “render path” as a *server* concept, calling
`ptr_server` a “module” server or implying a sibling non-module server,
telling users to wire custom outputs off `state$server_ns_fn` or to wrap
a bare engine in their own `moduleServer`.

### Shared placeholders — two distinct UI surfaces

**Shared placeholder**: A placeholder annotated `… (shared = "<key>")`.
Every occurrence of the same key — across aes, pipeline stages, and (in
multi-plot) across formulas — is driven by **one** widget in lockstep.
Valid in the **single-formula** case (one key used in multiple spots of
one plot) and the **multi-formula** case (one key across plots).
*Avoid*: “linked input”, “global parameter”.

The **shared section** vs **shared panel** choice is decided **per key**
by how many formulas reference it (the *partition rule* —
**implemented**: `shared_partition()` in `R/paintr-shared-coordinator.R`
computes `panel_keys` (≥2 formulas) vs formula-local, and ownership is
applied end-to-end via `host_owned_keys` / `panel_sources`):

> A shared key referenced in **exactly one** formula → that formula’s
> **shared section**. A shared key referenced in **≥2** formulas → the
> one **shared panel**.
>
> *Example*: `f1 = sharedA + sharedA + sharedB`,
> `f2 = sharedC + sharedC + sharedB` ⟹ f1’s section holds `sharedA`,
> f2’s section holds `sharedC`, the standalone panel holds `sharedB`.

The partition is **role-agnostic** (ADR 0023): it applies to value
placeholders, `ppVar(shared=)` consumers, **and** sources
(`ppUpload(shared=)`) alike. A panel-owned source is resolved once
host-side and delivered to every instance via `panel_sources`. One
asymmetry is illegal by construction: a **panel-owned consumer requires
a panel-owned source** — pairing a shared consumer (≥2 formulas) with
formula-local sources is a parse-time error at
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md),
because one shared column picker cannot list columns from two different
datasets.

**Shared section** *(inline, formula-local)*: The block of shared-key
widgets rendered **inside one formula’s control panel**, for keys that
occur **only in that formula**. **Namespaced via the controls’ `ns`**
(`<id>-shared_<key>`); driven by that formula’s own
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
(the single public server entry) — **no top-level
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)**.
Automatic (a formula with formula-local shared keys gets one; not
user-triggered). The internal `render_shared_section` flag toggles
whether the whole section renders; **the per-key partition is applied**
— panel-owned keys (≥2 formulas) are excluded from the inline section
via `host_owned_keys` (`ptr_bind_local_shared_consumers`), so a key
never renders in both the section and the panel. *Avoid*: calling this a
“panel”; assuming it knows about other formulas.

**Shared panel** *(standalone, cross-formula)*: The page-level
[`shiny::wellPanel()`](https://rdrr.io/pkg/shiny/man/wellPanel.html)
built by `ptr_shared_panel(obj)` (from the coordinator `obj`, not raw
`formulas`), holding only keys referenced by **≥2 formulas**.
`ptr_shared(..., id = ...)` namespaces every shared id under the
coordinator’s `id` (`<id>_shared_<key>`, `<id>_ptr_shared_draw_all`,
`<id>_ptr_shared_errors`) — see
`R/paintr-shared-coordinator.R:220–234, 292`. **Multiple coordinators on
one page are supported**, each with its own `id`; their server
counterparts `ptr_shared_server(obj)` all run at the top level, never
inside `moduleServer(id)`. Panel-owned shared **sources**
(`ppUpload(shared=)` referenced by ≥2 formulas) are resolved host-side
and delivered to each instance via the bundle’s `panel_sources` field
(`ptr_setup_panel_sources()`, ADR 0023) — the source-role gap ADR 0005
left open is now closed. *Avoid*: calling this a “section”; assuming a
single coordinator id is mandatory; assuming the panel currently filters
to cross-formula keys (today it takes *all* keys).

### Shared coordinator

**Shared coordinator** (`obj`) — **LOCKED 2026-05-16**: A single **pure,
non-reactive** object built once from the full formula set,
consolidating *all* shared config:
`ptr_shared(formulas, id = NULL, ui_text = NULL, expr_check = TRUE, draw_all_label = "Draw all")`
→ `obj`. (`expr_check` is needed because it translates the formulas to
find shared keys; `id` namespaces the panel so multiple coordinators can
coexist on one page. The former `shared_ui` per-key widget-override
argument was **removed** — every shared key now auto-renders via its
placeholder’s own `build_ui`.) It computes the **partition** (per key:
formula-local → that formula’s section; ≥2 formulas → the panel) and is
the *single source of truth* so UI and server can never disagree.
Consumed by exactly three functions, which take **only `obj`** (no
duplicated config): - `ptr_shared_panel(obj, css = NULL)` — L2
self-contained panel (`css` = the L2 restyle hook). -
`ptr_ui_shared_panel(obj)` — L3 bare panel (no `css`; the L3 shell owns
assets). - `ptr_shared_server(obj)` — the reactive `ptr_shared_state`
bundle, unchanged downstream into `ptr_server(shared_state=)`. *Avoid*:
deriving the partition independently anywhere else; treating `obj` as
reactive; putting `ui_text` config on the panel functions; referring to
a `shared_ui` argument (removed).

**Shared usage by instance count** — **LOCKED 2026-05-16**: - **One
ggpaintr instance** ⟹ no `shared=obj`; *all* shared widgets auto-render
in that instance’s inline shared section. **No coordinator, no panel —
ever.** (Single-instance “promote to standalone panel for prominence”
was considered and **rejected**.) - **Multiple instances** ⟹ user *must*
build `obj <- ptr_shared(formulas=list(…))`; cross-formula keys (≥2) go
to one `ptr_shared_panel(obj)`; formula-local keys still render in each
instance’s inline section. - Therefore `ptr_shared` / `ptr_shared_panel`
/ `ptr_shared_server` are **strictly multi-instance API**;
single-instance embedders never see them. The old
`ptr_ui(shared_panel=)` boolean is **removed** — routing is governed
solely by presence of `shared=obj` + the count-based partition.

**Shared coordinator consumers** — exactly two roles, never more: -
`ptr_ui_controls(id, formula, shared = obj)` — renders layer controls
**plus** that formula’s formula-local shared section, **excluding**
`obj`’s panel-owned keys. (Decision 2026-05-16: the section is folded
into the controls piece, not a separate placeable piece.) - the
**shared-panel piece** — renders exactly `obj`’s ≥2-formula keys. - All
other pieces (`ptr_ui_plot`/`ptr_ui_error`/`ptr_ui_code`/header) are
**shared-agnostic** — they never take `obj`. - Single-formula L3, no
panel → omit `obj`; the controls piece renders all the formula’s shared
keys (every key is formula-local by definition).

### Layer and stage toggles — `pp*` structural-keyword family

The `pp*` family has two roles, registered through the same registry but
routed differently by the translator:

- **Placeholders** (`role = "value"` / `"consumer"` / `"source"`):
  `ppText`, `ppNum`, `ppExpr`, `ppVar`, `ppUpload`. Become `ptr_ph_*`
  nodes; each is a UI widget bind-point. Naked-R semantics: identity
  (return their arg unchanged) so the formula renders out-of-ggpaintr
  too.
- **Structural keywords** (`role = "structural"`): `ppLayerOff`,
  `ppVerbSwitch`. Never become `ptr_ph_*` nodes — the translator’s
  *special-unwrap* branches recognise the wrapper, translate the inner
  expression as a normal layer/stage, and stamp metadata on the
  resulting carrier node. The wrapper itself never appears in the typed
  tree.

*Avoid*: calling structural keywords “placeholders” — they don’t bind to
widgets, they configure UI behavior of other nodes; “off-by-default”
wrappers as if there’s only one such family — `ppLayerOff(layer, hide=)`
toggles an *already-existing* layer checkbox;
`ppVerbSwitch(verb, switch_on=, label=)` *creates* a stage checkbox
(with custom label) that wouldn’t otherwise exist.

**Formula-as-source-of-truth principle** (ADR 0020, refined by ADR
0021): the formula is the canonical record of what the app shows at boot
for *every* first-class UI control — placeholders’ initial values
(`ppText(initial = "hi")`), layer-checkbox boot state
(`ppLayerOff(..., hide = TRUE)`), and stage-checkbox existence + boot
state + label (`ppVerbSwitch(verb, switch_on = FALSE, label = "…")`).
The same formula reads the same in or out of ggpaintr — naked-R
semantics drop hidden layers, skip switched-off verbs, and ignore
UI-only `label` args.

**`stage_id` is a UI-routing key, not execution-shape** — **LOCKED
2026-05-24 (ADR 0021)**: a `ptr_call` carrying
`stage_id = "<layer>_<path>_stage_enabled"` is *addressable* by a UI
checkbox; without a stage_id (or with one whose `stage_enabled[[sid]]`
reactive is unbound or non-`FALSE`) the stage runs unconditionally.
`disable_walk` drops a stage iff `isFALSE(stage_enabled[[sid]])` —
NULL/missing reactives are runtime no-ops. Therefore `stage_id` differs
between two trees only when they differ on “does this stage have a UI
checkbox?” — pure UI-affordance metadata. **`ptr_tree_structural_equal`
excludes `stage_id`** alongside `default_active` /
`default_stage_enabled` / `has_user_control` / `stage_label` / `op` /
`expr`. This supersedes ADR 0020’s *Out-of-Scope* note that called
`stage_id` “runtime-semantic”; that framing was based on what
`disable_walk` *can* do given a stage_id, not what it *does* in the
unwired case. *Avoid*: treating `stage_id` presence as an
execution-shape difference; assuming `stage_id` stamping changes runtime
behavior on its own (it doesn’t — only a bound `FALSE` reactive does);
“stage_id is runtime-semantic” (the ADR-0020 framing this overrides).

**Stage-checkbox gating** (`R/paintr-disable.R::is_data_chain_call`): a
`ptr_call` qualifies for a `stage_id` iff its subtree contains a
placeholder **OR** it carries `has_user_control = TRUE` (stamped by
`ppVerbSwitch`). Single gate, two triggers — the post-translate stamping
pass `stamp_default_stage_enabled_ids` that Plan 01 introduced as a
workaround is removed; the gate does the work in one place.

### Plotly interop — linked selection — **semantics LOCKED 2026-06-10 (grill in progress)**

The opt-in helper layer (plotly in `Suggests`) for rendering a formula’s
plot via `ggplotly()` and reading point selections back. It is plain
**L3 custom render** under ADR 0006 — no second server entry, no new
embedding level; helpers only remove the key-tagging/event boilerplate.

**Drawn data**: The plot’s evaluated data frame at draw time
(`state$runtime()$plot$data` of the draw that produced the widget) — the
*only* referent for selection. *Avoid*: “the original data”, “the source
object” — after a pipeline head (`filter`/`mutate`/`summarise`) or an
`ppUpload` swap there is no stable mapping back.

**Row key**: A per-draw identifier minted on the drawn data at render
time and carried through plotly’s `key` aesthetic; meaningless across
draws. *Avoid*: treating keys as stable row ids of any persistent
dataset.

**Linked selection**: The set of drawn-data rows currently
brush/lasso-selected on the plotly rendering — surfaced as a **slice of
the drawn data** (rows, not bare indices) and **reset to empty on every
draw** (a draw may change filters, columns, or the uploaded data, so
carrying a selection across draws is silently wrong). *Avoid*: returning
bare indices (invites subsetting an object that no longer lines up);
persisting selection across redraws.

**Selection projections** — **LOCKED 2026-06-10**: The one linked
selection is consumed in two shapes: **rows** (the selected slice of the
drawn data — for tables and detail views; zero rows, same columns, when
nothing is selected) and **flag** (the *full* drawn data plus a logical
`.ptr_selected` column — for highlight plots, where the formula maps
`color = .ptr_selected`). Flag is the shape that makes a **selection-fed
formula** work: a second
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
instance whose pipeline head is the selection reactive
(`sel() |> ggplot(...)`), redrawing per brush under `gate_draw = FALSE`
with no extra wiring. *Avoid*: inventing a third projection before a use
case demands it; “highlight mode” (it’s a projection of the same
selection, not a different selection).

**Reserved channels** — **LOCKED 2026-06-10**: The plotly interop owns
exactly three names: the plotly **`key` aesthetic** (minted per draw
inside the widget copy; a user formula’s own `aes(key = ...)` is
overridden with a one-time warning — selection is meaningless under
foreign keys), **`.ptr_row`** (the key column on the widget copy only;
never visible in either selection projection), and **`.ptr_selected`**
(the flag projection’s column — user-facing by design; formulas map it).
The two columns are documented reserved names and **silently
overwritten** on collision — overwrite is what keeps *chained*
selection-fed instances correct (instance N+1’s flag replaces the stale
flag riding in from instance N’s drawn data). All other per-instance
bookkeeping (source string, selection state, draw snapshot) stays
server-side, keyed by instance id — never in the data. *Avoid*: per-draw
collision warnings (warning spam under live draws); per-id flag column
names like `.ptr_selected_p1` (breaks formula portability across
instances).

**Draw** — **amended 2026-06-10**: One run of the runtime’s render body
(snapshot placeholders → eval → publish to `state$runtime()`). The
Update/Draw-all button is the *default* trigger (`gate_draw = TRUE`,
batch-on-click), **not part of the draw’s definition**: under
`ptr_options(gate_draw = FALSE)` the same body runs live, re-drawing on
any dependency change — including a **reactive pipeline head**
(`sel() |> ggplot(...)`), which makes a selection-fed formula redraw per
brush with no extra wiring. The draw button is an ergonomic default, not
a core invariant. *Avoid*: “ggpaintr only re-renders on the Update
click” as an unconditional claim (true only under the default
`gate_draw = TRUE`); treating `draw_trigger` as a general invalidation
hook (its implemented contract is a click-counter reactive — numeric ≥
1).

Accepted limitation: only identity-stat, point-like layers carry row
keys — selections on stat-transformed layers (`geom_smooth`, post-stat
bars) yield nothing.

### Naming convention — **LOCKED 2026-05-16**

> **`ptr_<x>` = L2 self-contained** (owns its own `.ptr-app` + asset
> bundle; drop into the host app; no shell to remember). **`ptr_ui_<x>`
> = L3 bare** (emits only its widgets; the L3 user supplies their own
> Shiny / shell).

This rule governs the entire api-audit rename. The earlier `ptr_embed_*`
idea is **dead** (superseded). The L2 self-contained pair is
**`ptr_ui`/`ptr_server`** (renamed from
`ptr_module_ui`/`ptr_module_server` by ADR 0006, which **voids** ADR
0004/0005’s sealed-bundle name exception). They are now regular
`ptr_<x>` self-contained names; `ptr_server` is the single public server
entry (no sibling “module” server exists, so the “module” qualifier was
dropped as misleading).

**L2 public surface** (self-contained, no `ptr_ui_` prefix): `ptr_ui` /
`ptr_server` (the L2 entry; `ptr_server` is also the single public
server entry at L3), `ptr_shared_panel` (cross-formula panel UI),
`ptr_shared` (coordinator) / `ptr_shared_server`. The
non-`ptr_ui`/`ptr_server` split `ptr_controls_ui`/`ptr_outputs_ui` is
**removed**.

**L3 public surface** (bare): `ptr_ui_controls`, `ptr_ui_plot`,
`ptr_ui_error`, `ptr_ui_code`, `ptr_ui_header`, `ptr_ui_shared_panel`,
plus the **combinators** (below). Pieces are **truly orthogonal** —
`ptr_ui_plot(id)` has no `error=`/`code_toggle=` flags; behavior is
added by combinators only. `ptr_ui_page(…, page, css)` is an **optional,
L3-only** shell convenience — never on the L2 path; L3 users may instead
write their own bare Shiny. It owns the one `.ptr-app` scope + (deduped
htmlDependency) asset bundle via internal `ptr_assets()`.

**Combinator** — **LOCKED 2026-05-16**: An `ptr_ui_*` function that
takes already-built bare pieces and returns them wired into a behavior.
Pure DOM-structure helpers; **no server coupling** (the server registers
`ptr_plot`/`ptr_error`/`ptr_code` regardless). The bundled JS
(`inst/www/ggpaintr-ui.js`) drives the toggle purely DOM-locally off
`.ptr-output` / `.ptr-code-toggle` / `.ptr-code-window` classes —
combinators just emit that markup. - `ptr_ui_inline_error(plot, error)`
— wraps a plot piece + error piece in the `.ptr-output` card so the
error renders inline. - `ptr_ui_toggle_code(plotish, code)` — wraps a
plot-ish tag + code piece so the code hides behind the `</>` toggle. -
**Nestable**:
`ptr_ui_toggle_code(ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)), ptr_ui_code(id))`. -
**Removed/superseded** by this model: the composites
`ptr_ui_code_toggle`, `ptr_ui_plot_code`, `ptr_ui_outputs`; the
`error=`/`code_toggle=` flags on `ptr_ui_plot`; ADR 0004’s locked
`ptr_ui_outputs ≡ ptr_ui_plot_code(error=TRUE)` contract; the
earlier-this-session `include_error_panel` rename + default-flip. ADR
0005 records the supersession. The default L2 layout (plot + inline
error + toggle code) lives **only** inside `ptr_ui`, composed from these
combinators.

**Asset escape hatch**: Exported `ptr_ui_assets(css)` — manual asset
injector, *only* for out-of-`ptr_ui_page`-contract roots (`navbarPage`,
bslib/BS5). Not part of normal L2/L3 use.

## Relationships

- An **L1** entry point internally composes the same pieces an **L3**
  user composes by hand; **L2** is the default-layout middle.
- A **shared placeholder** surfaces as a **shared section**
  (single-formula, inline in controls) *or* a **shared panel**
  (multi-formula, standalone, global ids) — never both for the same key
  on the same page.
- A **shared panel** pairs 1:1 with a top-level
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md);
  **bare pieces** / the **`ptr_ui` bundle** pair with the single
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  by matching `id`.
- The **partition rule decides *ownership*, not just placement**: the
  *one widget* for a key — including a `var (shared = …)` consumer’s
  column picker — is **rendered and server-bound by whoever owns its
  surface**. A **formula-local** key is owned by **that one instance’s
  own server**; a **panel** key (≥2 formulas) is owned by the
  **standalone panel + top-level
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)**.
  The host server owns **only** panel keys; it never reaches into a
  formula-local key. Single-instance has no panel, so the instance owns
  every shared key.
- The **page shell** contains **bare pieces**; an **embed composite** is
  (conceptually) a bare piece already wrapped in its own shell.

## Flagged ambiguities

- **“shared panel”** was used for *both* the inline single-formula
  section and the standalone multi-formula wellPanel. **Resolved**:
  these are distinct — **shared section** (inline,
  `render_shared_section`) vs **shared panel** (standalone, global ids,
  top-level server). Any design or doc must say which.
- **“L2 split” vs “L3 pieces”**: `ptr_controls_ui`/`ptr_outputs_ui`
  (self-wrapping composites) vs `ptr_ui_*` (bare). Earlier audit notes
  mislabeled cross-layer renames as mechanical. The redesign may delete
  the composite layer from L2 entirely.
- **“custom render path”** — **Resolved (ADR 0006, 2026-05-18)**: custom
  render is **L3 (UI-side)**, done by extracting components from the
  `state` returned by the single public
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md).
  It is not a server-side concept, not a separate level, and there is no
  user-wrapped engine.
- **“empty shared panel”** — a multi-instance app where every shared key
  is formula-local (no ≥2-formula key) leaves the panel with nothing to
  show. **Resolved (grill 2026-05-17)**: this is a **valid
  configuration, not an error** — the panel must render **nothing** (no
  empty “Shared controls” box, no orphan Draw-all); each instance still
  renders its own inline section, and the top-level
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
  stays a safe no-op.
- **“who binds a formula-local `var (shared=)`”** — the standalone-panel
  server was found also binding *formula-local* consumer pickers (global
  ids), contradicting “the owning instance binds it”. **Resolved (grill
  2026-05-17)**: the host server binds **only** panel keys; the owning
  instance binds every formula-local key — value, consumer, and source
  alike. One partition, one owner, no double-bind.
- **“is `stage_id` runtime-semantic?”** — ADR 0020’s *Out-of-Scope* note
  classified `stage_id` as “runtime-semantic — `disable_walk` reads it
  to drop stages” and on that basis forbade `stage_id` from the
  comparator-exclusion list, blocking Plan-03 SC7’s verb-side
  structural-equality invariant. **Resolved (ADR 0021, 2026-05-24)**:
  source inspection (`R/paintr-disable.R::is_stage_disabled`) shows
  `disable_walk` drops a stage iff `isFALSE(stage_enabled[[sid]])` —
  unbound reactives are no-ops. `stage_id` is a *UI-routing key*: it
  enables a UI-driven gate without changing baseline runtime behavior.
  It joins the exclusion list alongside `default_active` /
  `default_stage_enabled` / `has_user_control` / `stage_label`. SC7 is
  restored as a side effect of the `ppVerbOff` → `ppVerbSwitch`
  replacement.
- **“`ppVerbOff` vs `ppVerbControl` vs `ppVerbSwitch` naming and
  shape”** — ADR 0020’s *Out-of-Scope* note proposed a future
  `ppControl` placeholder with a *sibling-node* shape (control-only node
  parallel to the verb in the tree) to dodge the `stage_id` asymmetry.
  **Resolved (ADR 0021, 2026-05-24)**: the sibling-node proposal is
  rejected — the user-facing surface for any verb-stage UI control is
  wrapping (`ppVerbSwitch(verb, switch_on=, label=)`), not siblings
  (which cannot be expressed in surface R without inventing a non-pipe
  operator). One keyword (`ppVerbSwitch`) strictly subsumes `ppVerbOff`;
  `ppVerbOff` is hard-removed in the same change.

## Example dialogue

> **Embedder:** “I’ll drop
> [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
> into each module’s sidebar so every plot gets the shared control.”
> **Maintainer:** “No — the **shared panel** uses coordinator-namespaced
> ids built by `ptr_shared(..., id = ...)`; two copies of the *same*
> coordinator collide because they share an `id`. If you genuinely need
> two independent shared groups on one page, build two coordinators with
> different `id`s and emit a `ptr_shared_panel(obj)` for each. If you
> only have *one* formula, you don’t want the panel at all — you want
> the inline **shared section** (`render_shared_section = TRUE`).”
