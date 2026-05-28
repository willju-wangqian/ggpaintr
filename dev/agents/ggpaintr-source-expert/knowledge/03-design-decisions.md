# Design decisions — ADR digest

One paragraph per ADR. Status, date, one-line decision, and what it supersedes/voids. For load-bearing claims, open the ADR itself (`dev/adr/<file>`) — these digests are navigation aids, not verbatim.

> ⚠ **ADR-status vs code-state.** Each ADR carries a `Status:` line in its own meta block — that is *the ADR document's* status at draft time, not necessarily the state of the code. Many ADRs say `Proposed` even after the code change has merged (the ADR doc is rarely updated post-merge). When this digest claims an ADR is "MERGED" / "FIXED" / "LOCKED", that claim is grounded in **git log, CONTEXT.md, or auto-memory**, not in the ADR's own status field. Both are noted where they diverge.

Grouped by theme so you can find the right one without scanning numerically. The numerical index is at the bottom.

## A. UI shell, layout, theming

### ADR 0001 — Polish the default `fluidPage` UI with a bundled stylesheet
*Status: accepted · Date: 2026-05.* The default `ptr_app()` UI stays on Shiny's `fluidPage`; visual polish is delivered through a bundled stylesheet (`inst/www/ggpaintr.css`), not by making `bslib` the default. Rationale: `bslib` is offered as the `ptr_app_bslib()` wrapper for users who want it; keeping the core dependency-light keeps install cheap and avoids version-coupling on `bslib`.

### ADR 0003 — Controllable-region UI abstraction
*Status: accepted · Date: 2026-05-15.* Three separate sites in the UI emitted the same "checkbox above a region greys out the region and removes its contribution at eval time" pattern. Consolidated into one helper (`controllable_region` in `R/paintr-region.R`) used by all three. Pure DRY refactor — no behavior change for users.

### ADR 0007 — Bare pieces vs. host-theme compatibility
*Status: **deferred — NO decision made** · Date: 2026-05-19.* The question (should bare `ptr_ui_*` pieces play nicely with arbitrary bslib / BS5 host themes, or should they enforce the bundled stylesheet's classes?) was raised, discussed, and **not** resolved. ADR 0001 / 0005 / 0006 still hold; the asset-bundle question for L3 hosts stays open. If a question arises about "how do bare pieces interact with bslib theming", the answer is "the project hasn't decided yet."

## B. Public API tiering & rename history

### ADR 0002 — Public API tiering and the deliberately un-exported / renamed functions
*Status: accepted · Date: 2026-05.* The "L1 / L2 / L3" tiering was first formalised here. Established the *portal-smell* guard: an exported function whose runtime companions are internal is a leaky abstraction (later honored when `state$server_ns_fn`/`ui_ns_fn` were kept internal). Most rows in 0002 are now superseded by later ADRs, but the principle (*if the user has to reach into ggpaintr's internals to make X work, X belongs in public API*) still governs design.

### ADR 0004 — L3 UI-piece model, single shell, and the assets-internal supersession
*Status: accepted 2026-05-16 — **SUPERSEDED IN FULL by ADR 0005**.* The first attempt to formalize L3 as a fixed list of named pieces with locked composition contracts (`ptr_ui_outputs ≡ ptr_ui_plot_code(error=TRUE)`, etc.). Replaced 1 day later by 0005's "bare pieces + combinators" model. Don't follow 0004's contracts in new code.

### ADR 0005 — L2/L3 redesign: self-contained vs bare convention, shared coordinator, output combinators
*Status: accepted 2026-05-16 — **SUPERSEDED IN PART by ADR 0006** (2 days later).* Established three load-bearing decisions: (1) `ptr_<x>` = L2 self-contained, `ptr_ui_<x>` = L3 bare (the **naming convention still in force**); (2) shared partition model and coordinator; (3) orthogonal output pieces wired into behavior by nestable combinators (`ptr_ui_inline_error`, `ptr_ui_toggle_code`). What 0006 superseded: the §2 "L3 = own the render path" framing, the `ptr_module_*` name exception, and the "bare `ptr_server` inside your own `moduleServer`" custom-render pattern.

### ADR 0006 — Single public server entry; L3 is UI-side only; `ptr_module_*` rename
*Status: accepted · Date: 2026-05-18.* **The current public-API contract.** Three decisions: (1) exactly one public server function — `ptr_server(formula, id, …)` (formerly `ptr_module_server`); the 4-arg engine becomes the unexported `ptr_server_internal`. (2) L3 is **UI-side only** — there is no L3 server pattern; the server is always the single `ptr_server` at L2 and L3. (3) Custom render = extract from `state$runtime()` returned by `ptr_server`. The "module" qualifier in the old names was misleading (no sibling non-module server ever existed) and is dropped. **Read this one before recommending any public API shape.**

## C. The `pp*` placeholder grammar

### ADR 0008 — No `exprs` placeholder; placeholders stay bounded
*Status: **rejected — decision made** · Date: 2026-05-19.* Should ggpaintr add an `exprs(...)` placeholder that accepts a *list* of expressions (analogous to `rlang::exprs`)? Rejected. Placeholders stay bounded — one widget per placeholder — and a user who needs a list of inputs writes multiple placeholders. The freedom ceiling stays where ADR 0005 / 0006 set it.

### ADR 0009 — pp-prefixed placeholders, expression-mode input, and registry-declared argument schema
*ADR status: "proposed — implementation pending" · Date: 2026-05-20 · **Code state: MERGED 2026-05-21** (per auto-memory `project-adr9-merged`; commit `2ee656b`).* The big placeholder-grammar reform. Three things: (1) renamed all keywords with a `pp` prefix — `var`→`ppVar`, `text`→`ppText`, `num`→`ppNum`, `expr`→`ppExpr`, `upload`→`ppUpload`. **The old un-prefixed names are no longer in the registry**, so a formula that uses them still *parses* as ordinary R but produces zero placeholder nodes — see pitfall F.4 for the three downstream failure modes (silent wrong, eval error, semantic garbage), and the explicit empirical verification that bare/call/seeded forms of the `pp*` names are all recognized. (2) `ppExpr` now accepts a user-typed expression at runtime (the "expression-mode" input — closes the "user can type their own ggplot fragment" feature). (3) Registry entries declare their argument schema so the constructors can validate at registration time. Closely paired with ADR 0010.

### ADR 0009-edge-cases — Companion edge-case decisions for ADR 0009
*Status: notes · Date: 2026-05-20.* Companion document recording edge cases ADR 0009 was silent or ambiguous on — each case lists candidate behaviors with a "Recommended" pick. After the user decided each, the resolutions folded back into ADR 0009. Read this only if a follow-up to ADR 0009 is unclear from 0009 alone.

### ADR 0010 — `ppUpload(name=)` default-arg + identity-outside-app evaluation
*ADR status: "ACCEPTED — promoted from DEFERRED" · Date: 2026-05-21 · **Code state: merged** (per branch history).* Promoted from DEFERRED via /decision-to-plan session. Two-part: (1) `ppUpload` takes an optional `name=` arg that defaults to an auto-name when the user doesn't supply one (the basis for the source-shortcut text widget's auto-population). (2) When `ppUpload(...)` is evaluated outside `ptr_app()` (naked-R semantics), the function returns its first arg unchanged so the formula reads valid R either way. Sets up the world for ADR 0024 / ADR 0025.

### ADR 0014 — Placeholder-registry init ordering for custom-placeholder constructors
*Status: **DECIDED & APPLIED** · Date: 2026-05-23.* The three public placeholder constructors (`ptr_define_placeholder_value`, `_consumer`, `_source`) bypassed the package's own load-ordering and could land before `.ptr_registry` was populated. Added `ensure_registry_initialized()` shim that the constructors call, guaranteeing registration-order safety. Supersedes the temporary workaround in `tests/testthat/fixtures/vignette-apps/super-1-kitchen-sink/app.R:10–15`.

## D. Tree shape, canonicality, intermediate-stage primitives

### ADR 0011 — Bug 3a / 3b handoff: pipeline-internal `ppVar` cols + `load_all` S3-dispatch trap
*Status: **SUPERSEDED in part** — diagnosis of bug 3b was wrong · Date: 2026-05-21.* Historical investigation log. The diagnosis was incorrect; the live structural fix is ADR 0012. Read 0011 only if you need the original bug 3a/3b context. **Always cross-reference with 0012.** The `lapply(x, bare-namespace-generic)` S3-dispatch trap surfaced here is real (auto-memory `pipeline-head-data-source`); the diagnosis of bug 3b was not.

### ADR 0012 — Role-based tree, terminal-grounded pipeline lift, and `ptr_spec`
*Status: **DRAFT — design locked, implementation not started** · Date: 2026-05-21.* **The canonical-tree ADR.** One-line summary: *the tree currently leaks surface syntax* — `|>` vs `%>%` vs nested-call produce three structurally different trees, which they shouldn't. Locks the **canonical typed tree**: the same logical pipeline parses to the same tree regardless of surface syntax; pipe shape is preserved as metadata, not as tree shape. Establishes `ptr_tree_structural_equal` as the comparator and the rule that *narrowings preserving a surface-syntax divergence violate this ADR* (auto-memory `feedback-tree-is-semantic-not-syntactic`). Foundation for ADRs 0018, 0019, 0021.

### ADR 0018 — Eliminate per-pass S3 walker boilerplate (C3a)
*Status: **proposed** · Date: 2026-05-24.* Each mutating pass (classify, prune, substitute, render) re-implements a full S3 dispatch tree of 11–13 methods, ~80% of which is identical "recurse into children, rebuild node" boilerplate. Proposal: factor that out into reusable walker primitives so adding a new node type touches one file instead of four. The walker primitives in `paintr-walk.R` already exist; this ADR proposes the four mutating passes migrate to them. Interface design + migration plan deferred to a fresh session.

### ADR 0019 — Public staged pipeline (drop the 222 :::) (C3c)
*Status: **proposed** · Date: 2026-05-24.* Tests use `ggpaintr:::` 222 times across 21 files because the staged-pipeline primitives (`ptr_classify_data`, `ptr_prune`, `ptr_substitute`, `ptr_render`) are unexported — yet they're stable contracts in the project's internal vocabulary. Proposal: expose a `ptr_pipeline()` (or similar) staged primitive that returns the tree at a chosen stage. Independent of ADR 0018; either can land first.

## E. Structural keywords — layer/stage toggles

### ADR 0020 — `ppLayerOff` / `ppVerbOff`: source-mode checkbox toggles
*Status: **partially superseded by ADR 0021** · Date: 2026-05.* Introduced two structural keywords for checkbox toggles in the source formula: `ppLayerOff(layer, hide=)` (toggles an *existing* layer checkbox) and `ppVerbOff(verb)` (drops a verb when off). The `ppLayerOff` half stands. The `ppVerbOff` half was replaced by `ppVerbSwitch` in ADR 0021. Other surviving pieces: the `default_active` / `default_stage_enabled` reader sites, the `checkbox_defaults =` deprecation, the precedence ladder.

### ADR 0021 — `ppVerbSwitch`: user-controlled UI checkbox for any pipeline verb
*ADR status: "Proposed. Drafted 2026-05-24" · Date: 2026-05-24 · **Decision state: LOCKED 2026-05-24** per CONTEXT.md (the project lock is at the decision level; the ADR doc was not re-stamped to "accepted").* Replaces `ppVerbOff` with `ppVerbSwitch(verb, switch_on = FALSE, label = "…")`. Two-part decision: (1) `ppVerbSwitch` *creates* a stage checkbox on a placeholder-free verb (with custom label) — the only way to get a checkbox where there isn't one. (2) **`stage_id` is reclassified from "runtime-semantic" to "UI-routing key"** and added to the structural-equality comparator-exclusion list. Restores the verb-side structural-equality invariant via wrapping (not via the sibling-node shape `ppControl` ADR 0020 *Out-of-Scope* note had floated, which was rejected).

### ADR 0022 — Spec panel replaces preserve-mode formula view
*Status: **proposed** · Date: 2026-05-25.* The Code panel's "Show placeholders" radio toggle (preserve-mode) was attempting to serve an audience that does not exist (a non-owner who wants to clone the app). It cannot honestly reproduce the app under structural keywords — `ppLayerOff` / `ppVerbSwitch` never appear in the typed tree, so the preserve-mode renderer cannot emit them. Decision: soft-land preserve-mode out of the UI; introduce a `spec` panel instead — `spec` is the deparseable state of one running instance, owned by the app owner for cross-session persistence. Implementation pending.

## F. Shared coordinator, multi-instance, source binding

### ADR 0013 — Super-app pressure-test suite for placeholder × host × capture-mode interactions
*Status: **proposed** · Date: 2026-05-23.* After ADRs 0009 and 0010 merged, the placeholder grammar's combinatorial space exploded (5 placeholders × 3 hosts × 2 capture modes × shared/local × pipeline/aes/data positions). The "super-app" fixtures (`tests/testthat/fixtures/vignette-apps/super-*-*`) are the pressure-test suite for these interactions. ADR records the methodology and per-app role.

### ADR 0015 — Consumer picker binding under source-headed upstream
*Status: **decided** · Date: 2026-05-23.* When a consumer's upstream contains a data-source placeholder (`ppUpload`, `ppSample`, etc.), two parallel defects kept the consumer's column-picker `renderUI` from binding: the non-shared site suspended the picker on visibility, and the source-headed resolution skipped a pre-warm exclusion. Fixed both sites. Supersedes the fix-super1 pre-warm exclusion at `R/paintr-server.R:1685`.

### ADR 0016 — All super-app examples must be Path-B (naked-ggplot) evaluable
*Status: **proposed — implementation deferred** until all ADR 0013 plans close · Date: 2026-05-24.* Locks the principle: every `pp*` placeholder call inside a super-app formula must successfully evaluate as plain R (where each `pp*` is identity on its first arg). The "same formula works inside and outside `ptr_app()`" is a central ggpaintr value proposition; super-apps that violate it surface as Path-B failures. Two breakers identified at draft time: `super-4`'s `ppColor()` (no arg) and `super-2a`'s `ppMultiVar()` (no arg).

### ADR 0023 — Panel-owned shared sources end-to-end
*ADR status: "Proposed. Drafted 2026-05-25" · Date: 2026-05-25 · **Code state: code fix landed 2026-05-22 (per auto-memory `project-shared-section-binder`); the ADR was drafted after the fix to lock in the partition-discipline rule the fix established.*** Closes the host-vs-instance ownership question for shared sources. The partition rule decides ownership, not just placement: panel keys (≥2 formulas) are bound **only** by the host's `ptr_shared_server`; formula-local shared keys are bound **only** by the owning instance's `ptr_server`. The earlier "id-mismatch" theory was disproved; the fix was partition discipline applied consistently across the 3 hosts. (See `04-invariants.md` III.2.)

### ADR 0024 — Source companion as data-loading entry point
*Status: accepted · Date: 2026-05.* The shortcut `textInput` sibling of a `ppUpload` source placeholder is reclassified from "decorative shortcut" to the *primary data-loading entry point* for textually-named sources (`<auto-name> <- read.csv("…")`). The companion textbox carries the source's identity in the rendered code's prologue (`emit_upload_prologue`). Foundation for ADR 0025's auto-name + mutex.

### ADR 0025 — Source shortcut rename + upload auto-name + mutex-with-autoclear
*ADR status: "Proposed. Drafted 2026-05-26 from a /grill-with-docs session on branch `shared-id-upgrade`. Awaits implementation." · Date: 2026-05-26 · **Code state: MERGED to the current branch** (`git log` shows `642ea83 Merge shared-id-upgrade: ADR 0025 source-shortcut rename + upload auto-name + mutex + multi-host panel-shared support` — i.e. the implementation landed despite the ADR doc not being re-stamped).* Three intertwined changes: (1) **source shortcut rename** — the source-companion text widget got a stable naming scheme. (2) **upload auto-name** — when the textbox is empty and a file is uploaded, the snapshot is overwritten with the file's auto-name (§6 round-trip fallback in `ptr_setup_runtime` at `R/paintr-server.R:1781-1813`). (3) **Mutex with autoclear + the A2 race fix** — file picked wipes textbox; text typed resets fileInput via JS round-trip; the JS round-trip races with the bind unless the mutex is gated on the 400ms-debounced reactive (§7 A2 at `R/paintr-server.R:1546-1584`). This is the most intricately reactive ADR in the catalog. (See `04-invariants.md` VII.2 / VII.3.)

## G. Refactoring catalog

### ADR 0017 — Deepening opportunities catalog
*Status: catalog · Date: 2026-05.* A running list of "we should refactor / consolidate" opportunities in the codebase, scored by impact and effort. The C3 family of candidates (C3a — walker boilerplate; C3b — tree subtypes; C3c — public staged pipeline) was promoted from here. New refactoring ADRs typically start as a row in 0017 and graduate to their own ADR.

---

## Supersession & cross-reference map

```
0004 ────────────► 0005 ────► 0006
                     │            │
                     │            └──► current public-API contract
                     │                 (single ptr_server, L3 = UI-only,
                     │                  custom render = state$runtime())
                     └──► naming convention (ptr_<x> / ptr_ui_<x>)
                          + partition + combinators STILL LIVE

0011 ─────────────► 0012  (0011 diagnosis wrong; 0012 is the live tree design)

0020 (ppLayerOff stays) ────► 0021 (ppVerbSwitch supersedes ppVerbOff
                                     + stage_id reclassified as UI-routing key)

0009 + 0009-edge-cases + 0010  →  pp* grammar + ppUpload(name) (MERGED 2026-05-21/22)

0024 + 0025  →  source-companion identity + auto-name + mutex-with-autoclear (MERGED)

0023  →  partition decides ownership (FIXED 2026-05-22)

0017  ──promotes──►  0018 (C3a), 0019 (C3c)
```

## Numerical index

| ADR | Topic | Status (2026-05-27) |
|---|---|---|
| 0001 | Polish fluidPage with bundled stylesheet | accepted |
| 0002 | Public API tiering | accepted (mostly superseded by later) |
| 0003 | Controllable-region UI abstraction | accepted |
| 0004 | L3 UI-piece model (first attempt) | **SUPERSEDED by 0005** |
| 0005 | L2/L3 redesign + naming convention | accepted (partly superseded by 0006) |
| 0006 | Single public server entry; L3 = UI-only | **current public-API contract** |
| 0007 | Bare pieces vs host-theme compatibility | **DEFERRED — no decision** |
| 0008 | No `exprs` placeholder | **REJECTED** |
| 0009 | pp* prefix + expression-mode input | ADR: proposed · **Code: MERGED 2026-05-21** |
| 0009-edge-cases | Companion edge cases | folded into 0009 |
| 0010 | `ppUpload(name=)` + naked-R semantics | ADR: ACCEPTED 2026-05-21 · Code: merged |
| 0011 | Bug 3a/3b handoff | **SUPERSEDED in part by 0012** |
| 0012 | Role-based canonical tree + ptr_spec | **DRAFT — design locked** |
| 0013 | Super-app pressure-test suite | proposed |
| 0014 | Registry init ordering | **DECIDED & APPLIED** |
| 0015 | Consumer picker / source-headed upstream | decided |
| 0016 | Super-app examples Path-B evaluable | proposed (deferred) |
| 0017 | Deepening opportunities catalog | living catalog |
| 0018 | Eliminate per-pass walker boilerplate (C3a) | proposed |
| 0019 | Public staged pipeline (drop 222 :::) (C3c) | proposed |
| 0020 | `ppLayerOff` / `ppVerbOff` | partly superseded by 0021 |
| 0021 | `ppVerbSwitch` | ADR: proposed 2026-05-24 · **Decision: LOCKED 2026-05-24** per CONTEXT.md |
| 0022 | Spec panel replaces preserve-mode | proposed 2026-05-25 |
| 0023 | Panel-owned shared sources | ADR: proposed 2026-05-25 · **Code fix landed 2026-05-22** |
| 0024 | Source companion = data-loading entry | accepted |
| 0025 | Source shortcut + auto-name + mutex | ADR: proposed 2026-05-26 · **Code MERGED** (commit `642ea83`) |
