---
name: lazy-consumer-resolve
type: decision
status: accepted
scope: [runtime, ui, registry, consumer-placeholders]
created: 2026-05-08
---

# Lazy Consumer Resolve — Drop the Update-Data Button

## Understanding Summary

- **What:** Remove the per-layer "Update Data" button entirely. Replace its role in the reactive graph with a per-consumer reactive cache whose dependency set covers the consumer's own `node$upstream` subtree. Producer (`text` / `num` / `expr`) inputs flow through a dynamically-windowed `shiny::debounce` that auto-flips on/off based on measured upstream-resolve time.
- **Why:** The Update Data button is friction. Users must click it before downstream pickers/sliders reflect their edits, and the staleness rules added complexity (`update_data_stale_class`, `last_click_inputs` reactiveVal, per-button observer). Removing it without regression requires the runtime to refresh data-aware widgets at the right moments without adding new buttons or per-widget refresh icons. Pure-Shiny reactive cascade gives correct semantics, and the auto-debounce closes the keystroke-churn risk on slow data.
- **Who:** End users authoring ggplot formulas in `ptr_app()` and embedders writing custom data-aware placeholders via `ptr_define_placeholder_consumer()`. Custom consumers inherit the new behavior automatically — registry constructor already stamps `data_aware = TRUE`; framework-level wiring in `ptr_setup_consumer_uis()` is the single change point.
- **Constraints:**
  - Pure Shiny — no custom JS, no `focusin`, no `shiny::onevent`-style focus handlers.
  - Pre-warm at startup so consumers are populated on first paint (no empty-dropdown flicker on first Controls-tab visit).
  - Producer keystrokes must not cause perceptible UI lag on typical (in-memory tabular) data; auto-debounce kicks in only when measured eval time exceeds threshold.
  - Backward-compat for custom consumers in the wild = none today (rewrite branch); single breaking change to `build_ui` signature `(node, cols)` → `(node, cols, data)` documented in changelog.
- **Non-goals:**
  - No focus / JS-event-based triggers (`focusin`, `shown.bs.dropdown`, custom input bindings) — pure reactive graph + tab activation suffices.
  - No per-consumer refresh button (rejected — regression on the goal of removing the Update Data button).
  - No optimization for database-backed / multi-second upstream resolutions in v1; `ptr_app(producer_debounce_ms = ...)` is the manual escape hatch.
  - No change to bare-data-source layers (`ggplot(mtcars, aes(x = var))`) — they already reactively resolve via `is_bare_data_source_layer` branch in `ptr_setup_pipelines`.

## Decision

### D1 — Remove the Update Data button

Delete every artifact tied to the per-layer Update Data button:

- `update_data_input_id` field stamping in `R/paintr-ids.R:54` and the `is_pipeline_data_arg(layer$data_arg)` gate at `:106`.
- The button's UI rendering in `R/paintr-build-ui.R:143–149`.
- The `observeEvent(input[[uid]], …)` block in `R/paintr-server.R:218` and the surrounding seed/snapshot bookkeeping at `:196–215`.
- `last_click_inputs` reactiveVal and its initialization in `R/paintr-server.R:81–84`.
- Input-spec role `"layer_update_data"` in `R/paintr-input-spec.R:46–50`.
- Copy entries `update_data_button` and `update_data_stale_class` in `R/paintr-copy.R:11`, `:44–46`, `:320–337`.
- `update_data_label` and `update_data_stale_class` references in `R/paintr-build-ui.R:147`, `:315`.

Bare-data-source layers continue to auto-resolve reactively via the existing branch at `R/paintr-server.R:73,167–192`.

### D2 — Per-consumer reactive cache (Data tab)

Replace the single-shared `cols_memo` in `R/paintr-server.R:512–517` with per-consumer reactives keyed by `node$id`. Each consumer's reactive depends on:

1. `state$tree()`, `state$stage_enabled()` (structural changes)
2. **Each consumer-input value** (`input[[id]]`) for every `ptr_ph_data_consumer` whose `node$id` appears anywhere in this consumer's `node$upstream` subtree. Consumer commits fire on selection only — no churn.
3. **Each producer-input value** for every `ptr_ph_value` (`text` / `num` / `expr`) whose `node$id` appears in this consumer's `node$upstream`, **read through the dynamically-windowed debounce wrapper** (D7).
4. Tab-activation events: `input[[ns("ptr_layer_select")]]` (outer layer picker) plus the per-layer inner `tabsetPanel` selection input (Data / Controls).
5. Update Plot click (`input[[ns("ptr_update_plot")]]`).

When the cache invalidates, `output[[consumer_output_id(raw_id)]] <- renderUI(...)` re-fires and `build_ui(node, cols, data)` is called with freshly resolved upstream.

Static analysis helper required: `find_input_ids_in_upstream(node$upstream)` — walks the upstream subtree and returns `(consumer_ids, producer_ids)` lists. Lives in `R/paintr-server.R` next to `runtime_upstream_cols`.

### D3 — Controls tab consumers

Same per-consumer cache mechanism, but the dep set excludes producer-input values entirely. Producers feeding controls-tab consumers live on the Data tab and the user must switch tabs to edit them — tab activation already covers it. Deps:

1. `state$tree()`, `state$stage_enabled()`
2. Tab-activation event for the Controls tab
3. Update Plot click

### D4 — Pre-warm at startup

In `ptr_setup_consumer_uis()`, immediately after building the per-consumer reactives, force one synchronous evaluation per consumer using `shiny::isolate({ ... })` + a default-empty input snapshot (or the seed snapshot from `ptr_resolve_upstream(snapshot = list(), …)`). This populates picker choices before the user interacts with any tab. Reuses the existing seed logic from the deleted `R/paintr-server.R:202–212` block.

### D5 — Update Plot is the global trigger

Unchanged from current behavior. `input[[ns("ptr_update_plot")]]` invalidates every consumer's cache and re-runs the runtime observer that renders plot + code (`paintr-server.R:317–320`). With per-consumer caches now in place, "global invalidate on Update Plot" means iterating consumers and bumping a session-scoped reactiveVal that's in every consumer's dep set.

### D6 — Extend consumer `build_ui` contract

Change `R/paintr-registry.R:160` from:

```r
validate_hook(build_ui, "build_ui", c("node", "cols"))
```

to:

```r
validate_hook(build_ui, "build_ui", c("node", "cols", "data"))
```

Update `runtime_upstream_cols` (`paintr-server.R:453–481`):

- Rename to `runtime_upstream_data` (returns per-consumer `list(cols = ..., data = ...)`).
- Drop the `state$resolved_data[[layer_name]]` shortcut — it's tied to the now-removed Update Data click. All consumers go through `ptr_resolve_upstream(c$upstream, snapshot = current_inputs, …)`.
- Cache result in `state$upstream_cache` (already deduplicates per-subtree, so two consumers with shared upstream share storage).

Update `ptr_setup_consumer_uis()` (`paintr-server.R:496–549`):

- `invoke_build_ui` call at `:539–546` now passes `extra = list(cols = cols, data = data, selected = current %||% character(0))`.

Update built-in `var` consumer (`R/paintr-builtins.R:169` → `ptr_builtin_var_build_ui`): accept `data` arg and ignore it. One-line change.

Update test fixtures using `ptr_define_placeholder_consumer` to add the `data` arg.

### D7 — Producer-input debounce

In `ptr_setup_pipelines()` (or a new `ptr_setup_producer_inputs`), every `ptr_ph_value` (`text` / `num` / `expr`) input is wrapped:

```r
state$producer_input[[id]] <- shiny::debounce(
  shiny::reactive({ input[[ns(id)]] }),
  millis = function() state$producer_debounce_ms()
)
```

Per-consumer reactives read `state$producer_input[[id]]()` instead of `input[[ns(id)]]` directly. Consumer-input reactives (pickerInput etc.) are NOT debounced — commits are intentional and one-per-click.

### D8 — Auto-flip mechanism

State held on `state` (per session):

```r
state$producer_debounce_ms  <- shiny::reactiveVal(0L)
state$slow_count            <- 0L  # plain int, not reactive
state$fast_count            <- 0L
```

In `runtime_upstream_data`, wrap the `ptr_resolve_upstream` call:

```r
t0 <- Sys.time()
df <- ptr_resolve_upstream(...)
elapsed_ms <- as.numeric(Sys.time() - t0, units = "secs") * 1000
record_eval_time(state, elapsed_ms)
```

Constants (internal; not exposed as `ptr_app` args):

```r
SLOW_THRESHOLD_MS         <- 150
FAST_THRESHOLD_MS         <- 80
CONSECUTIVE_SLOW_REQUIRED <- 3
CONSECUTIVE_FAST_REQUIRED <- 5
DEFAULT_DEBOUNCE_WINDOW   <- 300
```

`record_eval_time(state, elapsed_ms)` logic:

```
if state$producer_debounce_ms() == 0 (off):
  if elapsed_ms > SLOW_THRESHOLD_MS:
    state$slow_count <- state$slow_count + 1
    if state$slow_count >= CONSECUTIVE_SLOW_REQUIRED:
      state$producer_debounce_ms(DEFAULT_DEBOUNCE_WINDOW)
      state$slow_count <- 0
      state$fast_count <- 0
      maybe_inform_on()
  else:
    state$slow_count <- 0
else (on):
  if elapsed_ms < FAST_THRESHOLD_MS:
    state$fast_count <- state$fast_count + 1
    if state$fast_count >= CONSECUTIVE_FAST_REQUIRED:
      state$producer_debounce_ms(0L)
      state$fast_count <- 0
      state$slow_count <- 0
      maybe_inform_off()
  else:
    state$fast_count <- 0
```

Skip the very first eval per session (cold-start bias) — gate via a `state$first_eval_done` flag.

Asymmetric thresholds (`FAST < SLOW`) and asymmetric counts (`5 fast > 3 slow`) provide hysteresis to prevent flap on values near the boundary; prefer staying debounced when perf is borderline.

`maybe_inform_on()` / `maybe_inform_off()`:

```r
if (ptr_get_setting(ptr_settings$verbose)) {
  cli::cli_inform("Producer debounce auto-enabled (300 ms): slow upstream resolution detected.")
}
# and the corresponding "auto-disabled" message for off
```

User override: extend `ptr_app(...)` and `ptr_server(...)` (via `ptr_server_state`) with new arg `producer_debounce_ms = NULL`.

- `NULL` (default) → auto mode (D8 runs).
- `0L` → manual force-off; auto logic skipped.
- Integer `>0` → manual force-on at that window; auto logic skipped.

### D9 — Verification spike S0 (precondition)

**Status: PASSED by source inspection (2026-05-08).** Reading `shiny::debounce` source confirmed: (1) `millis` accepts a function; (2) `millis()` is re-read on every upstream invalidation inside the internal tracker observer, so the window is dynamic at runtime; (3) the returned reactive `er` is constructed once and reused — identity stable across window toggles. Minor caveat: a pending timer does not re-arm on window change; a single trailing emission may lag by the old window after auto-flip. Acceptable.

Probe script kept at `dev/scripts/probe-debounce-dynamic-millis.R` for optional interactive sanity-check.

Original verification protocol below, retained for traceability:

Before touching any production code, write a 15-line standalone Shiny app that confirms `shiny::debounce(r, millis = function() ...)` honors a runtime-changing window without recreating the reactive:

- One `numericInput` driving `reactive({ input$x })`.
- Wrap in `shiny::debounce(r, millis = function() debounce_window())` where `debounce_window` is a `reactiveVal(0L)`.
- Two buttons: "set 0", "set 500".
- Confirm: with window 0, every keystroke fires; with window 500, rapid typing yields one emission ~500 ms after the last keystroke; toggling back to 0 resumes immediate firing.

If the spike confirms the assumption, proceed. If `millis = function()` is not honored at runtime, fall back to a hand-rolled per-input debounce helper using `shiny::invalidateLater` + a per-input timer reactiveVal — same external semantics, ~30 more lines.

## Alternatives Considered

### Alt A — Keep button + add reactive cascade alongside

Live cascade fires on producer changes; button still gates "commit" semantics. Rejected: doubles complexity, doesn't deliver the friction-removal goal that motivated the brainstorm.

### Alt B — Per-consumer refresh button

Each data-aware consumer ships with a small refresh icon next to its widget. Click resolves upstream and updates choices. Rejected: re-introduces the friction the Update Data button was creating, just at finer granularity. N small buttons is worse than one big one in click-count terms, and the visual noise scales with consumer count.

### Alt C — Lazy-on-open via custom JS / `focusin`

Custom Shiny input binding that listens for `focusin` on the consumer container; on fire, sends a server message that triggers per-consumer resolve. Rejected: discoverability problem (users don't know clicking opens triggers a resolve), keyboard-navigation gaps, requires JS resource hygiene (`htmlDependency`), and adds a non-Shiny code surface to maintain. The reactive-graph approach (D2) achieves the same effect (intra-tab cascade) using only Shiny primitives.

### Alt D — Commit-on-blur for `text` / `num` / `expr`

Custom Shiny input binding that fires the input on `change` / `blur` instead of `input`. Rejected: requires custom JS, not a one-liner in base Shiny; `shiny::debounce` is the official tool for coalescing rapid input changes. The auto-flip debounce (D8) provides the same UX guarantee (no per-keystroke lag on slow data) without any custom JS.

### Alt E — No debounce, accept pay-per-keystroke

Producers keystroke-fire downstream resolves directly. Rejected as default: works fine for in-memory mtcars-sized data but degrades visibly on 1M-row data or slow custom verbs. Auto-flip (D8) makes "no debounce" the default for typical small data and engages debounce only when needed.

### Alt F — One-shot auto-flip (debounce stays on once enabled)

Original proposal in the brainstorm. Rejected: a transient slow eval (GC pause, brief network hiccup if upstream is remote) permanently switches every producer to 300 ms even if the rest of the session is fast. Symmetric flip-flop with hysteresis (D8) handles users who simplify their pipeline mid-session and tolerates transient slow evals via the consecutive-counter requirement.

### Alt G — Static `ptr_app(producer_debounce_ms = X)` knob, no auto-mode

Manual override only; no automatic flip-flop. Rejected: forces formula authors to know in advance whether their data is slow. Auto mode is the right default; manual override is the escape hatch.

## Acceptance Criteria

### Behavior

1. **No Update Data button rendered, anywhere.** Every layer panel containing a data pipeline shows pipeline placeholder widgets followed directly by either the Controls tabsetPanel or the layer's other content — no button, no copy entry referenced.
2. **Pre-warm populates pickers on first paint.** Loading `ptr_app("ggplot(mtcars, aes(x = var))")` shows `var`'s picker with all column choices visible before any user interaction.
3. **Intra-pipeline cascade.** For `mtcars |> select(var1) |> select(var2)`, picking a column for `var1` updates `var2`'s available choices on the next Shiny flush. No button click required.
4. **Producer cascade for data-receiving consumers.** For a future slider consumer over upstream `mtcars |> filter(num_threshold > num)`, changing `num` updates the slider's min/max range on the next Shiny flush (debounced per D8 if in auto-on mode).
5. **Controls-tab activation refresh.** For a layer with both Data and Controls panels, switching to Controls re-resolves and re-renders Controls-tab consumers using the current Data-tab input snapshot.
6. **Update Plot global refresh.** Clicking Update Plot resolves every layer's pipeline end-to-end, refreshes consumer caches (any tab), updates plot, updates code panel.
7. **Bare-data-source layers unchanged.** `ggplot(mtcars, aes(x = var))` continues to behave identically to current main (reactive resolve via existing `is_bare_data_source_layer` branch).

### Auto-flip (D8)

8. After 3 consecutive upstream resolutions exceed 150 ms (with auto mode on), `state$producer_debounce_ms` flips to 300 and `slow_count` resets to 0.
9. After 5 consecutive resolutions under 80 ms with debounce on, the window flips back to 0 and `fast_count` resets to 0.
10. A single slow eval mid-streak resets `slow_count` to 0 (strict consecutive). Symmetric for fast side.
11. The first eval of the session is excluded from the counter.
12. Manual override `ptr_app(producer_debounce_ms = N)` skips the auto logic entirely and pins the window at `N`.
13. Verbose-gated `cli::cli_inform()` fires on each flip when `ptr_options(verbose = TRUE)`; silent otherwise.

### Consumer contract (D6)

14. `ptr_define_placeholder_consumer(build_ui = function(node, cols, data) ...)` registers without error.
15. The built-in `var` consumer's `build_ui` accepts the new `data` arg (and ignores it).
16. A test fixture custom consumer that uses `data` to compute `min`/`max` from a numeric column receives the resolved upstream df.

### Test coverage

17. Unit test: per-consumer reactive cache invalidates on the right deps and not on producer keystrokes when window is `0`.
18. Unit test: `runtime_upstream_data` returns `list(cols, data)` per consumer, sharing storage when subtrees match.
19. Unit test: auto-flip on/off transitions using an injected fake clock + slow-eval simulator (one slow upstream wrapping `Sys.sleep(0.2)`).
20. Manual test (added to `tests/manual/`): two-stage pipeline `mtcars |> select(var) |> select(var)` exercising intra-pipeline cascade + Controls-tab activation refresh + Update Plot.
21. Browser e2e test: click var1 in stage 1, verify var2's picker choices update without an Update Data click.

## Verification Plan

1. **S0 — Spike `shiny::debounce` dynamic millis** (D9). Cheap insurance against a foundation issue.
2. Implementation order: D6 contract change → D2 per-consumer cache → D1 button removal → D7 producer debounce wrap → D8 auto-flip → D4 pre-warm wiring → tests.
3. Run full `devtools::test()` after each step; gate landing on green.
4. Run `devtools::check()` (`--as-cran --no-manual`) before merge.
5. Render `README.Rmd` to confirm no doc references to Update Data button remain.

## Risks

- **R1 — `shiny::debounce` dynamic `millis` semantics.** Mitigated by D9 (verify first).
- **R2 — `ggpaintr.verbose` option has no exercised emit sites today.** Doc claims a "Layer foo() removed" message that I could not locate in source. The auto-flip notification will be the first or among the first verbose-gated `cli::cli_inform()` calls. Mitigation: re-read the option doc string before adding the call site to confirm intent still matches.
- **R3 — Auto-flip false-positives.** Transient slow evals (GC pauses, R compiler cold-start) could flip on prematurely. Mitigated by hysteresis (D8) + skipping the first eval per session.
- **R4 — `build_ui` signature change is breaking.** Mitigated: rewrite branch, no external custom consumers known. One-line changelog entry sufficient.
- **R5 — Dead code sweep.** `update_data_button` copy entries, `update_data_stale_class`, `last_click_inputs`, etc., must be cleanly removed (not commented-out) to avoid reviewer confusion. Mitigation: explicit checklist in the implementation PR description.
- **R6 — `cols_memo` deps change is subtle.** The current comment at `paintr-server.R:507–511` warns that depending on live input snapshots caused mid-typing dropdown clobber in a real browser session. The new per-consumer caches must preserve `intersect(selected, cols)` logic in built-in `var` (already there at `paintr-builtins.R`) so picker selection survives invalidation. Browser e2e test required (acceptance criterion 21).
- **R7 — Tab-activation event subscription.** Reading `input[[ns("ptr_layer_select")]]` and the inner Data/Controls `tabsetPanel` selection inputs requires the tabset to have a known `id`. Verify in `paintr-build-ui.R:layer_panel_inner` that the tabsetPanel is created with an `id` (Shiny needs an `id` to expose the `selected` input). If missing, add one.

## Decision Log

| # | Decision | Alternatives | Rationale |
|---|----------|--------------|-----------|
| D1 | Remove Update Data button + all related artifacts | Keep as opt-in; rename | Original goal of the brainstorm; click-gated model is the friction we're removing |
| D2 | Per-consumer reactive cache, dep set covers `node$upstream` subtree | Single shared `cols_memo`; per-layer cache | Per-consumer matches existing per-position upstream classification (`paintr-classify.R`); cleanly scopes invalidation |
| D3 | Controls-tab consumers refresh only on tab activation + Update Plot | Same model as D2 with producer deps | Producers feeding controls-tab consumers live on Data tab; user must switch tabs to edit them, so tab activation already covers it |
| D4 | Pre-warm at startup | Empty-state on first visit; warm only on demand | Avoids empty-dropdown flicker; cheap (one resolve per layer at boot) |
| D5 | Update Plot remains global trigger | (no change) | Current behavior is correct |
| D6 | Extend consumer `build_ui` contract to `(node, cols, data)` | Separate `resolve_meta(df)` hook; lazy thunk | Direct passthrough is simpler; `state$upstream_cache` already dedupes, so memory cost is bounded; future hooks layer on if needed |
| D7 | Wrap producer inputs (`text` / `num` / `expr`) in `shiny::debounce` with dynamic window; consumer commits NOT debounced | Debounce all; debounce nothing; commit-on-blur via JS | Producers are noisy (keystroke), consumers are intentional (commit) — differentiated treatment matches the actual signal model |
| D8 | Auto-flip with symmetric hysteresis (3 slow evals on, 5 fast evals off; 150 / 80 ms thresholds; reset opposing counters on flip; verbose-gated emit) | One-shot on; manual-only knob; debounce-always-on | Adapts to data scale; symmetry handles users who simplify pipeline mid-session; YAGNI-tolerant default of "off + auto" works for typical small data |
| D9 | Verification spike S0 first | Skip; assume `shiny::debounce(millis = function())` works | Cheap insurance; fallback path (hand-rolled debounce) is well-defined if spike fails |

## References

- `R/paintr-ids.R:54, 106` — id stamping (deletion target)
- `R/paintr-build-ui.R:143–149, 271–295, 315` — UI wiring (deletion + adjust)
- `R/paintr-server.R:73, 81–84, 167–215, 218–283, 441–549` — runtime wiring (rewire)
- `R/paintr-registry.R:154–173` — `ptr_define_placeholder_consumer` (signature change)
- `R/paintr-builtins.R:167–173` — built-in `var` consumer (one-line `data` arg add)
- `R/paintr-copy.R:11, 44–46, 320–337` — copy entries (deletion)
- `R/paintr-input-spec.R:46–50` — input spec role (deletion)
- `R/paintr-options.R:5–9, 107–109` — verbose option (consumed by D8 emit)
- `R/paintr-classify.R:89` — `node$upstream` stamping (consumed by D2)
- `tests/testthat/test-server-*.R` — test files needing update
- `tests/manual/` — manual checks to add per acceptance criteria 20
