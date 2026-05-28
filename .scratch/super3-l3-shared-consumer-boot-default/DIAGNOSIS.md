# DIAGNOSIS: super-3 `linked` boots to `mpg` — root cause is custom `shared_ui`, NOT host-scope `has_rendered`

Date: 2026-05-28. Diagnosed via `/diagnose` against `BUG.md`.

## Verdict: the BUG.md root-cause hypothesis is REFUTED

BUG.md hypothesised a "third seeding path" in `ptr_bind_shared_consumer_uis()` (host-scope) that
flips `has_rendered` on a `cols = character()` first fire and never re-injects the formula default —
the same shape as the `cddc46e` fix, unported to host scope.

A non-browser probe rendering the shared panel UI directly disproves this:

- The actual DOM widget for `linked` is the **user-supplied static `selectInput("shared_linked", names(mtcars))`**
  emitted by `shared_panel_body_tag()` at `R/paintr-shared-coordinator.R:333-334` — rendered with
  `<option value="mpg" selected>` (selectInput with no `selected=` → first choice).
- That branch fires because `linked` has a **custom `shared_ui`** entry. It does NOT call
  `build_ui_for()`/`invoke_build_ui()`, so `node$default = "cyl"` is never injected.
- `ptr_bind_shared_consumer_uis()` still runs for `linked` (it is a panel consumer key, not in
  `host_owned_keys`) and registers `output[["shared_linked_ui"]]` — but **no `uiOutput("shared_linked_ui")`
  container exists in the panel for custom-`shared_ui` keys**, so that default-injecting renderUI is
  **orphaned / never displayed**. `consumer_seed_decision()` / `has_rendered` are irrelevant to this fixture.

Probe result:
- WITH custom `shared_ui` (super-3): no `shared_linked_ui` container; static select has `mpg selected`.
- WITHOUT (super-1 style auto-render): `shared_linked_ui` container present; binder injects `node$default = cyl`.

## True differentiator: custom-`shared_ui`-on-a-consumer-key, NOT host scope

super-1 (`shared_grp`) and super-2b (`shared_fac`) seed correctly because they **auto-render** the shared
consumer widget (no custom `shared_ui`), so the binder's renderUI owns the DOM and injects `node$default`.
super-3 is the **only** fixture that supplies a custom `shared_ui` for a **consumer (`var`) key**. All other
`shared_ui` usages in the suite customize **value** placeholders (sliderInput with an explicit `value=`),
where the user naturally sets the default. The panel-rendering path is identical for single-app and
multi-cell, so this is not host-scope-specific; BUG.md conflated the two because super-3 happens to be
both multi-cell and the sole custom-shared_ui-consumer case.

## Contract question (the real fork — needs a human decision)

`shared_ui`'s documented contract (`R/paintr-shared-coordinator.R:198-200`) is
`function(id) -> shiny.tag`; the builder receives **only `id`** — no channel for the formula default.
So "expected = `cyl`" is a contract assumption, not settled behavior. Three defensible directions:

- **A (test/fixture bug):** custom `shared_ui` is fully user-owned; the fixture must pass `selected="cyl"`
  (or the test should assert `mpg` and drop the `linked` xfail pin). No product change. Downside: the
  formula's `ppVar(cyl, ...)` default is silently ignored for custom widgets; inconsistent with auto-path;
  a static `names(mtcars)` list also won't reactively refresh when upstream data changes.
- **B (product fix — honor default):** enrich the consumer-key builder call so it receives the default
  (e.g. `function(id, selected=NULL)` or seed via deferred `update*`). Public-API/contract change; can't
  reliably inject `selected` into an arbitrary returned tag.
- **C (product fix — narrow contract):** ignore/disallow custom `shared_ui` on **consumer** keys; always
  auto-render them (honors default AND restores reactive column refresh). custom `shared_ui` stays
  meaningful only for value placeholders. Changes what super-3 renders (no longer the user's selectInput).

## Feedback loop (Phase 1)

- Authoritative: `NOT_CRAN=true Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-boot-reference-oracle.R")'` (28.5s; pins hold → 29 PASS while bug present).
- Fast/sharp (no browser, ~3s): render `ptr_shared_panel(ptr_shared(...))` and grep the `shared_linked`
  `<select>` for `selected` + presence of `shared_linked_ui`. Decisive and deterministic.
