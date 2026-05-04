---
status: done
created: 2026-05-02
completed: 2026-05-02
size: small
depends-on: none
blocks: 02-data-aware-placeholder-helper, 03-docs-rewrite
outcome: docs-only — `control_panel` slot declared opaque (extensibility vignette §6 bullet 6 + `@seealso` on `ptr_input_ui()`); no code or NEWS changes; original "large" sizing collapsed after deciding L3 customization stops at the output boundary.
---

# Concern 01 — Public UI API for the new data-pipeline / layer-switcher capability

## Problem

Recent commits added two user-facing UI features that surface implicitly through the existing public API:

- **Picker-driven layer switcher** (`b3b9e17`) — replaces the stacked tabset with a `pickerInput` that selects which layer's controls are shown.
- **Data-pipeline placeholders** (`477b8c8`, `6486b18`) — placeholders inside `data = X |> verb(...)` chains, with a per-layer "Data" tab and an "Update data" button.

Existing users who embed ggpaintr via `ptr_input_ui()` + `ptr_output_ui()` + `ptr_server_state()` (or the module variants `ptr_module_ui()` / `ptr_module_server()`) automatically inherit both. There is no opt-out, no deprecation signal, no version bump, and no documented contract for what `ptr_input_ui()`'s `control_panel` slot will contain in future releases.

## Status quo (verified)

- `ptr_input_ui()` (`R/paintr-app.R:1008`) emits only `tagList(uiOutput(control_panel), actionButton(draw_button))`.
- All new UI is rendered server-side and pushed into `control_panel` by `ptr_get_layer_switcher_ui()` (`R/paintr-app.R:447`) and the runtime in `R/paintr-runtime.R`.
- `ptr_output_ui()` (`R/paintr-app.R:1035`) is unchanged structurally.
- `ptr_module_ui()` / `ptr_module_server()` (`R/paintr-app.R:1063, 1112`) wrap the same slots.
- The bslib variant `ptr_app_bslib()` calls `ptr_input_ui()` directly — same blast radius.
- README and vignettes do not describe the layer-switcher or "Update data" behaviors that now appear inside `control_panel`.

## Open questions for the fresh session

1. Is the implicit-inheritance model (UI grows under callers' feet) acceptable long-term, or do we need a feature-flag / opt-in argument on `ptr_input_ui()` (e.g. `layer_switcher = TRUE`, `data_pipeline_ui = TRUE`)?
2. Should the layer switcher be relocated from server-rendered to a client-side tag the user can compose into a custom layout? (Trade-off: ergonomics vs. reactivity.)
3. What is the public contract for the `control_panel` slot? Document it explicitly, or treat it as opaque?
4. Do `ptr_module_ui()` / `ptr_module_server()` need the same arguments mirrored?
5. How is `ptr_app_bslib()`'s sidebar layout affected — does it need updates so the layer picker renders sensibly inside `bslib::sidebar()`?
6. Versioning + deprecation story: do we bump to 0.10.0 and document the new surface in NEWS, or is this still pre-1.0 leeway?

## Out of scope

- Internal refactoring of `ptr_get_layer_switcher_ui()`.
- Visual restyling.
- Custom-placeholder data resolution (covered by `02-data-aware-placeholder-helper.md`).

## Success criteria

- Documented public contract for what `ptr_input_ui()` / `ptr_module_ui()` render, including extension points.
- Decision on opt-in vs. always-on for the new UI elements.
- Concrete migration note for existing embedders.

## Recommended next-session approach

Invoke `brainstorming` with this file's contents. Start with question 1 (opt-in vs. implicit) — the answer cascades into 2–4.
