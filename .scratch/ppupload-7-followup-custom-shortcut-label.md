# Follow-up (surfaced by ADR 0025 item #7): custom shortcut sources lose label/placeholder control over the framework-owned shortcut textInput

**Status:** needs-triage
**Surfaced:** 2026-05-29, during ADR 0025 item #7 (criterion 5 implementation; user-approved contract change "Framework owns it").
**Scope discipline:** the contract change itself is in-scope (criterion 5); this label-control limitation is the noted downside, flagged not silently fixed.

## The contract change (landed in #7)

Item #7 criterion 5 makes the **framework** emit the shortcut `textInput` for every `shortcut = TRUE` source (built-in `ppUpload` + any custom source). A custom source's `build_ui` now renders ONLY its data-payload widget (or `NULL` for a shortcut-only source) and must NOT render its own `textInput(node$shortcut_id)` (doing so double-binds the id). Registry roxygen (`ptr_define_placeholder_source`) updated to match; 3 fixtures updated.

## The limitation

The framework shortcut `textInput` is emitted in `build_ui_for.ptr_ph_data_source` (R/paintr-build-ui.R) with copy resolved from `ptr_resolve_ui_text("upload_name", ui_text)` — i.e. the **`upload$name`** copy namespace ("Optional dataset name" / "For example: sales_data" / the accepted-formats help). That copy is **ppUpload-flavoured and global**: a custom source (e.g. an env-frame loader) cannot give its shortcut box a bespoke label/placeholder/help (the old fixtures wanted "Env frame name").

A custom source CAN override it only by passing `ui_text = list(upload = list(name = list(label = ...)))` to `ptr_app()` — but that key is shared with `ppUpload` and is not per-keyword, so two different custom shortcut sources on one app cannot have distinct shortcut labels.

## Options (if this matters)

1. Add a per-source copy slot, e.g. resolve `ptr_resolve_ui_text("control", keyword = node$keyword, ...)$shortcut_label` (or a new `shortcut`-namespaced copy component) in `build_ui_for.ptr_ph_data_source`, falling back to the `upload_name` default.
2. Let `ptr_define_placeholder_source` accept a `shortcut_copy = list(label=, placeholder=, help=)` and stamp it on the node for `build_ui_for` to read.
3. Accept the limitation (the built-in `ppUpload` copy is a reasonable default for "type a dataset name").

## Why deferred

Out of scope for #7 (cosmetic-reset feature); the test fixtures don't assert the shortcut label, so this is a real-author ergonomics gap, not a test/gate blocker.
