# Public API audit — post-core-rewrite

Goal: every exported name is meaningful to a real user tier; no internal that should be exported; no stale exports; no stale arguments. Decisions reached in the grill-with-docs session on `core-rewrite-impl`. See `docs/adr/0002-public-api-tiering-and-trims.md` for the rationale.

## Resulting public tiers (target end state)

1. **End users** — `ptr_app`, `ptr_app_bslib`, `ptr_app_grid`, `ptr_options`, `ptr_normalize_column_names`, `ptr_ui_text`
2. **Shiny embedders** — `ptr_module_ui`/`ptr_module_server`, `ptr_controls_ui`, `ptr_outputs_ui`, `ptr_server`
3. **Advanced embedders** (programmatic driving / testing / own-the-outputs) — `ptr_init_state`, `ptr_register_plot`/`ptr_register_error`/`ptr_register_code`, `ptr_extract_plot`/`ptr_extract_error`/`ptr_extract_code`, `ptr_gg_extra`, `ptr_resolve_ui_text` *(UI fragments `ptr_controls_ui`/`ptr_outputs_ui` are reused here too — tiers are "primary audience", not a partition)*
4. **Placeholder authors** — `ptr_define_placeholder_value`/`ptr_define_placeholder_consumer`/`ptr_define_placeholder_source`, `build_ui_for`
5. **LLM tooling** — `ptr_llm_primer`/`ptr_llm_topic`/`ptr_llm_topics`/`ptr_llm_register`

Deliberately **not** public: `ptr_translate` (would commit the node-class contract as API), `ptr_run_formula` + the whole headless layer, `ptr_setup_*`. Audit position: non-Shiny use is intentionally unsupported.

## Checklist

### Un-export (stale portals)
- [ ] `ptr_runtime_input_spec` — drop `@export`; only internal callers (`paintr-headless.R`, `paintr-server.R`). (Was believed already done; it wasn't.)
- [ ] `ptr_ns_id` — drop `@export` (or delete; it's `function(ns_fn, id) ns_fn(id)`). No live Tier-3 use: hooks receive `node$id` already namespaced. Its man-page example documents the *removed* `context`/`meta` hook signature.

### Renames (clarity)
- [ ] `ptr_merge_ui_text` → `ptr_ui_text`. Public face = inspect/pre-validate the copy tree. `ptr_ui_text()` → defaults; `ptr_ui_text(ui_text = <overrides>)` → merged+validated `ptr_ui_text`-classed object reusable across `ptr_app()` calls (which already short-circuit on an already-merged object). Keep the internal merge-engine role; rename all call sites.
- [ ] `ptr_server_state` → `ptr_init_state`. Re-doc as: constructor for the `ptr_state` runtime container (translated tree + reactiveVals + eval env) for **programmatic driving, `shiny::testServer`, and `ptr_gg_extra`/`ptr_extract_*`** — *not* a from-scratch reactive-app builder (the `ptr_setup_*` observers stay internal). For a wired app use `ptr_server()` and override outputs as needed.

### Trim stale arguments
- [ ] `placeholders=` — remove everywhere. `ptr_default_ui_text()` ignores it; no caller passes non-NULL. Drop from `ptr_ui_text` (ex-`ptr_merge_ui_text`), `ptr_resolve_ui_text`, the ~6 internal copy/build-ui helpers that thread it, and the `build_ui_for` `…` roxygen.
- [ ] `ns=` on `ptr_app()` and `ptr_app_bslib()` — remove from the public signatures; keep on the internal `*_components(ns=)` helpers. Inert at the `ptr_app` layer (a `shiny.appobj` can't be embedded). Multi-instance stays via `ptr_module_*` / `ptr_app_grid`.
- [ ] After the `ui_text` consolidation (below): drop `title=` from `ptr_app_bslib`, drop `title=`/`draw_all_label=` from `ptr_app_grid`. Route through `ui_text`: `shell$title$label` exists; add `shell$draw_all_button$label` for the grid draw-all button.
- [ ] `ptr_init_state` (ex-`ptr_server_state`) deep-machinery args — give each of `server_ns`, `auto_bind_shared`, `shared_resolutions`, `producer_debounce_ms` a real `@param`, or move behind `...`, depending on whether it has a plausible Tier-3 use.

### Signature fix (not just polish — confirmed in this session)
- [ ] `ptr_resolve_ui_text(component, keyword = NULL, layer_name = NULL, param = NULL, ui_text = NULL)` → reorder to `ptr_resolve_ui_text(component, keyword = NULL, param = NULL, layer_name = NULL, ui_text = NULL)`. Matches the `defaults → params → layers` specificity chain; `keyword` always known, `layer_name` rarely passed. Blast radius ≈ 0 — every caller (5 internal) + both roxygen examples name-pass. (`layer_name` and the `rules$layers[...]` per-layer override branch are **live**, fully wired end-to-end — not stale; verified.)

### Keep (recategorize / document better — not trim)
- [ ] `ptr_resolve_ui_text` — recategorize Tier-3/introspection. Add a worked extensibility example that calls it inside a custom `build_ui` hook so a placeholder author labels their control through the same override chain as built-ins.
- [ ] `known_param_keys=` on `ptr_ui_text` — keep (real feature: warns on misspelled `ui_text$params` keys). Wire it from `ptr_ui_text`'s own internal callers so it's actually exercised.
- [ ] `build_ui_for` `…` contract — after dropping `placeholders`, re-verify `ui_text`, `layer_name`, `ns_fn`, `checkbox_defaults`, `shell_copy` are all actually passed; fix the roxygen to match. A Tier-3 author reads that list as the contract.

### Docs
- [ ] `_pkgdown.yml` reference index — rewrite entirely. Dead entries to remove: `ptr_build_ids`, `ptr_setup_controls`, `ptr_register_draw`, `ptr_input_ui`, `ptr_output_ui`, `ptr_define_placeholder`, `ptr_merge_placeholders`, `ptr_missing_expr`, `ptr_resolve_layer_data`, `ptr_parse_formula`, `ptr_exec`, `ptr_assemble_plot`. Add the real exports (`ptr_controls_ui`, `ptr_outputs_ui`, `ptr_module_ui`/`ptr_module_server`, `ptr_define_placeholder_value`/`consumer`/`source`, `ptr_gg_extra`, etc.). Group by the five tiers above.
- [ ] `ptr_ui_text` (ex-`ptr_merge_ui_text`) man page — add `@section UI text schema:` enumerating every leaf path: `shell$title$label`, `shell$draw_button$label`, `shell$draw_all_button$label`, `shell$layer_picker$label`, `shell$data_subtab$label`, `shell$controls_subtab$label`; `upload$file$label`; `upload$name$label`/`placeholder`/`help`; `layer_checkbox$label`; `defaults$<keyword>$<leaf>`; `params$<param>$<keyword>$<leaf>`; `layers$<layer_name>$<keyword>$<param>$<leaf>`. Leaf fields = `ptr_ui_text_leaf_fields()`.
- [ ] Every `@param ui_text` across the public functions → "named list of copy overrides; see [ptr_ui_text()] for the full schema and current defaults."
- [ ] Workflow vignette "Customizing copy" section — **out of scope here**; it's the separate parked-vignette rewrite. Just note the new `ptr_ui_text` name + `shell$draw_all_button$label` when that lands.
- [ ] `devtools::document()` after all roxygen/`@export` changes; confirm `NAMESPACE` diff matches intent.

### Verify
- [ ] `devtools::test()` — 0 fail; update any test referencing renamed functions / removed args.
- [ ] `devtools::check(--as-cran --no-manual)` — 0 errors, 0 warnings.
- [ ] `_pkgdown` rebuild clean.
