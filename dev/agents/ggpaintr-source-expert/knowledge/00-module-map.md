# Module map — R/paintr-*.R

Source of truth: `NAMESPACE` (auto-generated) + `mcp__serena__get_symbols_overview` of each file. Updated when the codebase changes — if you find drift while answering a question, report it but do not patch this file yourself.

**Legend:** `•` = exported in `NAMESPACE`. `◦` = internal helper. `→` = dispatches via S3 (see *S3 generics* section at the end).

## Layout at a glance

The package is organised by responsibility, not by feature. Each formula travels through this pipeline:

```
formula string
   │  ptr_capture_formula (paintr-app.R)
   ▼
language object
   │  ptr_translate (paintr-translate.R) ──── builds the ptr_* AST
   ▼
ptr_root ──→ classify_walk ──→ ptr_assign_ids ──→ walk_ptr_safety ──→ ptr_substitute / ptr_render / ptr_prune / ptr_eval
   │                                                                       │
   │                                                                       └── runtime: paintr-server.R
   ▼
build_ui_for (paintr-build-ui.R) → Shiny UI
```

Most files implement *one* of the walk methods, a small group of related helpers, or a slice of the runtime. The big files are `paintr-server.R` (full reactive runtime), `paintr-translate.R` (the parser), and `paintr-render.R` (tree → code string with preserve-mode).

## AST: node types & generic walks

### `R/paintr-nodes.R` — AST node constructors + predicates
Constructors and `is_*` predicates for every node class in the `ptr_*` tree. **This file is the schema for the AST** — every other walker pattern-matches against these classes.
- ◦ Constructors: `new_ptr_node`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source`, `ptr_user_expr`, `ptr_literal`, `ptr_closure`, `ptr_missing`
- ◦ Predicates: `is_ptr_node`, `is_ptr_root`, `is_ptr_layer`, `is_ptr_pipeline`, `is_ptr_call`, `is_ptr_ph_value`, `is_ptr_ph_data_consumer`, `is_ptr_ph_data_source`, `is_ptr_placeholder`, `is_ptr_user_expr`, `is_ptr_closure`, `is_ptr_literal`, `is_ptr_missing`
- ◦ `ptr_tree_structural_equal` — structural equality (used by tests + ADR 0012 round-trip checks)

### `R/paintr-walk.R` — tree-walk infrastructure + validators
The generic top-down rewrite cursor and the *structural* tree validators (asserts the tree is in the expected shape between phases).
- ◦ `ptr_walk`, `ptr_collect`, `ptr_cursor`, `ptr_cursor_descend`, `ptr_rewrite_pre` — cursor-threaded rewrite primitives
- ◦ `ptr_assert_ids_assigned`, `ptr_assert_classified`, `ptr_assert_no_placeholders`, `ptr_assert_no_missing`, `ptr_assert_acyclic` — invariant checks

### `R/paintr-classify.R` — `classify_walk` (annotate nodes)
- ◦ `ptr_classify_data` (entry) → `classify_walk` (S3)
- Methods: `.ptr_root`, `.ptr_layer`, `.ptr_pipeline`, `.ptr_call`, `.ptr_ph_data_consumer`, `.ptr_ph_data_source`, `.default`

### `R/paintr-translate.R` — formula → `ptr_root` tree (the parser)
The biggest of the tree-construction files. Handles native pipe, magrittr pipe, nested calls, top-level `+`, structural keywords (`ppLayerOff`, `ppVerbSwitch`), placeholder detection, shared-role validation, depth limits. **ADR 0012**: pipe / `%>%` / nested call must produce the same canonical tree.
- ◦ `ptr_translate` (entry), `ptr_first_pipe_op`, `ptr_assert_no_surviving_structural_wrappers`, `ptr_classify_calls`
- ◦ Pipeline lift: `build_pipeline_from_lift`, `desugar_pipes_to_nested`, `resugar_pipeline_stages`, `rebuild_nested_from_stages`, `try_lift_to_pipeline`
- ◦ Per-node translate: `translate_layer`, `translate_node`, `translate_call`, `translate_layer_children`, `translate_plain_layer`
- ◦ Placeholder detection: `is_placeholder_call`, `detect_placeholder`, `placeholder_keyword`, `extract_placeholder_args`, `extract_shared_value`, `build_placeholder_node`
- ◦ ADR 0020 (structural-keyword) helpers: `is_structural_keyword_call`, `validate_pp_off_hide`, `unwrap_pp_layer_off`, `validate_pp_verb_switch_*`, `unwrap_pp_verb_switch_stage`
- ◦ Shared-role validation: `ptr_validate_shared_roles`, `collect_shared_occurrences`
- ◦ Misc: `layer_call_name`, `dedupe_layer_names`, `check_translate_depth`, `detect_pipe_layer_misuse`, `pipe_layer_warnings_for_layer`, `pipe_stage_display_name`, `pipe_layer_misuse_msg`, `is_ggplot_call_node`, `is_pipe_head`, `pipe_op_from_symbol`, `preprocess_native_pipe`, `extract_call_data_arg`, `split_top_plus`, `abort_translate`
- ◦ Constants: `.ptr_pipe_native_sentinel`

### `R/paintr-substitute.R` — `substitute_walk` (placeholders → values)
Replaces placeholder nodes with their resolved values at render/eval time.
- ◦ `ptr_substitute` (entry) → `substitute_walk` (S3)
- Methods: every node class
- ◦ Helpers: `read_placeholder_value`, `is_missing_value_input`, `validate_resolve_expr_return`

### `R/paintr-render.R` — `render_walk` (tree → code string) + preserve-mode
The other big walk file. Renders an AST back to deparsed R code, with *preserve-mode* (`stamp_current_pick_walk`) that stamps the user's most recent pick into the placeholder before rendering so the displayed code matches the displayed plot.
- ◦ `ptr_render` (entry) → `render_walk` (S3)
- Methods: every node class
- ◦ Pipeline rendering: `render_pipeline_body`, `render_pipe_chain`, `subtree_contains_placeholder`, `first_index_where_subtree_contains_any_placeholder`, `rebuild_nested_from_stages_typed`
- ◦ Call rendering: `render_call_text`, `render_arg_pieces`, `is_binary_infix_op`, `render_binary_operand`
- ◦ Preserve-mode: `stamp_current_pick_walk` (S3, with full method set), `render_placeholder_preserved`, `format_placeholder_value`, `format_bareword`, `format_string`, `format_numeric`, `format_expr`, `.snapshot_value_is_set`
- ◦ Layer-head helpers: `layer_is_piped`, `layer_head_text`, `call_head_text`
- ◦ Constants: `RENDER_WIDTH`

### `R/paintr-prune.R` — `prune_walk` (remove disabled stages)
Pruning is what makes `ppLayerOff` / `ppVerbSwitch` actually drop a layer / stage from the rendered code and the eval pipeline.
- ◦ `ptr_prune` (entry), `ptr_default_is_standalone` → `prune_walk` (S3)
- ◦ Helpers: `bare_call_name`, `qualified_call_name`, `name_matches_remove_set`, `qualified_layer_name`, `check_prune_depth`

### `R/paintr-safety.R` — `walk_ptr_safety` (tree-level safety pass)
Tree-level safety check (composes with the AST-walker denylist in `paintr-utils.R`).
- ◦ `ptr_validate_tree_safety` → `walk_ptr_safety` (S3)
- Methods: every relevant node class

### `R/paintr-disable.R` — `disable_walk` + stage-id machinery
The plumbing behind disabled pipeline stages. The `is_data_chain_call` / `data_arg_position` helpers identify the data-arg position in dplyr/tidyr-style verbs.
- ◦ `disable_walk` (S3) — methods: `.default`, `.ptr_root`, `.ptr_layer`, `.ptr_pipeline`, `.ptr_call`
- ◦ Stage IDs: `stage_id_from_path`, `assign_stage_ids`, `collect_stage_ids`, `is_stage_disabled`
- ◦ Predicates: `is_data_chain_call`, `data_arg_position`

### `R/paintr-build-ui.R` — `build_ui_for` (tree → Shiny UI) + layer scaffolding
The S3 generic that builds the controls panel from the tree. Also owns the `ptr_layer` panel chrome, asset dependencies (`core_assets_dep`, `ptr_user_css_assets`, `ptr_assets`, `ptr_ui_assets`), and shared-placeholder collection logic that decides which widgets render at host vs panel scope.
- • `ptr_ui_assets`
- ◦ `build_ui_for` (S3) — methods: `.default`, `.ptr_literal`, `.ptr_missing`, `.ptr_user_expr`, `.ptr_ph_value`, `.ptr_ph_data_consumer`, `.ptr_ph_data_source`, `.ptr_layer`
- ◦ Output-id helpers: `value_output_id`, `source_output_id`
- ◦ Pipeline-stage UI: `build_pipeline_stage_ui`, `pipeline_override_for_node`
- ◦ Shared-placeholder partition: `find_nodes`, `is_shared_placeholder`, `find_layer_placeholders`, `find_layer_placeholders_with_stage`, `collect_shared_placeholders`, `collect_orphan_shared_stages`, `collect_shared_stage_keys`, `wrap_shared_widgets_with_stage_blocks`
- ◦ Layer panel chrome: `layer_panel_inner`, `layer_panel_content_id`, `layer_panel_default_shell_copy`, `ptr_layer_assets`
- ◦ Copy plumbing into the UI: `build_ui_copy_args`, `invoke_build_ui`
- ◦ Asset deps: `core_assets_dep`, `ptr_user_css_assets`, `ptr_assets`

## Placeholder registry & built-ins

### `R/paintr-registry.R` — process-global placeholder registry
The `.ptr_registry` env is **process-global** (see `project-placeholder-registry-global` memory). Define/lookup/validate happen here. The `ensure_registry_initialized` shim handles registry init ordering (ADR 0014).
- • `ptr_clear_placeholder`, `ptr_define_placeholder_value`, `ptr_define_placeholder_consumer`, `ptr_define_placeholder_source`, `ptr_signal_partial`
- ◦ Env + init: `.ptr_registry`, `.ptr_registry_initialized`, `ensure_registry_initialized`, `ptr_registry_clear`
- ◦ Lookup/listing: `ptr_registry_lookup`, `ptr_registry_keywords`, `ptr_registry_data_aware_keywords`
- ◦ Validators: `validate_keyword`, `validate_hook`, `validate_keyword_no_shadow`, `validate_default_arg`, `validate_named_args`, `ptr_check_keyword_lhs_drift`, `validate_copy_defaults`
- ◦ Internal: `ptr_registry_register`, `ptr_register_structural_keyword`
- ◦ Reserved-name lists: `.ptr_reserved`, `.ptr_shadow_pkgs`, `.ptr_grandfathered_keywords`

### `R/paintr-builtins.R` — built-in placeholder hooks (text/num/var/expr/upload) + `pp*` keywords
Defines the five built-in placeholders and the `pp*` user-facing functions that appear inside formulas. ADR 0009 / 0010 govern the `pp*` naming.
- • `ppVar`, `ppNum`, `ppText`, `ppExpr`, `ppUpload`, `ppLayerOff`, `ppVerbSwitch`
- ◦ Built-in hooks (per placeholder × phase): `ptr_builtin_text_build_ui` / `_resolve_expr`, `ptr_builtin_num_*`, `ptr_builtin_expr_*`, `ptr_builtin_var_*` (incl. `_validate_input`), `ptr_builtin_upload_build_ui` / `_resolve_data`
- ◦ Registration: `ptr_register_builtins`, `.onLoad` (calls registration at package load)
- ◦ Misc: `attach_help`, `strip_matched_quote_pair`, `ptr_upload_accept_formats`, `ptr_builtin_keywords`, `%||%`

## Server / runtime — the big one

### `R/paintr-server.R` — reactive runtime, all of it
The full Shiny server: state init, spec application, per-layer/-stage/-consumer/-source observers, runtime reactives, output bindings, plot/code/error caching. Public entry is `ptr_server`; the heavyweight is `ptr_server_internal` (called by every L2 wrapper). Defaults like `.PTR_DEFAULT_DEBOUNCE_WINDOW` and the slow/fast threshold constants live here. **The `ptr_register_*` functions are the contract surface for L3 custom rendering** — they expose the runtime's last-good plot/code/error caches under `state$runtime()`.
- • `ptr_init_state`, `ptr_server`, `ptr_extract_plot`, `ptr_extract_error`, `ptr_extract_code`, `ptr_gg_extra`
- ◦ Internal entry: `ptr_server_internal`
- ◦ State + spec: `ptr_spec_defaults_from_state`, `ptr_spec_from_snapshot`, `ptr_spec_combine`, `format_spec_for_panel`, `ptr_validate_state`, `apply_spec_at_boot`, `apply_spec_entry`
- ◦ Source binding (data sources): `set_resolve_error`, `is_bare_data_source_layer`, `bind_source_value`, `register_active_upload`, `clear_active_upload`, `vacate_source_binding`, `emit_upload_prologue`, `panel_owned_binding_name`, `try_bind_source_default`, `resolve_upload_source`, `try_bind_source_default_resolved`
- ◦ Pipelines: `ptr_setup_pipelines`, `ptr_bind_source_mutex` (ADR 0025), `ptr_setup_producer_inputs`, `record_eval_time`
- ◦ Stage-enabled observers: `ptr_setup_stage_enabled`, `ptr_setup_shared_stage_enabled`
- ◦ Runtime + error UI: `ptr_setup_runtime`, `ptr_error_ui`, `inject_resolved_data`, `inject_resolved_data_list`
- ◦ Layer UI observers: `ptr_setup_layer_picker`, `ptr_setup_layer_panel_classes`
- ◦ Upstream-data accessors (called by consumer pickers): `runtime_upstream_data`, `runtime_upstream_cols`, `runtime_upstream_data_frames`
- ◦ Per-placeholder UI observers: `ptr_setup_value_uis`, `ptr_setup_source_uis`, `ptr_setup_consumer_uis`, `ptr_bind_shared_consumer_uis`, `ptr_bind_local_shared_consumers`
- ◦ Consumer-resolution helpers: `pending_data_source_keywords`, `collect_upstream_ids`, `find_consumer_ids_in_upstream`, `find_producer_ids_in_upstream`, `find_source_companion_ids_in_upstream`, `find_source_self_ids_in_upstream`
- ◦ L3 cache contract: `ptr_register_last_ok_cache`, `ptr_register_plot`, `ptr_register_error`, `ptr_register_code`, `format_code_with_extras`
- ◦ Tunables: `.PTR_SLOW_THRESHOLD_MS`, `.PTR_FAST_THRESHOLD_MS`, `.PTR_CONSECUTIVE_SLOW_REQUIRED`, `.PTR_CONSECUTIVE_FAST_REQUIRED`, `.PTR_DEFAULT_DEBOUNCE_WINDOW`

## Top-level apps (L2 / L3 entry points)

### `R/paintr-app.R` — `ptr_app`, `ptr_app_grid`, public UI pieces
The L2 (`ptr_app` = self-contained UI+server pair) and the public UI pieces (L3 building blocks). Owns the formula-capture machinery (`ptr_capture_formula` — unwraps top-level `{ ... }` blocks since commit d5dbd2c).
- • `ptr_app`, `ptr_app_grid`, `ptr_ui`, `ptr_server`, `ptr_ui_page`, `ptr_ui_header`, `ptr_ui_controls`, `ptr_ui_plot`, `ptr_ui_error`, `ptr_ui_code`, `ptr_ui_inline_error`, `ptr_ui_toggle_code`
- ◦ Component factories: `ptr_app_components`, `ptr_app_grid_components`, `ptr_make_app_server`, `ptr_capture_formula`, `ptr_build_app_ui`
- ◦ UI internals: `shared_section_tags`, `ptr_controls_panel`, `ptr_outputs_panel`, `code_toggle_icon`, `error_slot_tag`, `code_toggle_button`, `plot_card_tag`, `code_mode_toggle`, `code_window_tag`, `code_block_tag`

### `R/paintr-app-bslib.R` — bslib wrapper
Thin wrapper that swaps `fluidPage` for `bslib::page_*`. Theme passthrough is documented (`feedback-wrapper-scope` memory).
- • `ptr_app_bslib`

## Shared coordinator (multi-host: panel + grid + super-apps)

ADR 0005 / 0006 / 0023 / 0025 govern this surface. The coordinator owns *shared* placeholders (a `var(shared = "x")` appears once and binds widgets in many panels). The partition rule (panel-shared vs. host-shared) is in `shared_partition`.

> ⚠ **File-name pitfall.** `paintr-shared-ui.R` actually holds the **server**, not the UI. `paintr-shared-coordinator.R` holds both the pure coordinator AND the panel UI. `paintr-shared.R` is the P3 AST rewrite (id rewriting). The names are historical — go by what each file's head comment says, not by the suffix.

### `R/paintr-shared.R` — P3 shared-binding AST rewrite
The tree-level rewrite (P3) that collects every placeholder node carrying a non-NULL `shared` field, groups by key, assigns one canonical id per group, and rewrites every member's `id` to point at it. Also owns the `shared_ns` / `canonical_shared_id` id-shape logic (the very thing that drove ADR 0025) and shared-consumer upstream resolution.
- ◦ `ptr_shared_bind`, `collect_then_rewrite_shared`, `canonical_shared_id`, `shared_ns`, `shared_stage_input_id`, `contains_placeholder`, `extract_source_leaf`, `truncate_upstream_at_placeholder`, `collect_shared_consumer_occurrences`, `shared_widget_base_label`, `shared_widget_label`, `shared_widget_default`, `resolve_shared_consumer`, `ptr_resolve_shared_consumers`, `rewrite_shared`

### `R/paintr-shared-coordinator.R` — `ptr_shared` constructor + partition + panel UI
The pure non-reactive multi-instance coordinator. Builds the `ptr_shared_spec`, computes the partition (formula-local vs panel-shared), and renders the panel UI pieces.
- • `ptr_shared`, `ptr_shared_panel`, `ptr_ui_shared_panel`
- ◦ Partition: `shared_partition`, `shared_upstream_source_signature`, `shared_assert_panel_consumer_sources`
- ◦ Panel render: `shared_panel_body_tag`

### `R/paintr-shared-ui.R` — `ptr_shared_server` (host-side reactive bundle)
**The "ui" suffix is misleading — this file is the SERVER.** Holds the `ptr_shared_state` value type, the AST/key plumbing shared with the coordinator, and `ptr_shared_server(obj)` which builds the matching reactives, wires the top-level consumer pickers via `ptr_bind_shared_consumer_uis()`, and returns a `ptr_shared_state` that the embedder threads into each `ptr_server(..., shared_state = ...)`.
- • `ptr_shared_server`, `print.ptr_shared_state` (S3)
- ◦ State: `new_ptr_shared_state`, `validate_ptr_shared_state`
- ◦ Plumbing: `shared_translate_formulas`, `shared_first_nodes`, `shared_consumer_representatives`, `ptr_setup_panel_sources`, `apply_spec_at_boot_host`, `ptr_setup_panel_values`

## Copy / UI text

### `R/paintr-copy.R` — `ui_text` rules: validate, merge, normalize, alias
The full `ui_text` machinery: default copy, validator, deep merge, alias normalization (`colour` → `color`), positional `__unnamed__` arg handling, and the resolver that turns a leaf into rendered text. ADR 0017 mentions deepening opportunities here.
- • `ptr_ui_text`, `ptr_resolve_ui_text`
- ◦ Tree shape: `ptr_ui_text_component_paths`, `ptr_default_ui_text`, `ptr_ui_text_param_aliases`, `ptr_ui_text_leaf_fields`, `ptr_ui_text_keywords`
- ◦ Param normalization: `ptr_param_is_unnamed`, `ptr_normalize_param_key`, `ptr_humanize_param`
- ◦ Interpolation + validation: `ptr_interpolate_ui_text`, `ptr_validate_ui_text_leaf`, `ptr_validate_ui_text`, `ptr_normalize_ui_text`
- ◦ Merge / compact: `ptr_deep_merge_ui_text`, `ptr_compact_ui_text_branch`, `ptr_compact_ui_text`

## Data + upload

### `R/paintr-data.R` — column-name normalisation
- • `ptr_normalize_column_names`
- ◦ `ptr_normalize_tabular_data`, `ptr_normalize_column_name_vector`, `ptr_reserved_words`, `.ptr_reserved_words`

### `R/paintr-upload.R` — file-upload readers
Per-extension readers (csv/tsv/rds/excel/json) + dimension check + default-name helpers. Reader dispatch is `reader_fn_name_for_ext`.
- ◦ `ptr_upload_default_name`, `ptr_read_uploaded_data`, `ptr_unsupported_upload_message`, `ptr_check_upload_dimensions`, `ptr_read_csv_upload`, `ptr_read_tsv_upload`, `ptr_read_rds_upload`, `ptr_read_excel_upload`, `ptr_read_json_upload`, `ptr_resolve_upload_info`, `reader_fn_name_for_ext`

### `R/paintr-resolve.R` — upstream data threading through pipelines
- ◦ `ptr_resolve_upstream`, `ptr_normalize_columns`

## Identifiers + runtime input spec

### `R/paintr-ids.R` — assign `id` to every node + canonical id shapes
Every placeholder/consumer gets a deterministic id; the ids form the contract between the rendered UI bindings and the runtime reactives.
- ◦ `ptr_assign_ids`, `ptr_validate_reserved_shared_keys`, `validate_ns_fn`, `assign_id_to_placeholder`, `walk_has_placeholder`, `ptr_render_id`, `consumer_output_id`

### `R/paintr-input-spec.R` — `ptr_runtime_input_spec` + `ptr_id_table`
Builds the runtime input spec (the typed list of widgets the server has to wire up) and exposes `ptr_id_table` as a public introspection tool.
- • `ptr_id_table`
- ◦ `ptr_runtime_input_spec`, `collect_layer_placeholders`, `placeholder_row`, `companion_row`, `empty_row`, `rows_to_data_frame`, `collect_id_table_rows`, `emit_placeholder_rows`, `walk_layer_placeholders`, `id_table_rows_to_df`
- ◦ Constants: `.spec_columns`, `.id_table_columns`, `.id_table_static_infra`

## Evaluation, defaults, completion

### `R/paintr-eval.R` — `ptr_eval` (tree → language object that can be `eval()`d)
- ◦ `ptr_eval`, `layer_to_eval_expr`, `layer_head_lang`, `pipeline_to_eval_expr`, `desugar_pipe_to_call`, `node_to_lang`, `node_list_to_lang`

### `R/paintr-default-args.R` — constant-fold registry + `ptr_default_*` factories
Default-value validators (`ptr_default_string`, `ptr_default_numeric`, `ptr_default_numeric_vector`, `ptr_default_expression`, `ptr_default_symbol_or_string`) and the constant-fold allowlist used when normalizing default args. Sealed-env eval lives here.
- • `ptr_register_constant_fold`, `ptr_clear_constant_fold`, `ptr_constant_fold_keywords`, `ptr_default_symbol_or_string`, `ptr_default_string`, `ptr_default_numeric`, `ptr_default_numeric_vector`, `ptr_default_expression`
- ◦ Registry env: `ptr_constant_fold_env`, `ptr_constant_fold_env_get`, `ptr_register_constant_fold_builtins`
- ◦ Walker + eval: `validate_constant_ast`, `ptr_constant_fold`
- ◦ Constant list: `.ptr_expr_wrapper_names`

### `R/paintr-safe.R` — runtime-safety wrappers (completion, plot assembly)
The runtime version of "do this carefully and surface a typed error." `ptr_complete_expr_safe` handles `ppExpr` completion (the `<unfinished> + finish_with` flow); `ptr_assemble_plot_safe` + `ptr_validate_plot_render_safe` wrap plot construction.
- ◦ `ptr_complete_expr_safe`, `ptr_format_runtime_message`, `fail_result`, `ptr_assemble_plot_safe`, `ptr_validate_plot_render_safe`

### `R/paintr-utils.R` — denylist, helpers, pruneable-op set
**The AST-walker denylist (`unsafe_expr_denylist`) lives here** (~151 entries; see `project-denylist-complete` memory). Also the per-package "drop names" lists (which symbols are safe to strip during prune) and basic tree helpers.
- ◦ Validators: `ptr_validate_shared_bindings`, `validate_safe_to_remove`, `validate_expr_safety`, `resolve_expr_check`, `is_pruneable_operator_call`
- ◦ Lookup helpers: `find_layer_by_name`, `find_stage_call_by_id`, `handle_duplicate_names`
- ◦ Drop-name registries: `default_drop_when_empty`, `ggplot2_drop_names`, `dplyr_drop_names`, `tidyr_drop_names`, `tibble_drop_names`, `pillar_drop_names`, `purrr_drop_names`, `stringr_drop_names`, `forcats_drop_names`, `lubridate_drop_names`, `hms_drop_names`
- ◦ Misc: `drop_null`, `extract_fn_names`, `pruneable_operator_names`, **`unsafe_expr_denylist`**

## Misc public surfaces

### `R/paintr-headless.R` — non-public batch helpers
**Despite the file name, these are internal.** Auto-memory `project-headless-removed`: post-rewrite ggpaintr has no public headless / non-Shiny path; L3 = custom rendering off `state$runtime()`. None of these symbols are exported.
- ◦ `ptr_exec_headless`, `ptr_headless_upstream_cols`, `ptr_default_snapshot`, `ptr_run_formula`

### `R/paintr-llm.R` — LLM primer / topic catalogue
The corpus that downstream LLM-assistant features read to learn ggpaintr's vocabulary.
- • `ptr_llm_primer`, `ptr_llm_topics`, `ptr_llm_topic`, `ptr_llm_register`

### `R/paintr-options.R` — `ptr_options()`
Per-session settings (debounce window, slow/fast thresholds, etc.).
- • `ptr_options`
- ◦ `ptr_settings`, `ptr_get_setting`, `ptr_validate_setting_value`

### `R/paintr-region.R` — `controllable_region`
ADR 0003 controllable-region UI piece. Internal — surfaced through the L3 UI pieces, not as a direct export.
- ◦ `controllable_region`, `controllable_region_continuation`, `shared_panel_header`

### `R/paintr-css-doc.R` — roxygen-only stub
No top-level R symbols (per `get_symbols_overview`). Hosts the `@docType ...` block for the CSS-class doc page.

### `R/ggpaintr-package.R` — roxygen-only package doc
No top-level R symbols. Hosts the package-level `@docType package` block.

## S3 generics — dispatch table

Every walk generic dispatches on the node's class; methods live in the file named for the walk (`paintr-<walk>.R`) unless noted.

| Generic | Defined in | Dispatch classes |
|---|---|---|
| `build_ui_for` | `paintr-build-ui.R` | `default`, `ptr_literal`, `ptr_missing`, `ptr_user_expr`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source`, `ptr_layer` |
| `classify_walk` | `paintr-classify.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_ph_data_consumer`, `ptr_ph_data_source` |
| `disable_walk` | `paintr-disable.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call` |
| `prune_walk` | `paintr-prune.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_closure`, `ptr_user_expr`, `ptr_literal`, `ptr_missing`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source` |
| `render_walk` | `paintr-render.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_literal`, `ptr_closure`, `ptr_user_expr`, `ptr_missing`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source` |
| `stamp_current_pick_walk` | `paintr-render.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_closure`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source` |
| `substitute_walk` | `paintr-substitute.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_closure`, `ptr_user_expr`, `ptr_literal`, `ptr_missing`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source` |
| `walk_ptr_safety` | `paintr-safety.R` | `default`, `ptr_root`, `ptr_layer`, `ptr_pipeline`, `ptr_call`, `ptr_literal`, `ptr_user_expr`, `ptr_ph_value`, `ptr_ph_data_consumer`, `ptr_ph_data_source`, `ptr_missing` |
| `print` | `paintr-shared-ui.R` | `ptr_shared_state` |

## Exported surface — quick lookup

50 functions exported (per `NAMESPACE`). Grouped by section above. The `pp*` keywords appear inside user formulas; the `ptr_*` functions are programmatic API.
