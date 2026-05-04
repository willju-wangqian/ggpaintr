---
status: accepted
created: 2026-05-03
plan: 05-pipeline-var-resolution-bug
---

# Placeholder × context-construction coverage

Companion audit for plan 05. Enumerates every built-in placeholder's hook consumers vs. every site that constructs a placeholder context, so latent gaps like the pipeline-`var` silent drop are visible at a glance.

## Hook consumers (what each placeholder hook reads from `context`)

| Placeholder | Hook (signature)                                | Context fields consumed                                                |
|-------------|-------------------------------------------------|------------------------------------------------------------------------|
| `var`       | `resolve_input` (default `ptr_resolve_placeholder_input`) | `shared_bindings`, `ns_fn`                                          |
| `var`       | `resolve_expr` → `ptr_validate_var_input`       | `var_column_map[layer_name]`                                           |
| `var`       | `bind_ui` (`ptr_bind_var_ui_impl`)              | `ptr_obj`, `eval_env`, `envir`, `ns_fn`, `ui_ns_fn`, `var_column_map`, `ui_text`, `input` (rebuilds `var_column_map` and `eval_env` if missing) |
| `var`       | `build_ui` (`ptr_build_var_placeholder_ui`)     | (UI shell only — `uiOutput`)                                           |
| `text`      | `resolve_expr`                                  | (none — value-pass-through)                                            |
| `num`       | `resolve_expr`                                  | (none)                                                                 |
| `expr`      | `resolve_expr`                                  | `expr_check` (via outer `ptr_resolve_placeholder_expr`, line 523)      |
| `upload`    | `resolve_input` (`ptr_resolve_upload_input`)    | (none — uses `input` directly)                                         |
| `upload`    | `resolve_expr`                                  | (none)                                                                 |
| `upload`    | `prepare_eval_env`                              | `ns_fn` (presumed; mounts uploads keyed by namespaced id)              |

The outer `ptr_resolve_placeholder_input` (line 482) reads `shared_bindings` and `ns_fn` regardless of placeholder type. The outer `ptr_resolve_placeholder_expr` (line 508) reads `expr_check`.

## Construction sites (what each site populates)

| Site (file:line)                                                  | Role        | Fields populated on the returned context                                                          |
|-------------------------------------------------------------------|-------------|---------------------------------------------------------------------------------------------------|
| `ptr_define_placeholder_context` (paintr-placeholders.R:433)      | base helper | `ptr_obj`, `placeholders`, `ui_text`, `envir`, `expr_check`, `eval_env`, `var_column_map` (default `NULL`) |
| `build_context` in `ptr_setup_data_pipeline_observers` (paintr-app.R:538) | pipeline    | base + `ns_fn`, `input`, `shared_bindings`. **`var_column_map` left `NULL`** — populated per-iteration inside `ptr_resolve_data_pipeline_expr` after this PR. |
| Cached `eval_env` block in `ptr_setup_main_observers` (paintr-app.R:398-417) → consumed at lines 433, 700 | aesthetic   | base + `ns_fn`, `input`, `eval_env`, `shared_bindings`; `var_column_map` precomputed via `ptr_build_var_column_map` (paintr-app.R:408) |
| `ptr_complete_expr` (paintr-runtime.R:133-162)                    | runtime     | base + `ns_fn`, `input`, `eval_env`, `shared_bindings`, `var_column_map` (rebuilt if missing, line 153) |
| `ptr_complete_expr_v2` (paintr-runtime.R:402-410)                 | runtime     | thin wrapper over the above                                                                       |
| `ptr_bind_placeholder_ui` (paintr-placeholders.R:552-571)         | ui-bind     | base + `ns_fn`, `ui_ns_fn`; `var_column_map` rebuilt lazily inside `ptr_bind_var_ui_impl` (line 1408) |
| `ptr_bind_upload_ui` (paintr-upload.R:234)                        | ui-bind     | sets `ns_fn`                                                                                      |

## Coverage matrix

Rows: `(placeholder, hook)`. Cols: site. Cell: `OK`, `n/a`, or note.

|                              | pipeline (paintr-app.R:538) | aesthetic (paintr-app.R:398-417) | runtime (paintr-runtime.R:133) | ui-bind (paintr-placeholders.R:552) |
|------------------------------|-----------------------------|----------------------------------|--------------------------------|--------------------------------------|
| var.resolve_input            | OK                          | OK                               | OK                             | n/a                                  |
| var.resolve_expr             | OK *post-fix*               | OK                               | OK                             | n/a                                  |
| var.bind_ui                  | n/a                         | n/a                              | n/a                            | OK (lazy rebuild)                    |
| text/num/upload.{resolve_*}  | OK                          | OK                               | OK                             | n/a                                  |
| expr.resolve_expr            | OK                          | OK                               | OK                             | n/a                                  |

`*post-fix*`: before this PR, `var_column_map = NULL` at the pipeline site → `ptr_validate_var_input` aborts → swallowed by `tryCatch` → silent drop. The fix populates `var_column_map` per iteration inside `ptr_resolve_data_pipeline_expr`.

## Findings

- **F1 (fixed by plan 05).** Pipeline site did not populate `var_column_map`. Fixed by per-iteration computation inside `ptr_resolve_data_pipeline_expr`. Regression test in `tests/testthat/test-data-pipeline-server.R`.
- **F2 (fixed by plan 05).** Two `tryCatch` swallow points (`ptr_resolve_data_pipeline_expr` lines 1067 / 1075 pre-fix; equivalent positions post-fix) hid the F1 abort. Now warn under `ptr_verbose()` with placeholder id + keyword + cause.
- **F3 (addressed at high-value site).** `ptr_bind_var_ui_impl` and `ptr_complete_expr` both rebuild `var_column_map` lazily when missing — fine, but a downstream call site can still pass an underpopulated context and never know. Mitigation: `ptr_require_context_field()` (paintr-utils.R) now fires inside `ptr_validate_var_input` when `var_column_map` is NULL, so any future under-populating caller fails loudly at the same place plan 05's silent-drop hid. Other consumer sites (`ptr_resolve_placeholder_input` for `ns_fn`, `ptr_resolve_placeholder_expr` for `expr_check`) keep their `%||%` fallbacks; converting those to assertions changes behavior and is deferred until a site collapse motivates it (plan 06).
- **F4 (documented).** `?ptr_define_placeholder` now lists the context fields guaranteed at each hook (`resolve_input`, `resolve_expr`, `bind_ui`, `prepare_eval_env`) and notes the four roles (pipeline / aesthetic / runtime / ui-bind). Custom-placeholder authors can now read the contract without grep. (Note: F4 mistakenly named `ptr_register_placeholder`; the actual public API is `ptr_define_placeholder` + `ptr_merge_placeholders`.)

## Recommended follow-up (out of scope)

1. **Single role-aware context helper.** Replace the four construction sites with one `ptr_build_context(role, ...)` that returns a fully-populated context for the given role, asserting required fields. Surfaces F3/F4 at construction time instead of at use time. Likely lands inside plan 06's shared-cohesion design.
2. **Document context-field contract in `?ptr_register_placeholder`.** Even without (1), enumerate which fields a custom resolver may rely on per role.
