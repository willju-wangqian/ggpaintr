---
name: core-rewrite
type: decision
status: accepted
scope: [parse, runtime, placeholders, pipeline, prune, ui-bridge]
created: 2026-05-05
related: [core-rewrite-bdd.md, checkbox-defaults.md]
---

# Core Rewrite — Typed AST + Visitor Passes

## Understanding Summary

ggpaintr's current core (`paintr-parse.R`, `paintr-runtime.R`, `paintr-placeholders.R`, `paintr-ui.R`, `paintr-app.R` — ~4600 lines) accreted as patches over time. The result has three structural problems:

1. **No real AST walker as a primitive.** `get_index_path` collects placeholder positions once into a flat sidecar map; runtime then iterates the flat map, never the tree. Multi-pass walks are not first-class.
2. **Implicit data-aware classification.** `ptr_define_placeholder` has no `data_aware` field. Whether a placeholder participates in the data pipeline is inferred late by `ptr_compute_data_pipeline_info` matching ids against pipe positions.
3. **Pipe operators stripped at parse, stitched back via string substitution.** `bait <- "ptrDataArgBait_x7Qk"` is the giveaway — `|>` and `%>%` survive only by gsub round-trip.

Plus duplication: `generate_ui_*` in `paintr-ui.R` (562 lines) parallels `ptr_build_*_placeholder_ui` in `paintr-placeholders.R`. Pipeline machinery and registry contract are tangled in the same file.

This rewrite replaces the core with a typed AST + S3 visitor passes. Custom placeholders register a node type rather than a bag of hooks. Pipes stay in the tree. Data-aware classification is structural, not inferred.

## Non-Goals

- No backward compatibility. Project has no real users (confirmed by maintainer); freedom is total. Tests reaching into internals (`ptr_complete_expr`, `prune_empty_substitution_artifacts`, `ptr_resolve_text_expr`, `expr_remove_null`, etc.) will be rewritten against new internals. Behavior survives; internal API names do not.
- No formula-syntax change. Bare-symbol placeholders (`var`, `text`, `num`, `expr`, `upload`) and call-form with shared key (`var(shared = "x_axis")`) keep their current syntax.
- No app-shell rewrite. `paintr-app.R` (1573 lines) keeps its server/observers/state shape; observers thread the new typed tree through P12 instead of reaching into `placeholder_map` and `data_pipeline_info`.
- No copy-rules rewrite. `paintr-copy.R` (763 lines) is fully orthogonal to AST shape and stays.
- No upload-helper rewrite. The csv/rds/excel/json/tsv readers in `paintr-upload.R` (251 lines) become implementation detail of the built-in `upload` source's `resolve_data` hook.

## Pass Inventory

The core is twelve passes, each a typed visitor or a server-state operation. Passes run in order during a render cycle; some are computed once at parse time (P1–P4), others on every input change (P6–P11), and P12 is the long-lived Shiny reactive layer.

### P1. translate — string → typed tree

`rlang::parse_exprs` once. Reject if zero or multiple top-level expressions. Rewrite `%>%` to native pipe form structurally (the typed tree records which op the user wrote so render can preserve surface). Split top-level `+` into a list of layers. For each layer, recurse: pipes become `ptr_pipeline(stages, op)`; placeholder tokens become `ptr_ph_value` or `ptr_ph_data_*` based on the registry's `data_aware` and `role` fields; other calls become `ptr_call(fun, args)`; constants become `ptr_literal`. `pkg::fn` and `pkg:::fn` heads parse normally; `get_fun_names` returns the bare name. Comments are dropped by R's parser and never reach the tree. Apply a depth limit to translation (default 100).

### P2. classify-data — annotate `ptr_ph_data_*` with upstream

Walk the typed tree carrying a `ctx_data` pointer. Rules:

- For a `ptr_layer`: when descending into the layer's `data_arg`, the children inherit the surrounding `ctx_data`. When descending into the layer's other children (aes, params), `ctx_data` becomes the layer's own `data_arg` (or inherits if absent — see ggplot/geom inheritance).
- For a `ptr_pipeline`: stage 1 sees inherited `ctx_data`; stage k > 1 sees stages\[1..k-1\] joined as a synthetic upstream subtree.
- For a `ptr_ph_data_consumer`: attach `node$upstream = ctx_data`.
- For a `ptr_ph_data_source`: ignore `ctx_data` (sources produce data, do not consume).

ggplot/geom data inheritance: the first layer named `ggplot` in a root sets the default `ctx_data` for sibling layers that lack their own `data_arg`. Geom layers with an explicit `data_arg` shadow it.

`upstream` is itself an AST subtree, not a data frame. Materializing it is P12's job.

### P3. shared-binding — group nodes by shared key

Collect every placeholder node carrying a non-NULL `shared` field. Group by key. For each group, derive one canonical id; rewrite every member's `id` to point at it. UI build (P6) then renders only one widget per group; substitution (P8) reads the canonical id from the input snapshot and applies to every group member.

`shared` keys are validated at translate time: must be a non-empty single string named `shared` in a single-arg call form (e.g., `var(shared = "axis")`). Bare symbols carry `shared = NULL`.

### P4. id-encoding — derive deterministic ids; thread `ns_fn`

Raw id format: `<layer_name>+<dot-joined-index-path>+<keyword>+<shared-or-NA>`. Plus per-layer derived ids: `<layer_name>_checkbox`, `<layer_name>_update_data` (only present when the layer has a non-empty pipeline `data_arg`), `<placeholder_id>_upload_name` (companion row for upload sources).

`ns_fn` is `shiny::NS("namespace")` or `shiny::NS(NULL)` (identity). The typed tree stores raw ids; rendered ids are computed at UI emit and observer-binding time by applying `ns_fn`. `ptr_runtime_input_spec` returns raw input_ids by default.

### P5. safety — denylist visitor

One walker over the typed tree. For every `ptr_call`, check `node$fun` (symbol, namespaced symbol via `::`/`:::`, or paren-wrapped head) against the denylist. For every bare symbol referenced as a value or higher-order argument, same check. For every string literal, check against the same list (catches `exec("system", ...)` and `getExportedValue("base", "system")`). Recurse into anonymous-function bodies (lambda formals' default values too) and pairlist results. Apply a depth limit (default 100).

The denylist is considered complete (~151 entries). The walker's recursive descent is the primary safety mechanism, not the denylist alone. Allowlist mode and per-call `deny_list`/`allow_list` overrides are preserved. `expr_check = FALSE` skips all of P5.

### P6. ui-build — per-node UI generation

S3 dispatch on node class. Each class has a `build_ui_for.<class>` method:

- `ptr_ph_value`: registry's `build_ui(node, ...)` with `id`, `label` (from copy-rules), and any keyword-specific args.
- `ptr_ph_data_consumer`: P12 supplies a resolved `cols` vector via `ptr_resolve_upstream(node$upstream, ...)`; registry's `build_ui(node, cols = cols, ...)` builds the picker.
- `ptr_ph_data_source`: registry's `build_ui(node, ...)` plus a paired companion widget (e.g., `upload`'s name input via `ptr_upload_name_id`).
- `ptr_layer`: layer-switcher panel scaffolding (Data sub-tab if any pipeline placeholders, Controls sub-tab otherwise; layer-toggle checkbox on top for non-`ggplot` layers).
- `ptr_user_expr`: no UI of its own (provenance node only).

Copy-rules resolution comes from `paintr-copy.R` unchanged. Hidden behind `build_ui_for.*`, the registry's `build_ui` always sees a resolved label string, never a path.

### P7. input-spec — derive `ptr_runtime_input_spec`

One walk over the typed tree, emitting a row per node that needs a Shiny binding:

- one row per `ptr_placeholder` with role `placeholder`
- one extra row per `ptr_ph_data_source` companion (e.g., `upload_name`)
- one row per non-`ggplot` `ptr_layer` with role `layer_checkbox`
- one row per `ptr_layer` with non-empty pipeline `data_arg` with role `layer_update_data`

Columns: `input_id`, `role`, `layer_name`, `keyword`, `param_key`, `source_id`, `shared`. Order: placeholder rows first (with companions adjacent), then derived layer rows.

### P8. substitute — values into the tree

Walk the tree carrying `input_snapshot`, `shared_bindings`, and `eval_env`. Per node class:

- `ptr_ph_value(id, keyword, ...)`: read `input_snapshot[[id]]`; if absent or NULL or empty, return `ptr_missing()`. Otherwise call registry's `resolve_expr(value, node)` and validate return type against the whitelist (numeric, character, logical, integer, NULL, language, symbol, expression). If `resolve_expr` returns a denied character literal, P5 is invoked on the result before replacement.
- `ptr_ph_data_consumer(id, ...)`: same flow; validation also checks `value` against the upstream's column set (computed via P12).
- `ptr_ph_data_source(id, ...)`: read snapshot; if absent, `ptr_missing()`. Otherwise return the symbol the user assigned (e.g., upload name) — the actual data frame substitution happens in P11 via `eval_env`.
- `ptr_user_expr(inner)`: provenance wrapper; substitute proceeds into `inner` but the wrapper is preserved through P9 (see G5).
- Built-ins for `text`/`num`: `text` strips one matching pair of leading/trailing quotes; `num` returns `ptr_missing()` for NULL/NA/empty.
- `ptr_call(fun, args)`: substitute children, rebuild call with `rlang::call2(fun, !!!new_args)` preserving arg names.
- `ptr_pipeline(stages, op)`: substitute children, rebuild pipeline node (op preserved).
- `ptr_layer`: substitute children; if layer's `active_input_id` is in snapshot and FALSE, mark layer with `node$active = FALSE` for P9 to drop. If the layer's `data_arg` has been pre-resolved to a cached frame by P12, replace `data_arg` with a marker referencing the cached value.

`shared_bindings` (an external lookup of canonical-id → reactive value) overrides input_snapshot when present.

P8 never decides that a call is "empty enough to drop." That is P9's job alone.

### P9. prune — drop missing and empty-after-substitution

S3 visitor. Rules per node class:

- `ptr_user_expr(inner)`: return as-is. Never recurse into. Never drop.
- `ptr_ph_data_consumer` / `ptr_ph_value` not yet substituted (still a node): treat as missing.
- `ptr_call(fun, args)`: prune children. For each pruned arg:
  - if it's `ptr_missing()` and the arg is **named**, drop the arg.
  - if it's `ptr_missing()` and the arg is **positional**, the call escalates to `ptr_missing()` (operators included — operators are calls).
  - otherwise keep.
  After filtering, if the call has zero remaining args **and** its name is in the configured `remove_set` **and** it does not match the configured `is_standalone(name)` predicate, escalate to `ptr_missing()`.
- `ptr_pipeline(stages, op)`: prune each stage. Drop stages that are `ptr_missing()`. If zero or one stage remains, collapse (single stage replaces the pipeline; zero stages → `ptr_missing()`).
- `ptr_layer`: prune children. If `active = FALSE`, return NULL (entire layer dropped). If the layer's call view became `ptr_missing()` and the layer name is not standalone-eligible, return NULL. Otherwise keep.
- `ptr_root`: prune layers, drop NULLs, return.

`remove_set` defaults to a curated set (matching today's `default_safe_to_remove`); user can pass extras via `safe_to_remove` parameter to the public render entry point.

`is_standalone(name)` defaults to `prefix == "geom_" || prefix == "stat_"`. Configurable.

### P10. render-code — typed tree → code text

S3 visitor producing strings. `ptr_pipeline` renders to `<stage1> <op> <stage2> <op> ...` honoring `node$op`. `ptr_call` renders to `fun(arg1, arg2, ...)` with named args printed `name = value`. `ptr_user_expr(inner)` renders to the inner expression's text. Layers are joined with ` +\n  `. `pkg::fn` heads are preserved. Comments do not appear (the parser stripped them in P1).

### P11. eval — typed tree → ggplot object

For each layer, render the typed tree to an evaluable R expression (not a string): `ptr_pipeline(stages, op)` folds via `Reduce(function(acc, stage) rlang::call2(op, acc, stage), stages[-1], stages[[1]])`; everything else is straightforward AST reconstitution. `validate_expr_safety` runs over each layer expr (P5 again, since P9 may have produced new structure). Each layer is `eval`'d in `eval_env`; results are accumulated with `+`. Errors are caught and routed to safe wrappers (P12).

### P12. server-state — Shiny reactive layer

The long-lived layer:

- holds `ptr_state` (id contract, copy rules, expr_check, checkbox_defaults, draw_trigger, gg_extras)
- snapshots input values into a per-layer `data_snapshot` reactiveVal, exposed via `state$resolved_data[[layer_name]]`
- exposes `state$is_stale_env[[layer_name]]` reactive: TRUE when current input values diverge from snapshot
- handles "Update Data" click: validate every selection against per-position upstream columns; if all valid, update snapshot atomically; else leave snapshot
- runs `ptr_resolve_upstream(subtree, snapshot, cache, eval_env)`: substitute placeholders inside subtree, eval to a data frame, memoize by digest of (subtree, substituted-input-state)
- routes errors via `ptr_complete_expr_safe` / `ptr_assemble_plot_safe` / `ptr_validate_plot_render_safe` so eval-time failures surface inline UI errors and the `code_text` panel still updates
- supports `ptr_gg_extra` for ad-hoc extra layers tracked in a reactiveVal; `ptr_extract_code` appends extras when runtime is ok

## Node Taxonomy

```
ptr_root      { layers: list<ptr_layer | ptr_user_expr> }
ptr_layer     { name, expr, data_arg?, children, active_input_id?, default_active, active }
ptr_pipeline  { expr, stages, op: "|>" | "%>%" }
ptr_call      { expr, fun, args: list (with names()) }
ptr_ph_value  { id, keyword, param, expr, shared? }
ptr_ph_data_consumer { id, keyword, param, expr, shared?, upstream: <subtree pointer> }
ptr_ph_data_source   { id, keyword, param, expr, shared?, companion_id }
ptr_user_expr { inner: <substituted-from-expr-placeholder subtree> }
ptr_literal   { expr }
ptr_missing   { }
```

Every node carries an `expr` field with the underlying R `language` object so `eval()` and code-text rendering need no reverse translation.

## Registry Contracts

Three constructors replace today's single `ptr_define_placeholder`:

```r
ptr_define_placeholder_value(
  keyword, build_ui, resolve_expr,
  copy_defaults = list(label = "Enter a value for {param}")
)

ptr_define_placeholder_consumer(
  keyword, build_ui, resolve_expr,    # build_ui(node, cols, ...) — cols is resolved
  validate_input = NULL,              # optional: (value, upstream_cols) -> bool / msg
  copy_defaults = list(...)
)

ptr_define_placeholder_source(
  keyword, build_ui, resolve_data,    # resolve_data(value, node, ...) -> data.frame
  resolve_expr = NULL,                # optional override; default returns rlang::sym(value)
  companion_id_fn = NULL,             # optional: (id) -> companion id, e.g. upload_name
  copy_defaults = list(...)
)
```

Built-ins map: `text`/`num`/`expr` → `*_value`; `var` → `*_consumer`; `upload` → `*_source` (with `companion_id_fn = ptr_upload_name_id` and `resolve_data = ptr_read_uploaded_data`).

Hooks dropped relative to today: `bind_ui` (absorbed by P6 and P12 reactivity), `prepare_eval_env` (folded into `resolve_data` for sources). `resolve_input` is gone; substitute reads from snapshot directly.

Validation:

- `keyword` is a single non-empty string, valid R name, not a reserved word.
- `build_ui` and `resolve_expr` (where mandatory) are functions accepting at least the documented args (excess via `...` allowed; arity warning when a hook only declares `...`).
- `copy_defaults` is a named list with values single strings; only documented fields allowed.

## G1–G9 Commitments

**G1. Layer checkbox / layer-active.** `ptr_layer` carries `active_input_id` (NULL for `ggplot`) and `default_active` (resolved from `checkbox_defaults` arg via `ptr_resolve_checkbox_defaults`). The pure resolver function survives unchanged; it consumes the layer-name list (with deduplication suffixes) from `node_root$layers`. P12 stores the resolved vector on `state$checkbox_defaults`. P6 emits the checkbox tag with `value = state$checkbox_defaults[[layer_name]]`. P9 drops layers whose `active = FALSE`. Validation rules (non-list error, NA error, length-0 error, duplicate-name error, unknown-key warning, etc.) live in `ptr_resolve_checkbox_defaults` as today.

**G2. Operator escalation.** Operators are calls. `ptr_prune.ptr_call` rule "positional missing arg → call escalates to `ptr_missing()`" handles `mpg > <missing>` natively. No special operator rule.

**G3. `safe_to_remove`.** Parameter to the public render entry point; merged with `default_safe_to_remove` and threaded into `ptr_prune` as `remove_set`.

**G4. `is_standalone(name)`.** Configurable predicate on `ptr_prune.ptr_layer` and `ptr_prune.ptr_call`. Default: regex match `^(geom_|stat_)`.

**G5. `expr` placeholder provenance.** P8 wraps every expr-placeholder substitution in `ptr_user_expr(inner)`. P9 has one rule for this class: `prune.ptr_user_expr <- function(node) node` — never recurses, never drops. Removes the parallel `orig`-tree comparison used today. Bare-symbol `expr` at layer level is recognized at P1 and routed to the same wrapper.

**G6. "Update Data" snapshot — three nuances.**

- **Initial seed.** App start: P9 runs over the initial-input state of the AST (every `ptr_placeholder` produces `ptr_missing()`, which prunes empty pipeline stages); P11 evaluates the result and `ptr_resolve_upstream` caches it. So `mtcars |> head(num) |> ggplot(...)` seeds with `head(mtcars)` (default 6 rows), as today.
- **Atomic snapshot on click.** Update Data button click validates every consumer's selection against its per-position upstream's column set. If all valid → update snapshot. If any invalid → snapshot untouched (no partial writes). Inline error surfaces.
- **Dual upstream.** For each `ptr_ph_data_consumer`, dropdown choices come from `ptr_resolve_upstream(node$upstream, ...)` (per-position upstream). For each layer, the data frame fed to the plot eval comes from the layer's `data_arg` post-substitution and post-snapshot (terminal upstream). Two consumers of the same `ptr_resolve_upstream` mechanism, different subtree arguments.

**G7. Pipe surface preservation.** `ptr_pipeline` carries `op` (`|>` or `%>%`). P10 walks the typed tree with op intact. P11 folds via `rlang::call2(op, ...)`. No string substitution. Mixed pipe chains (`a |> b() %>% c()`) preserve per-stage op.

**G8. `paintr-llm.R`.** Out of scope. The four functions (`ptr_llm_primer`, `ptr_llm_topics`, `ptr_llm_topic`, `ptr_llm_register`) are documentation/help-aware accessors that do not touch the AST. They consume the registry; they will need a small adapter when the registry grows the `role` field, but no structural change.

**G9. Internal API names.** The public-facing top-level entry points are renamed for clarity. The implementation modules behind them are reorganized:

| today | tomorrow |
|---|---|
| `R/paintr-parse.R` | `R/paintr-translate.R` (P1) |
| `R/paintr-runtime.R` | `R/paintr-substitute.R` + `R/paintr-prune.R` + `R/paintr-render.R` + `R/paintr-eval.R` (P8–P11) |
| `R/paintr-placeholders.R` | `R/paintr-registry.R` (constructors + validation) + `R/paintr-builtins.R` (the five built-ins) + `R/paintr-classify.R` (P2) |
| `R/paintr-ui.R` (`generate_ui_*` wrappers) | deleted; functionality folded into P6 dispatch in `R/paintr-ui.R` (scaffolding only) |
| `R/paintr-app.R` | unchanged in shape; observer wiring rewritten to consume typed tree via P12 |

Estimated final core: ~1500–2000 lines of new R; ~3500 lines of existing core dropped.

## Coverage Matrix

Every test-pinned behavior maps to a pass. See `core-rewrite-bdd.md` for executable scenarios. The 43 test files inventoried map as follows:

| test file | passes |
|---|---|
| test-app-bslib | P12 |
| test-caching-and-rename | P4 + P12 |
| test-checkbox-defaults | G1 + P12 + P6 |
| test-column-name-normalization | P11 (post-materialize) |
| test-complete-expr | P8 + P9 + G1 |
| test-copy-rules | P6 (paintr-copy untouched) |
| test-coverage-gaps | P8 + P11 + P2 |
| test-custom-placeholder-data-aware | source/consumer constructors + P2 |
| test-data-pipeline-placeholders | P2 + P9 |
| test-data-pipeline-plot-eval | P2 + P10 + P11 + P12 cache |
| test-data-pipeline-server | P12 + P2 cache |
| test-data-pipeline-ui | P6 |
| test-denylist-batch2 / pass2 / step3 | P5 |
| test-extensibility | P4 + P6 + P12 |
| test-gg-extra | P12 + P10 |
| test-layer-switcher | P6 |
| test-module / test-namespace | P4 (ns_fn threading) |
| test-options | options helper untouched + G1 interaction |
| test-package-namespace | P1 |
| test-parse-formula | P1 + P10 + P8 |
| test-placeholder-fixes | registry + P8 + P6 |
| test-placeholder-registry | constructors + P6 + P8 |
| test-placeholders | P8 + P6 + P2 |
| test-plot-build | P11 |
| test-prune-empty-substitution | P9 + G2 + G3 + G4 + G5 |
| test-publication-loop | P4 + P6 + P5 |
| test-refine-items | P4 + registry |
| test-runtime-feedback | safe wrappers around P8/P11 |
| test-runtime-input-spec | P7 |
| test-safety-fixes / test-safety-hardening | P5 |
| test-shared-placeholder | P3 + P8 + P12 |
| test-step3-changes | P7 + P6 + P9 |
| test-supported-use-cases | end-to-end |
| test-unsupported-use-cases | P1 + P6 |
| test-upload | upload built-in + P5 (injection check) |
| test-utils | P1 + P9 helpers |
| test-validation | registry + state + ui_text validators |
| test-var-piped-data | P2 |
| test-w1-w6-improvements | P8 + registry |

## Acceptance Criteria

1. Every `test_that` in `tests/testthat/test-*.R` whose behavior is in scope (i.e., not testing an internal API name that is being renamed) has an equivalent test in the rewritten suite, passing.
2. No `bait` / gsub-style string substitution appears in the new core.
3. `ptr_compute_data_pipeline_info` and `ptr_resolve_data_pipeline_expr` (and their helpers) do not exist in the new core.
4. `R CMD check` passes with 0 errors, 0 warnings.
5. Custom placeholder authoring requires ≤ 4 hooks for `*_value`, ≤ 5 for `*_consumer`, ≤ 5 for `*_source` (counting `keyword` and `copy_defaults` as hooks).
6. One prune visitor (P9) replaces today's three pruners (`prune_empty_substitution_artifacts`, `ptr_remove_empty_nonstandalone_layers`, `check_remove_null`).
7. Pipe surface preservation: round-trip every test in `test-parse-formula.R` involving `|>` or `%>%` without string substitution.
8. `paintr-copy.R` and `paintr-options.R` are untouched (no changes to source code; tests may need adjustment if they reach into renamed internals).
9. The BDD manual (`core-rewrite-bdd.md`) drives test-writing for every pass; every scenario in the manual either has a corresponding test or is explicitly out of scope.

## Decision Log

| # | decision | alternatives | reason |
|---|---|---|---|
| 1 | Scope = core + registry contract | core only; whole package; new package | UI/app/copy follow naturally; registry is where cruft lives |
| 2 | Back-compat = anything goes | nothing breaks; contract may break; signatures may break | no real users; maximum design freedom |
| 3 | AST shape = typed wrappers around R AST | native R AST only; sidecar metadata; pure typed tree | clean multi-pass + free `eval()` with no round-trip |
| 4 | Source vs consumer = two distinct constructors | one class with role; position-decides; single class | distinct contracts; easier authoring; one invariant per class |
| 5 | Provenance = `ptr_user_expr` wrapper | parallel `orig` tree; node-attribute flag | uniform visitor rule: `prune.ptr_user_expr <- identity` |
| 6 | Pipe handling = typed pipeline node with op | string bait; fold-then-restore | one tree consumed by both render and eval |
| 7 | Coverage approach = matrix-first | trust-and-iterate | "stacking patches" is exactly what we'd reproduce otherwise |
| 8 | Verify trickiest gaps before lock | lock now | reading test bodies surfaced the dual-upstream and atomic-snapshot details |

## References

- Executable behavior: [core-rewrite-bdd.md](core-rewrite-bdd.md)
- Existing accepted decision touching this work: [checkbox-defaults.md](checkbox-defaults.md) — preserved behavior maps to G1 commitment above
