---
name: core-rewrite-bdd
type: decision
status: accepted
scope: [bdd, behavior-spec, parse, runtime, placeholders, pipeline, prune, ui-bridge]
created: 2026-05-05
related: [core-rewrite.md]
---

# Core Rewrite — BDD Manual

This document is the executable behavior surface for the rewrite designed in [core-rewrite.md](core-rewrite.md). Each scenario is one Given/When/Then triple. Implementation derives one test (or one assertion within a test) per scenario. Scenarios are organized by pass (P1–P12) and then by cross-cutting commitments (G1–G9). End-to-end scenarios at the bottom exercise multiple passes together.

## How to read

- **Given** is precondition state: a formula, an input snapshot, a registry, server state.
- **When** is the trigger: a function call, a Shiny click, an input change.
- **Then** is the observable outcome: a returned value's shape, an error class, a UI tag content, a snapshot side effect.
- Scenarios use the formula syntax confirmed unchanged by this rewrite: bare-symbol placeholders (`var`, `text`, `num`, `expr`, `upload`) and call-form `keyword(shared = "key")`.
- Symbols `<missing>`, `<cached>`, `<subtree>` denote internal node markers — not literal user-facing values.

---

## P1 — translate (string → typed tree)

### P1.1 — Happy path: simple ggplot formula

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point()"`
- **When** translate runs
- **Then** the result is a `ptr_root` with two `ptr_layer` children named `ggplot` and `geom_point`; the `ggplot` layer's `data_arg` is a `ptr_literal` for `mtcars`; no `ptr_pipeline` node appears.

### P1.2 — Magrittr pipe rewritten to native pipe form structurally

- **Given** the formula `"mtcars %>% ggplot(aes(x = mpg))"`
- **When** translate runs
- **Then** the result is a `ptr_root` with one `ptr_layer` named `ggplot`; the layer's `data_arg` is a `ptr_pipeline` whose `op` field equals `"%>%"`; render (P10) of this tree reproduces `%>%` exactly.

### P1.3 — Native pipe preserved as native pipe

- **Given** the formula `"mtcars |> ggplot(aes(x = mpg))"`
- **When** translate runs
- **Then** the layer's `data_arg` is a `ptr_pipeline` with `op = "|>"`.

### P1.4 — Mixed pipe chain preserved per stage

- **Given** the formula `"mtcars %>% head(num) |> ggplot(aes(x = mpg))"`
- **When** translate + render runs
- **Then** code text contains exactly one `%>%` between `mtcars` and `head(num)` and exactly one `|>` between `head(num)` and `ggplot(...)`.

### P1.5 — Top-level `+` split into layers

- **Given** the formula `"ggplot(mtcars) + geom_point() + geom_smooth() + labs(title = 'x')"`
- **When** translate runs
- **Then** `ptr_root$layers` has length 4 with names `c("ggplot", "geom_point", "geom_smooth", "labs")` in order.

### P1.6 — Duplicate layer names get suffixes

- **Given** the formula `"ggplot(mtcars) + geom_point() + geom_point() + geom_point()"`
- **When** translate runs
- **Then** layer names are `c("ggplot", "geom_point", "geom_point-2", "geom_point-3")`.

### P1.7 — `pkg::fn` head parses

- **Given** the formula `"ggplot2::ggplot(mtcars) + ggplot2::geom_point()"`
- **When** translate runs
- **Then** layer names are `c("ggplot", "geom_point")` (bare names) and each layer's `expr` retains the `::` head intact.

### P1.8 — `pkg:::fn` head parses

- **Given** the formula `"pkg:::internal_fn() + geom_point()"`
- **When** translate runs
- **Then** the first layer's name is `"internal_fn"` and the original `:::` head is preserved on the underlying expr.

### P1.9 — Bare-symbol placeholder as trailing layer

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point() + expr"`
- **When** translate runs
- **Then** the third layer is a `ptr_ph_value` with keyword `expr`, not a `ptr_call`; its position is at layer level.

### P1.10 — Quoted placeholders are not placeholders

- **Given** the formula `"ggplot(mtcars, aes(x = \"var\", y = \"var\")) + geom_point()"`
- **When** translate runs
- **Then** the `aes` call's args are two character literals `"var"`; no `ptr_ph_*` node is produced for them.

### P1.11 — Single-layer formula without `+`

- **Given** the formula `"ggplot(mtcars, aes(x = mpg, y = hp))"`
- **When** translate runs
- **Then** `ptr_root$layers` has length 1 named `"ggplot"`.

### P1.12 — Multi-expression formulas rejected

- **Given** the formula `"ggplot(mtcars)\nggplot(iris)"` (two top-level expressions separated by newline)
- **When** translate runs
- **Then** `rlang::abort` fires with a message naming "exactly one top-level expression" and a hint about missing `+`; the same applies to `;` separation and trailing `;`.

### P1.13 — Empty/whitespace formulas rejected

- **Given** the formula `""`, `" "`, `"\n"`, or `"\t\n"`
- **When** translate runs
- **Then** `rlang::abort` fires with a message containing "empty or whitespace".

### P1.14 — Depth limit triggers abort

- **Given** an expression nested 105 levels deep (e.g., `f(f(f(...)))`)
- **When** translate runs
- **Then** `rlang::abort` fires with a message naming the limit value (default 100); 100 levels passes; 101 levels aborts.

### P1.15 — Type guards on input

- **Given** any of `NULL`, `123`, `c("a", "b")`, or `quote(x)` as the formula argument
- **When** translate runs
- **Then** an assertion error fires; only a single character string is accepted.

### P1.16 — Call-form placeholder with `shared` parses

- **Given** the formula `"ggplot(aes(x = var(shared = \"axis\"), y = var(shared = \"axis\"))) + geom_point()"`
- **When** translate runs
- **Then** both `var` positions become `ptr_ph_data_consumer` nodes with `shared = "axis"`.

### P1.17 — Call-form placeholder with no args is equivalent to bare symbol

- **Given** the formula `"ggplot(aes(x = var())) + geom_point()"`
- **When** translate runs
- **Then** the result equals translation of `"ggplot(aes(x = var)) + geom_point()"` (same node class, `shared = NULL`).

### P1.18 — Call-form placeholder rejects unknown args

- **Given** the formula `"ggplot(aes(x = var(unknown = \"x\"))) + geom_point()"`
- **When** translate runs
- **Then** `rlang::abort` fires; only `shared` is allowed.

### P1.19 — Call-form placeholder rejects positional arg

- **Given** the formula `"ggplot(aes(x = var(\"x_axis\"))) + geom_point()"`
- **When** translate runs
- **Then** `rlang::abort` fires; positional args in placeholder call form are rejected.

### P1.20 — Empty `shared` string rejected

- **Given** the formula `"... var(shared = \"\")"`
- **When** translate runs
- **Then** `rlang::abort` fires.

### P1.21 — Comments dropped

- **Given** the formula `"mtcars |> head(num) |> # trim rows\nggplot(aes(x = mpg))"`
- **When** translate runs
- **Then** the resulting tree has no record of the comment; render (P10) emits no `#` characters.

---

## P2 — classify-data (annotate `ptr_ph_data_*` with upstream)

### P2.1 — `var` inside `aes()` of `ggplot(df, ...)` sees `df`

- **Given** the formula `"ggplot(mtcars, aes(x = var)) + geom_point()"`
- **When** classify runs
- **Then** the `var` node's `upstream` is the `ptr_literal` for `mtcars`.

### P2.2 — `var` inside `aes()` of `ggplot(...)` with named `data =` sees the named arg

- **Given** the formula `"ggplot(data = mtcars, aes(x = var)) + geom_point()"`
- **When** classify runs
- **Then** the `var` node's `upstream` is the `ptr_literal` for `mtcars`.

### P2.3 — `var` in geom inherits from ggplot

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_line(aes(y = var))"`
- **When** classify runs
- **Then** `geom_line`'s child `var` has `upstream` pointing at the `ggplot` layer's `data_arg` (mtcars).

### P2.4 — Geom with explicit `data` shadows inheritance

- **Given** the formula `"ggplot(mtcars) + geom_line(data = iris, aes(y = var))"`
- **When** classify runs
- **Then** the `var` inside `geom_line` has `upstream = ptr_literal(iris)`, not `mtcars`.

### P2.5 — Pipeline stage k > 1 sees prior stages as upstream

- **Given** the formula `"mtcars |> filter(year >= num) |> ggplot(aes(x = var))"`
- **When** classify runs
- **Then** the `num` node has `upstream = ptr_literal(mtcars)` (inherited stage-1 ctx); the `var` node has `upstream = ptr_pipeline(stages = c(mtcars, filter(year >= num)), op = "|>")`.

### P2.6 — Pipeline stage 1 inherits enclosing `ctx_data`

- **Given** the formula `"ggplot(data = mtcars |> filter(num > 0))"` where `num` is the only placeholder
- **When** classify runs
- **Then** `num`'s upstream is `ptr_literal(mtcars)` (the stage-1 `mtcars` itself).

### P2.7 — Source placeholder has no upstream

- **Given** the formula `"ggplot(data = upload(shared = \"ds\")) + geom_point(aes(x = var, y = var))"`
- **When** classify runs
- **Then** the `upload` node has `upstream = NULL` and is classified `ptr_ph_data_source`; both `var` nodes have `upstream` pointing at the `upload` source node.

### P2.8 — Two var nodes in the same pipeline share an upstream subtree pointer

- **Given** the formula `"mtcars |> filter(num > 0) |> ggplot(aes(x = var, y = var))"`
- **When** classify runs
- **Then** the two `var` nodes have `identical(node1$upstream, node2$upstream)` true at the subtree-pointer level.

### P2.9 — Chained var pipeline assigns position-correct upstream per var

- **Given** the formula `"mtcars |> select(var) |> select(var) |> ggplot(aes(x = var))"`
- **When** classify runs
- **Then** the first `select`'s `var` has `upstream = mtcars`; the second `select`'s `var` has `upstream = mtcars |> select(var)`; the `aes` `var` has `upstream = mtcars |> select(var) |> select(var)`.

### P2.10 — Circular reference rejected

- **Given** a (synthetic) AST in which a `ptr_ph_data_consumer`'s upstream subtree contains the same node id
- **When** classify validates
- **Then** `rlang::abort` fires with a message about a circular reference.

### P2.11 — Bare-symbol non-keyword data arg

- **Given** the formula `"ggplot(my_local_df, aes(x = var))"` where `my_local_df` is a free symbol
- **When** classify runs
- **Then** `var$upstream` is `ptr_literal(my_local_df)`; resolution at runtime evaluates this symbol in `eval_env`.

### P2.12 — Layer with no data placeholder is absent from any pipeline registry

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point()"` (no `var`/`upload`)
- **When** classify runs
- **Then** no `ptr_ph_data_*` nodes exist; no upstream metadata is attached anywhere.

---

## P3 — shared-binding

### P3.1 — Two var nodes with same shared key resolve to one canonical id

- **Given** the formula `"ggplot(aes(x = var(shared = \"axis\"), y = var(shared = \"axis\"))) + geom_point()"`
- **When** shared-binding runs
- **Then** both nodes' `id` field is rewritten to a single canonical id; UI build (P6) emits exactly one widget for the pair.

### P3.2 — Different shared keys remain distinct

- **Given** the formula `"ggplot(aes(x = var(shared = \"x\"), y = var(shared = \"y\"))) + geom_point()"`
- **When** shared-binding runs
- **Then** two distinct canonical ids result; two widgets render.

### P3.3 — Shared metadata surfaces in runtime input spec

- **Given** the formula `"... + geom_point(size = num(shared = \"size_filter\"))"`
- **When** P7 builds the runtime input spec
- **Then** the placeholder's row has `shared = "size_filter"`.

### P3.4 — Upload companion row carries shared

- **Given** the formula `"ggplot(data = upload(shared = \"ds\"))"`
- **When** P7 builds the runtime input spec
- **Then** both the upload row and its `upload_name` companion row have `shared = "ds"`.

### P3.5 — Bare-symbol placeholder has `shared = NA` in input spec

- **Given** the formula `"ggplot(aes(x = var)) + geom_point()"`
- **When** P7 builds the runtime input spec
- **Then** the `var` row has `shared = NA_character_`.

### P3.6 — Shared binding overrides input snapshot at substitute time

- **Given** a `ptr_ph_value` for `num(shared = "size")` and `shared_bindings = list(size = reactiveVal(5))`
- **When** P8 substitutes
- **Then** the substituted value is `5` even if `input_snapshot` for that id is NULL or different.

### P3.7 — Missing shared binding falls back to ptr_missing

- **Given** a placeholder with `shared = "absent"` and an empty `shared_bindings` list
- **When** P8 substitutes
- **Then** the result is `ptr_missing()` (does not look at input_snapshot for shared placeholders).

### P3.8 — UI omits widgets for shared placeholders

- **Given** the formula `"... + geom_point(size = num(shared = \"size_filter\"))"`
- **When** P6 builds the UI list
- **Then** the per-layer control list contains no widget for the `num` node (shared widgets render once at app-level via `ptr_app_grid`'s `shared_ui`).

---

## P4 — id-encoding

### P4.1 — Raw id format

- **Given** a `ptr_ph_value` for `num` at index path `c(2, 3)` in layer `geom_point`, with `shared = NULL`
- **When** id-encoding runs
- **Then** the raw id equals `"geom_point+2.3+num+NA"` (or the agreed delimiter form documented in registry helpers).

### P4.2 — `ns_fn` applied at UI emit

- **Given** raw id `"geom_point+2.3+num+NA"` and `ns_fn = shiny::NS("plot1")`
- **When** P6 emits the widget
- **Then** the widget's HTML id is `"plot1-geom_point+2.3+num+NA"`.

### P4.3 — `ns_fn = shiny::NS(NULL)` is identity

- **Given** raw id `"x"` and `ns_fn = shiny::NS(NULL)`
- **When** ns is applied
- **Then** the rendered id is `"x"` unchanged.

### P4.4 — Two distinct namespaces produce disjoint rendered ids

- **Given** the same parsed object rendered with `ns_fn = shiny::NS("a")` and `ns_fn = shiny::NS("b")`
- **When** P6 emits widgets
- **Then** every rendered id under `a` differs from every rendered id under `b`.

### P4.5 — Companion id for upload

- **Given** an upload source with raw id `"ggplot+1+upload+NA"`
- **When** id-encoding derives the companion
- **Then** the companion id equals `ptr_upload_name_id("ggplot+1+upload+NA")` per the upload built-in's `companion_id_fn`.

### P4.6 — Layer-update-data id only present for pipeline layers

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point()"` (no pipeline)
- **When** P4 derives layer ids
- **Then** no `<layer>_update_data` id is created.

### P4.7 — Layer-update-data id present for pipeline layers

- **Given** the formula `"mtcars |> filter(num > 0) |> ggplot(aes(x = var))"`
- **When** P4 derives layer ids
- **Then** the `ggplot` layer has an `update_data` id derived via `ptr_update_data_input_id("ggplot")`.

### P4.8 — Non-function `ns_fn` rejected

- **Given** `ns_fn = "string"` or `ns_fn = 42L` or `ns_fn = NULL`
- **When** id-encoding runs
- **Then** `rlang::abort` fires.

### P4.9 — `ptr_runtime_input_spec` returns raw input_ids regardless of ns

- **Given** a parsed object and any `ns_fn`
- **When** P7 builds the input spec
- **Then** the `input_id` column contains raw (un-namespaced) ids; namespacing happens only at UI emit.

---

## P5 — safety

### P5.1 — Denylist symbol blocked

- **Given** the formula `"ggplot(mtcars) + geom_point(data = system('id'))"`
- **When** translate calls P5
- **Then** `rlang::abort` fires with a message identifying `system`.

### P5.2 — Bare denylist symbol blocked

- **Given** the formula `"ggplot(mtcars) + sapply(1, system)"`
- **When** P5 runs
- **Then** abort fires; `system` as a higher-order arg is caught.

### P5.3 — Namespaced denylist call blocked

- **Given** the formula `"... + base::system('id')"`
- **When** P5 runs
- **Then** abort fires; `extract_fn_names` returns the qualified form.

### P5.4 — IIFE wrapping denylist call blocked

- **Given** the formula `"(function(x) x)(system('id'))"`
- **When** P5 runs
- **Then** abort fires.

### P5.5 — Anonymous function body checked

- **Given** the formula `"... + sapply(1:3, function(x) file.remove('a'))"`
- **When** P5 runs
- **Then** abort fires; the lambda body is recursed into.

### P5.6 — Lambda formal default value checked

- **Given** the formula `"... + (function(x = system('id')) x)()"`
- **When** P5 runs
- **Then** abort fires; default values of formals are checked.

### P5.7 — Pairlist contents checked

- **Given** an AST where a pairlist contains a denylisted symbol
- **When** P5 runs
- **Then** abort fires.

### P5.8 — String literal in denylist blocked

- **Given** the formula `"exec('system', 'id')"` (where `'system'` is a string literal arg)
- **When** P5 runs
- **Then** abort fires; string literals are checked.

### P5.9 — Safe expression unaffected

- **Given** the formula `"ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + scale_x_log10()"`
- **When** P5 runs
- **Then** no error; a normal ggplot2 call passes.

### P5.10 — `expr_check = FALSE` bypasses

- **Given** a formula containing `system('id')` and `expr_check = FALSE`
- **When** P5 is invoked
- **Then** no check runs; the dangerous expression passes through.

### P5.11 — Allowlist mode

- **Given** an `allow_list` argument and a formula containing a function not on the allowlist
- **When** P5 runs in allowlist mode
- **Then** the disallowed function's call aborts with a message.

### P5.12 — Custom `deny_list` adds entries

- **Given** `deny_list = c("my_dangerous_fn")` and a formula calling `my_dangerous_fn()`
- **When** P5 runs
- **Then** abort fires.

### P5.13 — Super-assign blocked

- **Given** the formula `"ggplot(mtcars) + (x <<- 1)"`
- **When** P5 runs
- **Then** abort fires.

### P5.14 — Assignment form `body<-` blocked

- **Given** the formula `"`body<-`(f, value = quote(x))"`
- **When** P5 runs
- **Then** abort fires.

### P5.15 — Depth limit triggers abort

- **Given** an AST whose depth exceeds 100 levels
- **When** P5 walks
- **Then** abort fires with a message naming the limit.

### P5.16 — Custom placeholder `resolve_expr` returning a denied character literal blocked

- **Given** a custom placeholder whose `resolve_expr` returns the string `"system"`
- **When** P8 invokes the hook and routes the result through P5
- **Then** abort fires; the substituted string is treated as a literal that must pass denylist.

### P5.17 — `ptr_assemble_plot` runs P5 on each layer

- **Given** `plot_expr_list` containing a dangerous expression
- **When** P11 runs
- **Then** abort fires before any `eval`; safety is checked again post-substitution.

### P5.18 — Empty `plot_expr_list` is a no-layers error, not a safety error

- **Given** `plot_expr_list = list()` after pruning removes everything
- **When** P11 runs
- **Then** abort fires with a "no layers" message, not a safety message.

---

## P6 — ui-build

### P6.1 — `text` placeholder UI

- **Given** a `ptr_ph_value` for `text` and the default copy
- **When** P6 dispatches
- **Then** a `shiny::textInput` (or registry-equivalent) renders with id = the rendered id and label from copy resolution.

### P6.2 — `num` placeholder UI

- **Given** a `ptr_ph_value` for `num`
- **When** P6 dispatches
- **Then** a numeric input renders with the appropriate label.

### P6.3 — `var` consumer UI receives resolved cols

- **Given** a `ptr_ph_data_consumer` for `var` whose upstream resolves to a data frame with columns `c("mpg", "hp")`
- **When** P6 dispatches
- **Then** `pickerInput` renders with `choices = c("mpg", "hp")`.

### P6.4 — `var` consumer with NULL upstream renders empty choices

- **Given** a `var` consumer whose upstream cannot be resolved (e.g., upload not yet provided)
- **When** P6 dispatches
- **Then** the picker renders with empty choices and a help tip from copy.

### P6.5 — `upload` source UI emits paired widgets

- **Given** a `ptr_ph_data_source` for `upload`
- **When** P6 dispatches
- **Then** a `fileInput` AND a `textInput` (for the dataset name companion) both render with their respective ids.

### P6.6 — Layer switcher includes Data sub-tab when pipeline placeholders present

- **Given** the formula `"mtcars |> head(num) |> ggplot(aes(x = var))"`
- **When** P6 builds the layer switcher for the `ggplot` layer
- **Then** the layer panel contains a Data sub-tab AND a Controls sub-tab.

### P6.7 — Layer switcher omits Data sub-tab when no pipeline placeholders

- **Given** the formula `"ggplot(mtcars, aes(x = var)) + geom_point()"`
- **When** P6 builds the layer switcher
- **Then** the layer panel renders flat controls; no Data sub-tab.

### P6.8 — Layer toggle checkbox rendered for non-ggplot layers

- **Given** the formula `"ggplot(mtcars, aes(x = var)) + geom_point()"`
- **When** P6 builds the layer panel for `geom_point`
- **Then** a checkbox renders at the top of the panel.

### P6.9 — Layer toggle defaults to TRUE when key absent from `checkbox_defaults`

- **Given** `checkbox_defaults = c(geom_smooth = FALSE)` and a `geom_point` layer
- **When** P6 emits the checkbox tag
- **Then** the rendered HTML contains `checked="checked"`.

### P6.10 — Layer toggle defaults to FALSE when key set FALSE

- **Given** `checkbox_defaults = c(geom_point = FALSE)` and a `geom_point` layer
- **When** P6 emits the checkbox tag
- **Then** the rendered HTML does NOT contain `checked="checked"`.

### P6.11 — Layer toggle FALSE adds `ptr-layer-disabled` class

- **Given** `checkbox_defaults = c(geom_point = FALSE)`
- **When** P6 emits the layer panel
- **Then** the content div carries class `ptr-layer-disabled` at render.

### P6.12 — `ggplot` layer has no checkbox

- **Given** any formula with a `ggplot` layer
- **When** P6 emits the layer panel for `ggplot`
- **Then** no checkbox tag is emitted, regardless of `checkbox_defaults`.

### P6.13 — Copy resolution applied to labels

- **Given** `ui_text = list(params = list(num = list(label = "Pick a number")))`
- **When** P6 builds the `num` widget
- **Then** the rendered label is `"Pick a number"`.

### P6.14 — Custom placeholder UI renders via registered `build_ui`

- **Given** a custom `*_consumer` for `numeric_col` with a `pickerInput`-based `build_ui`
- **When** P6 dispatches
- **Then** the registered `build_ui` is invoked with `cols` resolved from upstream.

### P6.15 — `ptr_user_expr` emits no UI

- **Given** a substituted tree containing a `ptr_user_expr` provenance node
- **When** P6 walks
- **Then** no UI is emitted for the provenance node itself.

### P6.16 — Copy alias normalization

- **Given** `ui_text` with a `size` key while the placeholder uses canonical `linewidth`
- **When** P6 resolves copy
- **Then** the `size` alias normalizes to `linewidth` and the resolved label matches.

---

## P7 — input-spec

### P7.1 — One row per placeholder in formula order

- **Given** the formula `"ggplot(aes(x = var, y = var)) + geom_point(color = text)"`
- **When** P7 runs
- **Then** rows appear in formula order: two `var` placeholder rows then one `text` placeholder row.

### P7.2 — Upload companion row immediately follows upload row

- **Given** the formula `"ggplot(data = upload)"`
- **When** P7 runs
- **Then** the upload placeholder row is followed by an `upload_name` companion row with the same `source_id`.

### P7.3 — Layer checkbox rows follow placeholder rows

- **Given** the formula `"ggplot(aes(x = var)) + geom_point() + geom_smooth()"`
- **When** P7 runs
- **Then** the table contains placeholder rows first, then `layer_checkbox` rows for `geom_point` and `geom_smooth` (none for `ggplot`).

### P7.4 — Layer update-data rows present only for pipeline layers

- **Given** the formula `"mtcars |> head(num) |> ggplot(aes(x = var))"`
- **When** P7 runs
- **Then** a `layer_update_data` row exists for the `ggplot` layer.

### P7.5 — Empty formula returns zero-row data frame with all columns

- **Given** a parsed object whose formula has no placeholders and only a single `ggplot` layer
- **When** P7 runs
- **Then** the result is a 0-row data frame with the documented columns: `input_id`, `role`, `layer_name`, `keyword`, `param_key`, `source_id`, `shared`.

### P7.6 — Resolved duplicate layer names preserved

- **Given** the formula `"ggplot() + geom_point() + geom_point() + geom_point(size = num)"`
- **When** P7 runs
- **Then** the `num`'s row has `layer_name = "geom_point-3"` and the layer-checkbox rows match the deduplicated names.

### P7.7 — Custom placeholder keywords surface

- **Given** a registry containing a custom keyword `numeric_col`
- **When** P7 runs over a formula using `numeric_col`
- **Then** the row's `keyword` field equals `"numeric_col"`.

### P7.8 — Piped vs symbol-data formulas yield identical specs

- **Given** the formulas `"mtcars |> ggplot(aes(x = var))"` and `"ggplot(mtcars, aes(x = var))"`
- **When** P7 runs on each
- **Then** the resulting input specs are identical (modulo layer-name auto-discovery).

---

## P8 — substitute

### P8.1 — `text` strips matched leading/trailing double quotes

- **Given** `input_snapshot[[text_id]] = "\"hello\""`
- **When** P8 substitutes
- **Then** the substituted expression evaluates to `"hello"` (one pair stripped).

### P8.2 — `text` strips matched single quotes

- **Given** `input_snapshot[[text_id]] = "'hello'"`
- **When** P8 substitutes
- **Then** the substituted expression evaluates to `"hello"`.

### P8.3 — `text` leaves unbalanced quotes alone

- **Given** `input_snapshot[[text_id]] = "\"hello"`
- **When** P8 substitutes
- **Then** the substituted expression evaluates to `"\"hello"` exactly.

### P8.4 — `text` leaves bare text unchanged

- **Given** `input_snapshot[[text_id]] = "hello"`
- **When** P8 substitutes
- **Then** the substituted expression evaluates to `"hello"`.

### P8.5 — `num` returns `ptr_missing` for NULL

- **Given** `input_snapshot[[num_id]] = NULL`
- **When** P8 substitutes
- **Then** the substituted node is `ptr_missing()`; no warning fires.

### P8.6 — `num` returns `ptr_missing` for NA

- **Given** `input_snapshot[[num_id]] = NA_real_`
- **When** P8 substitutes
- **Then** `ptr_missing()`.

### P8.7 — `num` returns `ptr_missing` for empty numeric/character

- **Given** `input_snapshot[[num_id]] = numeric(0)` or `character(0)`
- **When** P8 substitutes
- **Then** `ptr_missing()`.

### P8.8 — `num` returns the literal for valid input

- **Given** `input_snapshot[[num_id]] = 5`
- **When** P8 substitutes
- **Then** the substituted expression equals `5` (numeric literal).

### P8.9 — `expr` returns `ptr_user_expr(parsed)` for valid input

- **Given** `input_snapshot[[expr_id]] = "geom_smooth(method = 'lm')"`
- **When** P8 substitutes
- **Then** the substituted node is a `ptr_user_expr` whose inner is the parsed call.

### P8.10 — `expr` returns `ptr_missing` for empty string

- **Given** `input_snapshot[[expr_id]] = ""`
- **When** P8 substitutes
- **Then** `ptr_missing()`.

### P8.11 — `expr` rejects multi-line input

- **Given** `input_snapshot[[expr_id]] = "geom_smooth()\nlabs()"`
- **When** P8 substitutes
- **Then** `rlang::abort` fires with a message about "exactly one expression".

### P8.12 — `var` returns symbol for valid column choice

- **Given** a `var` consumer whose upstream cols include `"mpg"` and `input_snapshot[[var_id]] = "mpg"`
- **When** P8 substitutes
- **Then** the substituted expression equals `quote(mpg)`.

### P8.13 — `var` aborts on column not in upstream

- **Given** a `var` consumer with upstream cols `c("mpg")` and `input_snapshot[[var_id]] = "cyl"`
- **When** P8 substitutes (with validation)
- **Then** `rlang::abort` fires; the message names the invalid column.

### P8.14 — `var` aborts on multiple selected columns

- **Given** `input_snapshot[[var_id]] = c("mpg", "hp")`
- **When** P8 substitutes
- **Then** `rlang::abort` fires.

### P8.15 — `upload` returns symbol for valid name

- **Given** an upload source with companion name `"my_data"` set
- **When** P8 substitutes
- **Then** the substituted expression equals `quote(my_data)` (a syntactically valid R name).

### P8.16 — `upload` aborts on injection attempt

- **Given** an upload companion name like `"x; system('id')"`
- **When** P8 substitutes
- **Then** `rlang::abort` fires before any eval; only valid R names allowed.

### P8.17 — `upload` returns `ptr_missing` for empty name

- **Given** the companion name is `""` or `NULL`
- **When** P8 substitutes
- **Then** `ptr_missing()`.

### P8.18 — `expr` placeholder produces `ptr_user_expr` provenance

- **Given** `input_snapshot[[expr_id]] = "theme()"` for an `expr` placeholder
- **When** P8 substitutes
- **Then** the substituted node is `ptr_user_expr(theme())`; downstream P9 will not prune it.

### P8.19 — Bare-symbol expr at layer level uses same provenance

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + expr"` and `input_snapshot[[expr_id]] = "geom_point()"`
- **When** P8 substitutes
- **Then** the substituted layer is a `ptr_user_expr(geom_point())`.

### P8.20 — Custom `resolve_expr` return-type whitelist

- **Given** a custom placeholder whose `resolve_expr` returns an `environment` or `raw` or `list` object
- **When** P8 substitutes
- **Then** `rlang::abort` fires with a message naming the offending type. Numeric, logical, integer, character, NULL, language, symbol, and expression all pass.

### P8.21 — Custom `resolve_expr` returning a function aborts

- **Given** a custom placeholder whose `resolve_expr` returns a `function`
- **When** P8 substitutes
- **Then** `rlang::abort` fires (functions are not in the whitelist).

### P8.22 — `ptr_missing` propagates positionally

- **Given** the formula `"... + filter(year >= num)"` with `num` unset
- **When** P8 substitutes (without P9 yet)
- **Then** the substituted call is `filter(year >= ptr_missing())`.

### P8.23 — Mixed NULL and non-NULL inputs

- **Given** a formula with two placeholders, one set, one NULL
- **When** P8 substitutes
- **Then** the set placeholder substitutes normally; the NULL one becomes `ptr_missing()`; no error fires.

### P8.24 — Shared bindings override snapshot

- **Given** a placeholder with `shared = "size"`, `shared_bindings = list(size = reactiveVal(7))`, and `input_snapshot` at that id is irrelevant
- **When** P8 substitutes
- **Then** the result reflects `7`.

---

## P9 — prune

### P9.1 — Positional missing arg drops via two paths (operator vs non-operator)

- **Given** a `ptr_call` for `filter(year >= ptr_missing())`
- **When** P9 prunes
- **Then** `>=` is an operator → operator-escalation path returns `ptr_missing()`. The outer `filter(ptr_missing())` is a non-operator with a positional `ptr_missing` arg → relaxed P9 drops the arg → `filter()` empty. Because `filter` is NOT in `default_drop_when_empty()`, the empty call survives and renders as `filter()` (eval: identity on `.data`). If the user adds `filter` to `safe_to_remove`, the empty call would drop instead.

### P9.2 — Named missing arg dropped

- **Given** a `ptr_call` for `geom_point(color = ptr_missing(), size = 3)`
- **When** P9 prunes
- **Then** the result is `geom_point(size = 3)`; the named `color` arg is silently dropped.

### P9.3 — Empty call in remove_set escalates

- **Given** a `ptr_call` for `theme(plot.title = ptr_missing())` and `theme` is in `remove_set`
- **When** P9 prunes
- **Then** the dropped `plot.title` arg leaves `theme()`; with `theme` in `remove_set`, the call escalates to `ptr_missing()`.

### P9.4 — Empty call NOT in remove_set survives

- **Given** a `ptr_call` for `pcp_theme()` (after pruning) with `pcp_theme` NOT in `remove_set`
- **When** P9 prunes
- **Then** the call survives as `pcp_theme()`.

### P9.5 — Standalone-eligible empty layer survives even when in remove_set

- **Given** `geom_point(color = ptr_missing())` with `geom_point` in `remove_set` and `is_standalone(name) = TRUE` for `geom_*`
- **When** P9 prunes
- **Then** the layer is preserved as `geom_point()`.

### P9.6 — User `safe_to_remove` extends remove_set

- **Given** the formula `"... + pcp_theme(title = text)"`, `text` unset, and `safe_to_remove = c("pcp_theme")`
- **When** P9 prunes via the public render entry point
- **Then** the `pcp_theme` layer is dropped.

### P9.7 — Pipeline stage with positional missing keeps the call empty

- **Given** the formula `"mtcars |> head(num) |> ggplot(aes(x = mpg))"` with `num` unset
- **When** P9 prunes
- **Then** the positional `ptr_missing` arg drops from `head`, leaving `head()` empty. `head` is NOT in `default_drop_when_empty()`, so the empty call survives and the pipeline renders as `mtcars |> head() |> ggplot(aes(x = mpg))`. Eval relies on `head`'s default `n = 6`. (Under strict P9 the entire `head(num)` call would have escalated, dropping the stage; that contradicted P12.1 and was relaxed.)

### P9.8 — Pipeline collapses to single stage when only one remains

- **Given** a pipeline whose only surviving stage is `mtcars`
- **When** P9 prunes
- **Then** the pipeline collapses to the `ptr_literal(mtcars)` directly (no `ptr_pipeline` wrapper).

### P9.9 — Pipeline with all stages dropped becomes missing

- **Given** a pipeline where every stage prunes to `ptr_missing`
- **When** P9 prunes
- **Then** the result is `ptr_missing()`.

### P9.10 — `ptr_user_expr` never pruned

- **Given** a `ptr_user_expr(theme())` (substituted from an `expr` placeholder; `theme` is in `remove_set`)
- **When** P9 prunes
- **Then** the result equals the input `ptr_user_expr(theme())`; the wrapper is preserved.

### P9.11 — `ptr_user_expr` containing a nested zero-arg call survives

- **Given** `ptr_user_expr(theme(plot.title = element_text()))`
- **When** P9 prunes
- **Then** the result is unchanged — even though `element_text()` is a zero-arg call.

### P9.12 — Operator escalation: comparison with sentinel operand

- **Given** `subset(mtcars, mpg > ptr_missing())`
- **When** P9 prunes
- **Then** `>` is an operator → operator-escalation path returns `ptr_missing()`. The outer `subset(mtcars, ptr_missing())` is non-operator with a positional `ptr_missing` arg → relaxed P9 drops the arg, leaving `subset(mtcars)` (one arg remaining, call survives).

### P9.13 — Operator escalation works for unary operators

- **Given** the bare unary call `!ptr_missing()`
- **When** P9 prunes
- **Then** `!` is in `pruneable_operator_names` → operator-escalation path returns `ptr_missing()`. (If wrapped as `filter(!ptr_missing())`, the outer `filter(ptr_missing())` would then drop the positional arg under relaxed P9, leaving `filter()` empty — which survives unless `filter` is in `safe_to_remove`.)

### P9.14 — Layer with `active = FALSE` dropped entirely

- **Given** a `ptr_layer` with `active = FALSE` (set by P8 from checkbox snapshot)
- **When** P9 prunes
- **Then** P9 returns NULL for that layer; root drops it.

### P9.15 — Anonymous-head call kept

- **Given** a call with an anonymous-function head `(function() 1)()` that became zero-arg after pruning
- **When** P9 prunes
- **Then** the call survives (no name match → no escalation).

### P9.16 — Namespaced call name matched against remove_set by bare name

- **Given** `patchwork::plot_layout(ncol = ptr_missing())` with `plot_layout` in `remove_set`
- **When** P9 prunes
- **Then** the named arg is dropped, the call becomes empty, and the bare name `plot_layout` matches `remove_set` → escalates to missing.

### P9.17 — Depth limit triggers abort

- **Given** an AST nested 105 levels
- **When** P9 walks
- **Then** abort fires with a message naming the depth limit.

### P9.18 — Invalid `safe_to_remove` rejected

- **Given** `safe_to_remove = list("a")`, `safe_to_remove = NA`, `safe_to_remove = "::"`, or `safe_to_remove = ""`
- **When** the public render entry point validates
- **Then** abort fires.

### P9.19 — `safe_to_remove = NULL` accepted

- **Given** `safe_to_remove = NULL`
- **When** validation runs
- **Then** the default `remove_set` applies; no error.

### P9.20 — `aes(x = var)` with var missing collapses

- **Given** the formula `"... + geom_point(aes(colour = var))"` with `var` unset
- **When** P9 prunes
- **Then** the named `colour` arg is dropped from `aes`, leaving `aes()` empty. `aes` IS in `default_drop_when_empty()` and is not standalone, so the empty call returns `ptr_missing()` (drop sentinel). The wrapping `geom_point(aes())` is non-operator with a positional `ptr_missing` arg → relaxed P9 drops the arg → `geom_point()` empty. `geom_point` is standalone-eligible, so the layer survives as `geom_point()`.

### P9.21 — Top-level `theme(plot.title = text)` collapses with text missing

- **Given** the formula `"ggplot(...) + theme(plot.title = text)"`, `text` unset, `theme` in default `remove_set`
- **When** P9 prunes
- **Then** the `theme` layer is dropped from `ptr_root$layers`.

### P9.22 — Top-level standalone `geom_point()` survives even when in remove_set

- **Given** the formula `"ggplot(mtcars) + geom_point(colour = var)"`, `var` unset, `geom_point` in `remove_set`
- **When** P9 prunes
- **Then** the resulting layer equals `geom_point()` (named arg dropped, layer kept because standalone).

---

## P10 — render-code

### P10.1 — Single-layer ggplot formula round-trips

- **Given** the typed tree for `"ggplot(mtcars, aes(x = mpg))"`
- **When** P10 renders
- **Then** the code text equals `"ggplot(mtcars, aes(x = mpg))"`.

### P10.2 — Layers joined with ` +\n  `

- **Given** the typed tree for `"ggplot(mtcars) + geom_point() + geom_smooth()"`
- **When** P10 renders
- **Then** the code text equals `"ggplot(mtcars) +\n  geom_point() +\n  geom_smooth()"`.

### P10.3 — Native pipe surface preserved

- **Given** the typed tree for `"mtcars |> ggplot(aes(x = mpg))"`
- **When** P10 renders
- **Then** the code text contains `|>`, not `%>%`, and the pipe call form is not folded into nested calls.

### P10.4 — Magrittr pipe surface preserved

- **Given** the typed tree for `"mtcars %>% ggplot(aes(x = mpg))"`
- **When** P10 renders
- **Then** the code text contains `%>%`, not `|>`.

### P10.5 — Mixed pipe chain preserved per-stage

- **Given** the typed tree for `"mtcars %>% head(num) |> ggplot(...)"`
- **When** P10 renders
- **Then** code text contains `%>%` between `mtcars` and `head` and `|>` between `head` and `ggplot`.

### P10.6 — Chained pipe drops middle-link arg when its placeholder is empty

- **Given** the typed tree for `"mtcars |> head(num) |> ggplot(...)"` with `num` empty (so head dropped by P9)
- **When** P10 renders
- **Then** code text equals `"mtcars |> ggplot(...)"`; no `head` call.

### P10.7 — `pkg::fn` heads preserved

- **Given** the typed tree for `"ggplot2::ggplot(mtcars) + ggplot2::geom_point()"`
- **When** P10 renders
- **Then** code text contains `ggplot2::ggplot(...) +\n  ggplot2::geom_point()`.

### P10.8 — Comments do not appear

- **Given** the typed tree for `"mtcars |> head(num) |> # trim\nggplot(...)"`
- **When** P10 renders
- **Then** code text contains no `#`.

### P10.9 — `ptr_user_expr` renders inner verbatim

- **Given** a `ptr_user_expr(theme(plot.title = element_text()))`
- **When** P10 renders
- **Then** the rendered text equals `"theme(plot.title = element_text())"`.

### P10.10 — Named args print as `name = value`

- **Given** a `ptr_call` for `geom_point(color = "red", size = 3)`
- **When** P10 renders
- **Then** the rendered text equals `"geom_point(color = \"red\", size = 3)"`.

### P10.11 — Code text reflects snapshotted data-placeholder values when snapshot supplied

- **Given** an Update Data click that snapped `num = 3`
- **When** P10 renders against the post-snapshot tree
- **Then** code text contains `head(3)`, not `head(num)`.

### P10.12 — Code text falls back to live input when no snapshot

- **Given** no snapshot supplied and current `input` has `num = 5`
- **When** P10 renders
- **Then** code text contains `head(5)`.

### P10.13 — Nested `data = ...` pipe round-trips

- **Given** the typed tree for `"ggplot(data = mtcars |> filter(num > 0))"`
- **When** P10 renders
- **Then** the rendered code preserves the inner pipeline form, not folded.

---

## P11 — eval

### P11.1 — Plain ggplot evaluates to a ggplot object

- **Given** the typed tree for `"ggplot(mtcars, aes(x = mpg)) + geom_point()"`
- **When** P11 evaluates
- **Then** the result inherits from class `"ggplot"`.

### P11.2 — Pipeline folds via `rlang::call2(op, ...)`

- **Given** the typed tree for `"mtcars |> head(2) |> ggplot(aes(x = mpg))"`
- **When** P11 evaluates
- **Then** the layer's expression is structurally equivalent to `ggplot(head(mtcars, 2), aes(x = mpg))`; eval succeeds.

### P11.3 — Empty plot list errors as no-layers

- **Given** an empty `plot_expr_list` after pruning removes everything
- **When** P11 runs
- **Then** abort fires with a "no layers" message.

### P11.4 — Optional layers unchecked produce base ggplot only

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point() + geom_smooth()"` with both layer checkboxes FALSE
- **When** P12 → P9 → P11 runs
- **Then** P11 evaluates a tree with only the `ggplot` layer; the result is a base ggplot.

### P11.5 — Column normalization on uploaded data

- **Given** an uploaded data frame with columns `c("if", "for")` (reserved words)
- **When** P11 materializes via `ptr_resolve_upstream`
- **Then** columns are renamed deterministically (e.g., `c("if_", "for_")`) before being exposed as `var` choices and used in eval.

### P11.6 — Pre-existing `if_` not silently overwritten

- **Given** uploaded data with columns `c("if", "if_")`
- **When** column normalization runs
- **Then** the original `if_` column is preserved; the renamed `if` becomes `if_2` (or equivalent disambiguation).

### P11.7 — Eval-time errors caught and surfaced

- **Given** a tree whose evaluation throws (e.g., a missing local data object)
- **When** P12's safe wrapper runs P11
- **Then** the error is captured; `code_text` still updates; UI surfaces the formatted error message.

### P11.8 — Render-time faceting errors captured

- **Given** a tree producing a plot whose `ggplot_build` fails
- **When** `ptr_validate_plot_render_safe` runs
- **Then** the failure is reported; the user sees a formatted error.

---

## P12 — server-state

### P12.1 — Initial cache seeded via trim-to-root

- **Given** the formula `"mtcars |> head(num) |> ggplot(aes(x = var, y = var))"`, app start, no inputs set
- **When** P12 initializes
- **Then** `state$resolved_data[["ggplot"]]()` is `head(mtcars)` (default 6 rows) — the result of P9 pruning the empty `head(num)` arg, then P11 evaluating; column names match `mtcars`.

### P12.2 — Update Data click refreshes cache from current inputs

- **Given** the formula above; `num` set to `3`; Update Data button clicked
- **When** P12 handles the click
- **Then** `state$resolved_data[["ggplot"]]()` becomes `head(mtcars, 3)` (3 rows).

### P12.3 — Stale flag flips TRUE on input divergence

- **Given** an established snapshot
- **When** the user changes a placeholder input without clicking Update
- **Then** `state$is_stale_env[["ggplot"]]()` becomes TRUE.

### P12.4 — Stale flag flips back to FALSE on click

- **Given** a stale flag
- **When** Update Data is clicked
- **Then** the flag returns to FALSE; snapshot equals current inputs.

### P12.5 — Atomic snapshot validation: out-of-upstream column rejected

- **Given** the formula `"mtcars |> select(var) |> select(var) |> ggplot(...)"`, second `var` set to `"cyl"` while first `var` set to `"mpg"` (so second's upstream cols = `c("mpg")`)
- **When** Update Data clicked
- **Then** snapshot is **untouched**; cache remains at prior value (here, `select(mtcars, mpg)`).

### P12.6 — Pipeline var dropdown stays anchored to per-position upstream after click

- **Given** the formula `"mtcars |> select(var) |> ggplot(aes(x = var))"`, var click on the inner `select(var)` set to `"mpg"`
- **When** the dropdown UI re-renders
- **Then** the inner `select(var)`'s dropdown choices remain the full `mtcars` columns (its upstream is `mtcars`), not the post-snapshot 1-column frame.

### P12.7 — Update click on unresolvable pipeline leaves cache untouched

- **Given** the formula `"nonexistent_object |> head(num) |> ggplot(...)"`, `num = 3`, Update clicked
- **When** P12 attempts to evaluate
- **Then** an inline error surfaces; cache is not corrupted; `state$resolved_data[["ggplot"]]()` remains NULL.

### P12.8 — `ptr_build_var_column_map` prefers cached data

- **Given** a `resolved_data = list(ggplot = reactiveVal(data.frame(only_one = 1:3)))`
- **When** column map is built
- **Then** the var's choices equal `c("only_one")`, not the live-pipeline mtcars columns.

### P12.9 — `ptr_build_var_column_map` falls back to live resolution when cache NULL

- **Given** `resolved_data = list(ggplot = reactiveVal(NULL))`
- **When** column map is built
- **Then** choices reflect the live-pipeline result.

### P12.10 — `ptr_gg_extra` captures expressions and evaluates them

- **Given** a server state and a call `ptr_gg_extra(state, list(scale_x_log10()))`
- **When** the call returns
- **Then** `state$extras` contains the evaluated layer; `ptr_extract_code` will append `+ scale_x_log10()`.

### P12.11 — `ptr_gg_extra` no-op when input list empty

- **Given** `ptr_gg_extra(state, list())`
- **When** the call returns
- **Then** `state$extras` is an empty list.

### P12.12 — `ptr_gg_extra` does not write extras when evaluation fails

- **Given** an extra expression that fails to evaluate
- **When** `ptr_gg_extra` runs
- **Then** `state$extras` is unchanged; an error surfaces.

### P12.13 — `ptr_extract_code` appends extras when runtime ok

- **Given** a successful runtime result and one extra `scale_x_log10()`
- **When** `ptr_extract_code(state)` runs
- **Then** the returned code text ends with ` +\n  scale_x_log10()`.

### P12.14 — `ptr_extract_code` suppresses extras when runtime not ok

- **Given** a failed runtime and an extra
- **When** `ptr_extract_code` runs
- **Then** the returned text contains base code only; no extras appended.

### P12.15 — `ptr_module_server` returns module-scoped state

- **Given** two `ptr_module_*` instances with namespaces `"a"` and `"b"`
- **When** their servers run
- **Then** ids in module `a` differ from those in module `b`; state objects are independent.

### P12.16 — `ptr_app_grid` wires shared widgets across plots

- **Given** a `ptr_app_grid(plots = c(formula1, formula2), shared_ui = list(my_shared = function(ns) ...))`
- **When** the app starts
- **Then** the rendered HTML contains both per-plot module ids AND the shared widget id; clicking the draw-all button re-renders all plots.

---

## G1–G10 cross-cutting commitments

### G1.1 — `ptr_resolve_checkbox_defaults` NULL → all-TRUE

- **Given** `ptr_resolve_checkbox_defaults(NULL, expr_list)` where `expr_list` is `c("ggplot", "geom_point", "geom_smooth")`
- **When** the call returns
- **Then** the result is `c(geom_point = TRUE, geom_smooth = TRUE)`.

### G1.2 — Sparse override; missing keys default to TRUE

- **Given** `checkbox_defaults = list(geom_smooth = FALSE)` against `c("ggplot", "geom_point", "geom_smooth", "labs")`
- **When** resolution runs
- **Then** the result is `c(geom_point = TRUE, geom_smooth = FALSE, labs = TRUE)` in formula order.

### G1.3 — Order in result is formula order regardless of arg order

- **Given** `checkbox_defaults = list(c = FALSE, a = FALSE)` and layer order `c("a", "b", "c")`
- **When** resolution runs
- **Then** the result names are `c("a", "b", "c")` and values `c(FALSE, TRUE, FALSE)`.

### G1.4 — Vector value applies positionally within duplicate group

- **Given** layer list `c("geom_point", "geom_point-2", "geom_smooth")` and `list(geom_point = c(TRUE, FALSE))`
- **When** resolution runs
- **Then** the result is `c(geom_point = TRUE, "geom_point-2" = FALSE, geom_smooth = TRUE)`.

### G1.5 — Direct deduped key wins; group key fills the rest

- **Given** layers `c("geom_point", "geom_point-2", "geom_point-3")` and `list(`geom_point-2` = TRUE, geom_point = FALSE)`
- **When** resolution runs
- **Then** the result is `c(geom_point = FALSE, "geom_point-2" = TRUE, "geom_point-3" = TRUE)`.

### G1.6 — Long vector warns and truncates

- **Given** layer count 2 and `list(geom_point = c(TRUE, FALSE, TRUE))`
- **When** resolution runs
- **Then** a warning fires; the third value is discarded.

### G1.7 — Strict validation: non-list errors

- **Given** `checkbox_defaults = c(geom_point = TRUE)` (a vector, not a list)
- **When** resolution runs
- **Then** abort fires with "named list".

### G1.8 — Strict validation: NA/non-logical/length-0 errors

- **Given** `list(geom_point = NA)` or `list(geom_point = "off")` or `list(geom_point = logical(0))`
- **When** resolution runs
- **Then** abort fires.

### G1.9 — `ggplot` key treated as unknown (warns)

- **Given** `list(ggplot = FALSE)`
- **When** resolution runs
- **Then** a warning fires naming `ggplot`.

### G1.10 — `ptr_state$checkbox_defaults` exposed by P12

- **Given** a server state initialized with `checkbox_defaults = list(geom_smooth = FALSE)`
- **When** the state is built
- **Then** `state$checkbox_defaults` equals the resolved vector.

### G2.1 — Operator missing operand escalates

- **Given** `mpg > ptr_missing()` after substitution
- **When** P9 prunes
- **Then** the call escalates to `ptr_missing()` via the operator-escalation path (first branch of `prune_walk.ptr_call`; operators in `pruneable_operator_names` always escalate on a missing operand, independent of the relaxed positional-missing rule for non-operators).

### G2.2 — Inequality inside `subset` collapses upward

- **Given** `subset(mtcars, mpg > ptr_missing())` after substitution
- **When** P9 prunes
- **Then** `>` operator escalates → second positional arg of `subset` becomes `ptr_missing` → relaxed P9 drops the arg → `subset(mtcars)` survives.

### G2.3 — Unary operator missing operand escalates

- **Given** `!ptr_missing()`
- **When** P9 prunes
- **Then** escalates.

### G2.4 — Fully-substituted comparison unaffected

- **Given** `mpg > 5` after substitution
- **When** P9 prunes
- **Then** the call survives unchanged.

### G3.1 — User `safe_to_remove` extends remove set

- **Given** the formula `"ggplot(...) + pcp_theme(title = text)"`, `text` unset, `safe_to_remove = c("pcp_theme")`
- **When** the public render runs
- **Then** the `pcp_theme` layer is dropped.

### G3.2 — `safe_to_remove` of valid name accepted

- **Given** `safe_to_remove = c("custom_theme")`
- **When** validation runs
- **Then** no error.

### G3.3 — `safe_to_remove` of invalid token rejected

- **Given** `safe_to_remove = c("::")`, `c("")`, or `c(NA_character_)`
- **When** validation runs
- **Then** abort fires.

### G3.4 — `safe_to_remove = NULL` accepted

- **Given** `safe_to_remove = NULL`
- **When** validation runs
- **Then** the default `remove_set` applies.

### G4.1 — Default `is_standalone` recognizes `geom_*` and `stat_*`

- **Given** layer names `geom_point`, `stat_smooth`
- **When** the predicate runs
- **Then** TRUE for both.

### G4.2 — Default `is_standalone` rejects non-geom/stat

- **Given** layer names `theme`, `labs`, `pcp_theme`
- **When** the predicate runs
- **Then** FALSE for all.

### G4.3 — Custom `is_standalone` predicate honored

- **Given** a user-supplied predicate `function(name) startsWith(name, "my_layer_")`
- **When** P9 runs with this predicate
- **Then** layers named `my_layer_x` survive empty.

### G5.1 — Bare-symbol `expr` substituted to a `theme()` call survives prune

- **Given** the formula `"ggplot(mtcars) + expr"`, `expr` set to `"theme()"`, `theme` in `remove_set`
- **When** P8 + P9 run
- **Then** the substituted layer is `ptr_user_expr(theme())`; P9 returns it unchanged; render shows `+ theme()`.

### G5.2 — `expr` placeholder substituted to nested zero-arg call survives

- **Given** `expr` set to `"theme(plot.title = element_text())"`
- **When** P8 + P9 run
- **Then** the wrapper is preserved; nested `element_text()` is not pruned.

### G5.3 — Non-`expr` substitution does NOT get the wrapper

- **Given** a `text` placeholder substituted to `"hello"`
- **When** P8 runs
- **Then** the result is a character literal directly, NOT wrapped in `ptr_user_expr`.

### G5.4 — Layer-level bare-symbol `expr` provenance honored

- **Given** the formula `"ggplot(mtcars) + expr"` with `expr` substituting to `geom_point()`
- **When** P8 + P9 + P10 run
- **Then** the rendered code contains `geom_point()` even though `geom_point` could be in `remove_set`.

### G6.1 — Initial seed via relaxed P9

- **Given** app start with the formula `"mtcars |> head(num) |> filter(year > num) |> ggplot(...)"`, no input set
- **When** P12 initializes
- **Then** under relaxed P9: `head(num)` empty drops the arg → `head()` empty (head NOT in `default_drop_when_empty`) → survives. `year > num` operator escalates → `filter(ptr_missing)` → drop arg → `filter()` empty (filter NOT in `default_drop_when_empty`) → survives. Pipeline = `mtcars |> head() |> filter()`. Eval: `head(mtcars)` (default n = 6), then `filter(.)` (identity), so `state$resolved_data[[layer]]` is a 6-row frame with `mtcars` columns. (The original "both verbs dropped — mtcars itself" reading was the strict-P9 outcome and contradicted P12.1; the relaxed rule supersedes it.)

### G6.2 — Atomic snapshot success

- **Given** a multi-var pipeline; all selections valid
- **When** Update Data clicked
- **Then** snapshot updates; cache reflects new inputs; stale flag is FALSE.

### G6.3 — Atomic snapshot failure

- **Given** a multi-var pipeline; one selection is out-of-upstream
- **When** Update Data clicked
- **Then** snapshot UNCHANGED; cache UNCHANGED; inline error surfaces; stale flag stays TRUE.

### G6.4 — Per-position upstream for choices vs terminal upstream for plot

- **Given** `"mtcars |> select(var=mpg) |> ggplot(aes(x = var))"` after Update
- **When** P6 renders the inner `select(var)`'s picker AND the layer's resolved_data
- **Then** the picker shows full `mtcars` columns (per-position upstream); the resolved_data has 1 column (`mpg`, terminal upstream).

### G7.1 — `ptr_pipeline$op` preserved through translate, classify, substitute, prune

- **Given** a pipeline with `op = "%>%"`
- **When** the tree passes through P1 → P2 → P8 → P9
- **Then** the op field on each surviving pipeline node remains `"%>%"`.

### G7.2 — P10 reads op from the node, not from the source string

- **Given** a typed tree built from `"mtcars %>% ggplot(...)"`
- **When** P10 renders
- **Then** the op token comes from `node$op`, never from a re-parsed source.

### G7.3 — P11 folds via `rlang::call2(op, ...)`

- **Given** a `ptr_pipeline` with stages and op
- **When** P11 builds the evaluable expression
- **Then** the eval expression uses `rlang::call2(node$op, accumulator, next_stage)`; no string substitution.

### G8.1 — `paintr-llm.R` functions still callable post-rewrite

- **Given** a registry built from the new constructors
- **When** `ptr_llm_primer()`, `ptr_llm_topics()`, `ptr_llm_topic("var")`, `ptr_llm_register(...)` are called
- **Then** they return their documented outputs; the only change is that `data_aware` and `role` are surfaced via the new registry shape.

### G9.1 — Source-file reorganization compiles

- **Given** the new file layout (`paintr-translate.R`, `paintr-substitute.R`, `paintr-prune.R`, `paintr-render.R`, `paintr-eval.R`, `paintr-registry.R`, `paintr-builtins.R`, `paintr-classify.R`, plus untouched `paintr-app.R`, `paintr-copy.R`, `paintr-options.R`, `paintr-llm.R`)
- **When** `devtools::document()` and `devtools::check()` run
- **Then** 0 errors, 0 warnings; NAMESPACE regenerates correctly.

### G10.1 — Drop-when-empty rule (relaxed P9)

- **Given** a `ptr_call` after substitution
- **When** P9's `prune_walk.ptr_call` runs
- **Then** the rule has three branches, in order:
  1. **Operator escalation.** If `bare_call_name(node$fun) %in% pruneable_operator_names` AND any pruned arg is `ptr_missing` → return `ptr_missing()`. Operators always escalate; this is independent of the drop-when-empty list.
  2. **Drop missing args.** Otherwise, walk args; drop both named-missing and positional-missing args (relaxed P9).
  3. **Drop-when-empty.** If the call is now zero-arg AND `bare_call_name(node$fun)` is in `remove_set` (= `default_drop_when_empty()` ∪ user `safe_to_remove`) AND `is_standalone(name)` is FALSE → return `ptr_missing()` (the drop sentinel; reuses `ptr_missing` so parent walkers and pipeline-stage filtering compose without protocol changes). Otherwise the empty call survives and renders as `verb()`, relying on the verb's own defaults at eval (e.g. `head()` → 6 rows).

### G10.2 — `default_drop_when_empty` is rename of `default_safe_to_remove`

- **Given** the renamed helper in `R/paintr-utils.R`
- **When** `ptr_prune` builds `remove_set`
- **Then** the new name carries the unified semantic ("drop the call when it goes empty"). The old `default_safe_to_remove` symbol persists as a thin alias for legacy callers in `paintr-runtime.R`; the alias is removed at the 4c cutover.

### G10.3 — Empty calls not in the list survive at any nesting depth

- **Given** `head()` empty (after positional-missing drop) inside a pipeline OR as a regular argument
- **When** P9 prunes
- **Then** the call survives unchanged (`head` is not in `default_drop_when_empty`). Eval errors from partial-arg calls (e.g. `filter(year)` after a sibling drop) are NOT silently swallowed by prune — they surface via Phase 4a safe wrappers (`ptr_complete_expr_safe_v2`) as user-visible inline errors.

---

## End-to-end scenarios (multiple passes)

### E1. Upload + var cascade

- **Given** the formula `"upload(shared = \"ds\") |> filter(year > num) |> ggplot(aes(x = var, y = var))"`, no inputs set
- **When** the app initializes
- **Then** the upload widget renders; the `num` numeric input renders; the two `var` pickers render with empty choices (upstream unresolved); `state$resolved_data[[layer]]` is NULL.

### E1a. Upload completes; var dropdowns refresh

- **Given** the same formula; user uploads a CSV and types `"my_data"` into the upload-name input
- **When** the upload reactive completes
- **Then** the `var` pickers' choices refresh to the uploaded columns.

### E1b. User picks num and clicks Update

- **Given** uploaded data; `num = 1990`; both vars not yet picked; click Update
- **When** P12 handles the click
- **Then** snapshot updates: the resolved_data becomes `filter(my_data, year > 1990)`; vars' upstreams reflect this.

### E1c. User picks var and renders plot

- **Given** snapshot established; both `var` set to columns present in the post-filter frame
- **When** P8 → P9 → P11 runs
- **Then** the plot renders successfully; code panel shows the full pipe form `my_data |> filter(year > 1990) |> ggplot(aes(x = ..., y = ...))`.

### E2. Shared `var` overrides at runtime

- **Given** the formula `"ggplot(aes(x = var(shared = \"axis\"), y = var(shared = \"axis\")))"`, shared widget set to `"mpg"`
- **When** P8 runs with `shared_bindings = list(axis = reactiveVal("mpg"))`
- **Then** both substituted positions equal `quote(mpg)`; only one widget rendered.

### E3. Custom data-source placeholder (database)

- **Given** a registry built with `ptr_define_placeholder_source(keyword = "db_table", build_ui = ..., resolve_data = function(value, node) DBI::dbReadTable(conn, value))` and the formula `"db_table(shared = \"src\") |> ggplot(aes(x = var))"`
- **When** the app runs and the user picks a table
- **Then** the `var` picker's choices come from the chosen table's columns; the plot evaluates against the table.

### E4. Layer toggle interacts with prune

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point() + geom_smooth()"`, user unchecks `geom_smooth`
- **When** P8 + P9 + P11 run
- **Then** P9 drops the `geom_smooth` layer; P11 evaluates only `ggplot + geom_point`; code panel shows only the surviving layers.

### E5. `expr` placeholder injects an extra layer at runtime

- **Given** the formula `"ggplot(mtcars, aes(x = mpg)) + geom_point() + expr"`, user types `"theme_minimal()"` into the `expr` input
- **When** P8 + P9 + P10 + P11 run
- **Then** the substituted layer is `ptr_user_expr(theme_minimal())`; P9 keeps it; P10 renders `+ theme_minimal()`; P11 evaluates the full plot with the minimal theme applied.

### E6. Module isolation

- **Given** two `ptr_module_*` instances `m1` and `m2` with the same formula but different namespaces
- **When** the user changes an input in `m1`
- **Then** `m2`'s state, snapshot, and rendered plot are unaffected; ids do not collide.

### E7. Stale flag drives UI hint

- **Given** a stale snapshot
- **When** the user views the layer panel
- **Then** an inline "stale" notice surfaces; clicking Update clears it.

### E8. Code panel reproduces magrittr surface form

- **Given** the formula `"mtcars %>% head(num) %>% ggplot(...)"`, `num = 3`
- **When** P8 + P10 run (with snapshot)
- **Then** code text equals `"mtcars %>% head(3) %>% ggplot(...)"` exactly; no `|>`.

### E9. End-to-end with all five built-ins

- **Given** the formula `"upload(shared = \"ds\") |> head(num) |> ggplot(aes(x = var)) + geom_point(color = text) + expr"`, all inputs set: file uploaded, name typed, num = 5, var picked, text typed `"red"`, expr typed `"labs(title = 'demo')"`
- **When** P1 → P12 runs end-to-end
- **Then** plot renders successfully; code panel shows the full reconstructed pipeline + layers + extras; no errors surface.

---

## Out-of-scope cases (explicit)

These are NOT covered by this BDD manual; they live elsewhere or are deliberately excluded:

- Copy-rules deep-merge semantics — owned by `paintr-copy.R`, untouched by rewrite; existing test-copy-rules passes carry over.
- Upload reading internals (CSV BOM stripping, RDS coercion, Excel readxl detection, JSON nested-arrays errors, etc.) — owned by the upload built-in's `resolve_data`; existing test-upload passes carry over.
- `ptr_options` global option semantics — `paintr-options.R` untouched.
- `paintr-app-bslib.R` shell variant — untouched apart from any new id-contract plumbing.
- Pkgdown site building, vignette rendering — orthogonal to this rewrite.

These exclusions are deliberate: they were inventoried in the coverage matrix and confirmed to be either independent of AST shape (copy/options/llm) or owned by a registered built-in's internal implementation (upload helpers).
