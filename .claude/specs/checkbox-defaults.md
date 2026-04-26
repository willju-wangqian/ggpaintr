---
name: checkbox-defaults
type: decision
status: accepted
scope: [api, ui, runtime]
created: 2026-04-26
---

# checkbox_defaults — initial layer-checkbox state

## Understanding summary

- **What.** Add `checkbox_defaults` argument to the four formula-taking exported functions (`ptr_app`, `ptr_app_bslib`, `ptr_server`, `ptr_server_state`). Controls the initial checked state of each layer's "include this layer" checkbox at app launch.
- **Why.** Multi-layer formulas can declare layers that conflict when all rendered together (e.g., two competing geoms). Today every layer starts checked; users must launch the app and click off the unwanted ones. New argument lets authors ship apps with a sensible starting subset.
- **Who.** App authors composing formulas with multiple geoms or alternative-layer recipes. Same audience already on `ptr_app*` / `ptr_server*`.
- **Constraints.** Backward compatible (`NULL` default → today's all-on). Follows existing conventions (`placeholders =`, `ui_text =` are named lists). Strict validation at the boundary.
- **Non-goals.** No runtime override (the checkbox itself is the runtime mechanism). No reactive `checkbox_defaults`. Does not affect placeholder controls inside a layer — only the layer-level checkbox.

## Decision

Approach **A — Normalize once at boundary; pass lookup through render pipeline.**

`ptr_server_state()` validates and normalizes user input via a new internal helper `ptr_resolve_checkbox_defaults()` and stores the result on `ptr_state$checkbox_defaults` as a length-N named logical vector (N = number of non-`ggplot` layers). `ptr_setup_controls()` threads it through `ptr_get_tab_ui()` → `ptr_build_ui_list()` → `ui_insert_checkbox()`, which reads it once per checkbox at render time. The three other exported functions (`ptr_app`, `ptr_app_bslib`, `ptr_server`) just forward to `ptr_server_state()`. Parameter name is `checkbox_defaults` end-to-end (public arg → state field → render-path arg).

## Decision log

| # | Decision | Alternatives considered | Reason |
|---|---|---|---|
| 1 | Named list, sparse override (missing keys → `TRUE`) | exhaustive list; unnamed positional vector; "off only" inverse list | Matches `placeholders =`/`ui_text =`; minimal typing for common case |
| 2 | Parameter name `checkbox_defaults` | `layer_defaults`, `layers_on`, `layer_initial`, `default_layers` | Explicit about what's being controlled (the checkboxes, not the layers themselves) |
| 3 | Unknown names → `cli::cli_warn()` + ignore | hard error; silent ignore | Catches typos; doesn't break apps when formula is later edited |
| 4 | Duplicate layers: vector value at the shared key, positional within group | use parser's deduped keys directly; custom suffix scheme; positional-only fallback | No new naming convention; both group key and direct deduped key (`geom_point-2`) are valid addresses |
| 5 | Short vector → pad with `TRUE`; long vector → warn + truncate | strict error; full recycling; scalar-only broadcast | Consistent with sparse-override philosophy; loud about extras, lenient about omissions |
| 6 | API surface: 4 formula-taking exported functions; stored on `ptr_state$checkbox_defaults`; read by internal `ui_insert_checkbox()` | also expose on `ptr_setup_controls`; narrower scope | Single source of truth (`ptr_server_state`); other 3 delegate |
| 7 | Strict logical validation: `is.logical(x) && !any(is.na(x))`; everything else errors | NA-as-sentinel; coercive | Boundary validation should be loud; sparse semantics already cover the "skip" case |

## Layer-name addressing

Layer names come from `names(parsed$expr_list)`. Duplicate layers use the project's `handle_duplicate_names()` rule (`R/paintr-utils.R:161`): first occurrence stays bare, second becomes `<name>-2`, third `<name>-3`, etc.

```
y ~ x + geom_point() + geom_point() + geom_smooth()
→ names(parsed$expr_list): "ggplot", "geom_point", "geom_point-2", "geom_smooth"
```

Two equivalent ways to address duplicates:

```r
# Group key — vector applies positionally over all instances
list(geom_point = c(TRUE, FALSE))

# Direct key — addresses the deduped name (backticks required for hyphen)
list(`geom_point-2` = FALSE)
```

When both appear, direct keys resolve first (consume their layer), then group keys fill remaining instances.

## Resolution algorithm — `ptr_resolve_checkbox_defaults(checkbox_defaults, expr_list)`

```
1. layer_names <- setdiff(names(expr_list), "ggplot")
2. result <- setNames(rep(TRUE, length(layer_names)), layer_names)
3. if (is.null(checkbox_defaults)) return(result)
4. validate_input_shape(checkbox_defaults)
   # named list, no empty/duplicate names
5. validate_value_types(checkbox_defaults)
   # each value: logical, length > 0, no NA
6. # Pass 1: direct hits
   for user_key in names(checkbox_defaults):
     if user_key %in% layer_names:
       result[user_key] <- checkbox_defaults[[user_key]][1]
       mark user_key consumed
       mark layer_names[user_key] consumed
   # Pass 2: group expansion
   for user_key not yet consumed:
     group <- layer_names whose strip("-\\d+$") == user_key,
              minus already-consumed slots, in formula order
     if length(group) == 0:
       collect for unknown-keys warning
       continue
     value_vec <- checkbox_defaults[[user_key]]
     if length(value_vec) > length(group):
       collect for too-long warning
       value_vec <- value_vec[seq_along(group)]
     if length(value_vec) < length(group):
       value_vec <- c(value_vec, rep(TRUE, length(group) - length(value_vec)))
     result[group] <- value_vec
7. emit collected warnings (one cli_warn per category, batched lists)
8. return result
```

## Error / warning text

**Errors (via `rlang::abort()`):**

- `"checkbox_defaults must be a named list, not a {class}."`
- `"checkbox_defaults must be fully named. Found unnamed entries at positions: {pos}."`
- `"checkbox_defaults has duplicate names: {dup_names}. Each layer key must appear once."`
- `"checkbox_defaults${key} must be logical, not {class(value)}."`
- `"checkbox_defaults${key} contains NA. Use TRUE / FALSE only."`
- `"checkbox_defaults${key} has length 0. Use TRUE / FALSE or omit the key."`

**Warnings (via `cli::cli_warn()`):**

- Unknown keys — one batched warn with `x` lines for unknown keys + `i` line listing valid layer names.
- Too-long vectors — one warn per offending key, with given length, expected length, and "extra values dropped" note.

## Edge cases

| Case | Behavior |
|---|---|
| `checkbox_defaults = NULL` | All-`TRUE`. No work, no message. |
| `checkbox_defaults = list()` | Same as `NULL`. |
| Formula has no non-`ggplot` layers | Resolved vector is empty (`logical(0)`); any user input warns "no layers to configure". |
| Both group key + direct key | Direct key resolved first; group key fills remaining instances. |
| Only `list(ggplot = FALSE)` | `ggplot` is not a checkbox layer → unknown-key warn. |
| Dynamic formula re-parse | `ptr_resolve_checkbox_defaults()` runs once at `ptr_server_state()` construction; reactive UI re-renders use the already-resolved vector. |
| `ns =` namespace prefix | Lookup keys remain layer names; `ns()` only prefixes the rendered input id, not the lookup key. |

## Files touched (estimate)

- `R/paintr-app.R` — 4 exported function signatures + `ptr_server_state()` body + `ptr_setup_controls()` thread-through + `ptr_validate_state()` field assertion
- `R/paintr-app-bslib.R` — 1 signature + 1 forwarding line
- `R/paintr-ui.R` — 3 internal signatures (`ptr_get_tab_ui`, `ptr_build_ui_list`, `ui_insert_checkbox`); `ui_insert_checkbox` body
- `R/paintr-checkbox-defaults.R` — **new file**; `ptr_resolve_checkbox_defaults()` helper
- `tests/testthat/test-checkbox-defaults.R` — **new file**
- `tests/manual/` — short script with a 3-layer mixed-defaults formula
- `vignettes/ggpaintr-workflow.Rmd` — one short paragraph + example

## Acceptance criteria

- `ptr_server_state(formula = y ~ x + geom_point() + geom_smooth(), checkbox_defaults = list(geom_smooth = FALSE))` returns a `ptr_state` with `state$checkbox_defaults` equal to `c(geom_point = TRUE, geom_smooth = FALSE)`.
- Launching `ptr_app(formula, checkbox_defaults = list(geom_smooth = FALSE))` shows `geom_smooth` unchecked and `geom_point` checked at startup; toggling either works as before.
- Unknown key `list(geom_typo = FALSE)` produces a single `cli::cli_warn()` and the app launches with all-`TRUE`.
- `list(geom_point = c(TRUE, FALSE))` against a formula with two `geom_point()` produces `c(geom_point = TRUE, "geom_point-2" = FALSE)`.
- `list(geom_point = c(TRUE, FALSE, TRUE))` against a formula with two `geom_point()` warns about the extra value, applies `c(TRUE, FALSE)`.
- `list(geom_smooth = "no")` errors with the type message.
- `list(geom_smooth = NA)` errors with the NA message.
- `checkbox_defaults = NULL` (default) produces zero diff in app behavior compared to today (regression-tested).
- `R CMD check --as-cran` remains 0 errors / 0 warnings / 1 NOTE (New submission).

## Risks

- **R1.** Roxygen drift across 4 functions. Mitigation: canonical `@param checkbox_defaults` on `ptr_server_state`; `@inheritParams ptr_server_state` on the wrappers.
- **R2.** Test surface ~20 blocks; mostly direct-on-helper, light Shiny touch. Manageable.
- **R3.** Hyphenated direct keys (`` `geom_point-2` ``) require backticks — common R footgun for users unfamiliar with non-syntactic names. Mitigated by recommending the group-key form in the roxygen example.

## Open follow-ups

- (none blocking) Consider after first user feedback: reactive `checkbox_defaults` for the L3 dynamic-formula path. Out of scope for this decision.
