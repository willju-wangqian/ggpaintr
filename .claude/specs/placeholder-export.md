---
name: placeholder-export
type: decision
status: accepted
scope: [placeholders, export, api]
created: 2026-04-09
---

## Understanding Summary

- **What:** Lift the inline-only restriction on custom placeholder hook functions by adding `source_file`, `source_package`, `source_function`, and `on_missing` parameters to `ptr_define_placeholder()`
- **Why:** Users (both analysis-script authors and package authors) need to define hook functions outside the `ptr_define_placeholder()` call and still produce exportable standalone Shiny apps via `ptr_generate_shiny()`
- **Who:** Both analysis-script authors (reusing top-level helpers) and R package authors (shipping functions in a package namespace)
- **Key constraints:** No breaking changes; existing inline `function(...)` hooks must continue to work; exported app must remain self-contained or clearly warn when external dependencies are missing
- **Non-goals:** Auto-detecting function provenance; per-hook granularity as the primary API

---

## Decision

Add four new parameters to `ptr_define_placeholder()`:

```r
ptr_define_placeholder(
  keyword,
  build_ui,
  resolve_expr,
  resolve_input      = NULL,
  bind_ui            = NULL,
  prepare_eval_env   = NULL,
  copy_defaults      = list(label = "Enter a value for {param}"),
  source_file        = NULL,   # path(s); scalar = default for all hooks
  source_package     = NULL,   # package name(s); scalar = default for all hooks
  source_function    = NULL,   # named list: hook_name -> function object to deparse
  on_missing         = "warn"  # "warn" | "error"
)
```

Per-hook override uses `.default` key in named lists:

```r
# All hooks from one file, except resolve_expr from another
source_file = list(
  .default     = "helpers.R",
  resolve_expr = "resolve-helpers.R"
)

# source_function is always a named list (hook-specific by nature)
source_function = list(
  build_ui     = my_build_ui,
  resolve_expr = my_resolve
)
```

**Precedence (highest to lowest):** inline `function(...)` literal → `source_function` → `source_file` → `source_package`

---

## Export-Time Preamble Generation

`ptr_serialize_export_placeholders()` gains `ptr_serialize_placeholder_preamble()`. Preamble is emitted after `library()` calls, before placeholder definitions. Order within preamble: `source_function` blocks first, then `source_file`, then `source_package`.

**`source_function`** → named assignment from deparsed formals + body:
```r
my_build_ui <- function(id, copy, meta, context) {
  shiny::dateInput(id, copy$label)
}
```

**`source_file`** with `on_missing = "warn"`:
```r
tryCatch(
  source("helpers.R"),
  error = function(e) {
    warning("ggpaintr: could not source 'helpers.R' — custom placeholder hooks unavailable. ",
            "Provide this file alongside app.R.")
  }
)
```
With `on_missing = "error"`: plain `source("helpers.R")` (no tryCatch).

**`source_package`** with `on_missing = "warn"`:
```r
if (!requireNamespace("mypkg", quietly = TRUE)) {
  tryCatch(
    install.packages("mypkg"),
    error = function(e) warning("ggpaintr: could not install 'mypkg' — custom placeholder hooks unavailable.")
  )
}
library(mypkg)
```
With `on_missing = "error"`: omit the `tryCatch` around `install.packages`.

---

## Validation Logic

`ptr_validate_exportable_placeholder()` updated:

```
for each hook in (build_ui, resolve_expr, resolve_input, bind_ui, prepare_eval_env):
  if hook is NULL                 → skip
  if hook is a function literal   → pass (existing behavior)
  if hook is a symbol:
    resolve source strategy (per-hook override > placeholder default)
    if any source strategy covers this hook → pass
    else → abort("must define {hook} inline or supply source_file/source_package/source_function")
  else → abort
```

Additional validation at definition time:
- `source_function` values must be function objects (`is.function()`)
- `source_file` and `source_package` values must be character strings
- `on_missing` must be `"warn"` or `"error"`
- Hook name typos in `source_function` → `rlang::warn()`
- `ptr_check_free_variables()` extended to run on `source_function` deparsed bodies

---

## Testing Strategy

Tests in `tests/testthat/test-export.R` (or `test-export-source.R` if file grows too large). Test approach: inspect text output of `ptr_generate_shiny()` via `withr::local_tempfile()`.

Key test groups:
- `source_function`: deparse correctness, per-hook override, free-variable abort
- `source_file`: tryCatch emission for `"warn"`, plain source for `"error"`, per-hook paths
- `source_package`: requireNamespace + install + library block, `on_missing` variants
- Validation: non-inline with no strategy aborts; non-function value aborts; `on_missing` invalid value aborts; hook name typo warns
- Precedence: `source_function` beats `source_file`; inline beats all

---

## Alternatives Considered

| Alternative | Rejected Because |
|-------------|-----------------|
| Auto-detect from `environment()` / `srcref` | Fragile; silently wrong in edge cases |
| Separate `ptr_export_config()` at `ptr_generate_shiny()` | Splits definition from export strategy; larger new API surface |
| `source_function` only (no file/package support) | Doesn't solve the package-author use case |
| Per-hook params only (no per-placeholder default) | Verbose for common case where all hooks share a strategy |

---

## Acceptance Criteria

- [ ] Inline `function(...)` hooks continue to work without any new params
- [ ] `source_function` produces valid, runnable deparsed function definitions in exported app
- [ ] `source_file` emits correct tryCatch/plain source based on `on_missing`
- [ ] `source_package` emits correct requireNamespace + install + library based on `on_missing`
- [ ] Per-hook `.default` override resolves correctly with precedence rules
- [ ] Validation aborts for non-inline hooks with no source strategy
- [ ] Free-variable check runs on `source_function` bodies
- [ ] All new test cases pass; existing export tests unaffected
- [ ] `devtools::check()` clean (0 errors, 0 warnings)
