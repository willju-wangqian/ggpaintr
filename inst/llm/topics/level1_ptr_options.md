# Level 1 — package-global settings with `ptr_options()`

Use when: the user wants to flip a session-wide ggpaintr default — quiet/verbose pipeline output, or invert the layer-checkbox initial state — without threading an argument through every `ptr_app()` call.

`ptr_options()` is a getter/setter, modeled after base `options()`. No Shiny code is involved; the call sets `getOption()` keys that ggpaintr reads at runtime.

## Signature

```r
ptr_options(...)
```

- No arguments → returns a named list of all current ggpaintr settings.
- One or more named logical arguments → sets those settings and **invisibly** returns the previous values, suitable for round-trip via `do.call(ptr_options, old)`.
- Unknown names abort. Non-logical / non-scalar / `NA` values abort.

## Available settings

| Name                                | Default | Underlying option                              | Meaning |
|-------------------------------------|---------|------------------------------------------------|---------|
| `verbose`                           | `FALSE` | `ggpaintr.verbose`                             | When `TRUE`, ggpaintr emits informational notices like "Layer foo() removed (no arguments provided)." Intended for debugging the formula pipeline. |
| `checkbox_default_all_other_layer`  | `TRUE`  | `ggpaintr.checkbox_default_all_other_layer`    | Fallback initial state for layer checkboxes not named in `checkbox_defaults =`. Flip to `FALSE` so every layer starts unchecked unless explicitly opted in. |

## Read current settings

```r
library(ggpaintr)

ptr_options()
#> $verbose
#> [1] FALSE
#>
#> $checkbox_default_all_other_layer
#> [1] TRUE
```

## Set settings, with round-trip

```r
old <- ptr_options(verbose = TRUE,
                   checkbox_default_all_other_layer = FALSE)
on.exit(do.call(ptr_options, old), add = TRUE)
# ... ptr_app() / ptr_app_bslib() calls inside this scope inherit the new defaults ...
```

## Equivalent base-options usage

`ptr_options()` is the public face; the underlying `getOption()` keys are stable and may be set directly (e.g., from `.Rprofile`):

```r
options(
  ggpaintr.verbose                          = TRUE,
  ggpaintr.checkbox_default_all_other_layer = FALSE
)
```

Prefer `ptr_options()` in scripts and tests so validation runs and unknown names error early.

## When to move up a level

- Need to scope a setting to a single app, not a whole session → pass `checkbox_defaults =` directly to `ptr_app()`. `ptr_options()` is for session-wide defaults; per-call arguments override it.
