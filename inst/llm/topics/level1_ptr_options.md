# Level 1 — package-global settings with `ptr_options()`

Use when: the user wants to flip a session-wide ggpaintr default — quiet/verbose pipeline output — without threading an argument through every `ptr_app()` call.

`ptr_options()` is a getter/setter, modeled after base `options()`. No Shiny code is involved; the call sets `getOption()` keys that ggpaintr reads at runtime.

## Signature

```r
ptr_options(...)
```

- No arguments → returns a named list of all current ggpaintr settings.
- One or more named logical arguments → sets those settings and **invisibly** returns the previous values, suitable for round-trip via `do.call(ptr_options, old)`.
- Unknown names abort. Non-logical / non-scalar / `NA` values abort.

## Available settings

| Name      | Default | Underlying option   | Meaning |
|-----------|---------|---------------------|---------|
| `verbose` | `FALSE` | `ggpaintr.verbose`  | When `TRUE`, ggpaintr emits informational notices like "Layer foo() removed (no arguments provided)." Intended for debugging the formula pipeline. |

## Read current settings

```r
library(ggpaintr)

ptr_options()
#> $verbose
#> [1] FALSE
```

## Set settings, with round-trip

```r
old <- ptr_options(verbose = TRUE)
on.exit(do.call(ptr_options, old), add = TRUE)
# ... ptr_app() / ptr_app_bslib() calls inside this scope inherit the new default ...
```

## Equivalent base-options usage

`ptr_options()` is the public face; the underlying `getOption()` key is stable and may be set directly (e.g., from `.Rprofile`):

```r
options(ggpaintr.verbose = TRUE)
```

Prefer `ptr_options()` in scripts and tests so validation runs and unknown names error early.

## Per-widget defaults are not a global setting

To change a *specific* widget's initial value — a default column pick, a layer checkbox's initial checked state, a starting numeric — use the `spec =` boot-override on `ptr_app()` / `ptr_server()` (a named list of input id → value; ADR 0012), not a global option. Discover the input ids with `ptr_id_table(formula)`. `ptr_options()` is for session-wide behaviour only.
