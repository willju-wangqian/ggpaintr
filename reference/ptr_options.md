# Get or Set ggpaintr Package Options

Combined getter/setter for ggpaintr's global settings, modeled after
base [`base::options()`](https://rdrr.io/r/base/options.html). Calling
with no arguments returns all current settings as a named list. Passing
one or more named logical arguments sets those settings and invisibly
returns the previous values, suitable for the
`do.call(ptr_options, old)` round-trip pattern used by
[`withr::with_options()`](https://withr.r-lib.org/reference/with_options.html)
and [`on.exit()`](https://rdrr.io/r/base/on.exit.html).

## Usage

``` r
ptr_options(...)
```

## Arguments

- ...:

  Named logical arguments — one per setting to update. Setting names
  must match the registry above.

## Value

When called with no arguments, a named list of all current setting
values. When called with named arguments, the previous values of the
updated settings, returned invisibly.

## Available settings

- `verbose`:

  Logical. When `TRUE`, ggpaintr emits informative messages such as
  "Layer foo() removed (no arguments provided)." Default `FALSE` — these
  messages are intended for debugging the formula pipeline and are off
  by default. Underlying option: `options(ggpaintr.verbose = ...)`.

- `checkbox_default_all_other_layer`:

  Logical. The fallback initial state for layer checkboxes that aren't
  explicitly named in a call's `checkbox_defaults` argument. `TRUE`
  (default) starts unspecified layers checked (current behavior);
  `FALSE` starts unspecified layers unchecked, so apps with many
  alternative layers can opt-in only the ones they want via
  `checkbox_defaults =`. The per-call argument always wins over this
  global fallback. Underlying option:
  `options(ggpaintr.checkbox_default_all_other_layer = ...)`.

## Examples

``` r
# Inspect current values
ptr_options()
#> $verbose
#> [1] FALSE
#> 
#> $checkbox_default_all_other_layer
#> [1] TRUE
#> 

# Silence the "Layer ... removed" notice for one block
old <- ptr_options(verbose = FALSE)
on.exit(do.call(ptr_options, old), add = TRUE)

# Start every app with all layers unchecked unless the call opts them in
ptr_options(checkbox_default_all_other_layer = FALSE)
```
