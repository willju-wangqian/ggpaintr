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

## Examples

``` r
# Inspect current values
ptr_options()
#> $verbose
#> [1] FALSE
#> 

# Silence the "Layer ... removed" notice for one block
old <- ptr_options(verbose = FALSE)
on.exit(do.call(ptr_options, old), add = TRUE)
```
