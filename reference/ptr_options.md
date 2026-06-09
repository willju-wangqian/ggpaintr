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

- `gate_draw`:

  Logical. When `TRUE` (the default), the rendered plot updates only
  when the user clicks the "Update plot" button — every placeholder
  change is batched until the click. When `FALSE`, the button is omitted
  from the UI and the plot re-renders reactively on every placeholder
  change (live mode). Read once when the app is built, so set it before
  calling
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  /
  [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md).
  Underlying option: `options(ggpaintr.gate_draw = ...)`.

- `suppress_warnings`:

  Logical. When `TRUE`, R warnings emitted while the plot is drawn (e.g.
  `loess` fit warnings such as "all data on boundary of neighborhood" or
  "Failed to fit group N") are silenced rather than printed to the
  console. Default `FALSE` — warnings surface as usual. Only the
  plot-drawing step is wrapped; errors still propagate to the inline
  error pane. Read once when the app is built, so set it before calling
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  /
  [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md).
  Underlying option: `options(ggpaintr.suppress_warnings = ...)`.

## Examples

``` r
# Inspect current values
ptr_options()
#> $verbose
#> [1] FALSE
#> 
#> $gate_draw
#> [1] TRUE
#> 
#> $suppress_warnings
#> [1] FALSE
#> 

# Silence the "Layer ... removed" notice for one block
old <- ptr_options(verbose = FALSE)
on.exit(do.call(ptr_options, old), add = TRUE)
```
