# Signal a transient "partial input" failure from a placeholder hook

Placeholder `resolve_expr` (and `validate_input`) hooks should call
`ptr_signal_partial()` – instead of
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) /
[`stop()`](https://rdrr.io/r/base/stop.html) – when the current widget
value is incomplete or transiently unparseable. Typical example: a
free-text `expr` placeholder whose body has not finished being typed
(`mpg /`). The condition carries class `"ptr_partial_input"`.

## Usage

``` r
ptr_signal_partial(message, ...)
```

## Arguments

- message:

  Diagnostic text. Surfaced on the gated plot path.

- ...:

  Additional named fields attached to the condition (forwarded to
  [`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html)).

## Value

Never returns – always signals.

## Details

ggpaintr's *live* reactive boundaries (column-picker `entry_reactive`s
that re-fire on every keystroke) catch this class and silently cancel
the current re-render, so the downstream picker keeps its previous state
instead of strobing blank and writing a Shiny warning to the R console.
The *Update / Draw*-gated plot path does not catch it, so a value that
is still partial when the user clicks "Update" still surfaces as a
normal inline error.

Use ordinary
[`rlang::abort()`](https://rlang.r-lib.org/reference/abort.html) for
failures that are NOT user mid-typing artifacts (security violations,
real argument-shape errors, etc.).

## Examples

``` r
# A resolve hook that treats an unfinished expression as a transient,
# silently-cancelled partial input rather than a hard error:
my_expr_resolve <- function(value, node, ...) {
  tryCatch(
    rlang::parse_expr(value),
    error = function(e) ptr_signal_partial(conditionMessage(e))
  )
}
```
