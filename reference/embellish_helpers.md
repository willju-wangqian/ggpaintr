# Built-in helpers for a placeholder's plain-R evaluation slot

A placeholder-embellished ggplot expression must stay valid plain R that
renders the original plot with no app running. Each placeholder carries
a callable (the `embellish_eval =` argument of the
`ptr_define_placeholder_*()` constructors) that supplies its plain-R
meaning when the naked expression is evaluated outside
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).
These two factories are the built-in callables for that slot:

## Usage

``` r
embellish_identity()

embellish_symbol_to_string()
```

## Value

Each factory returns a function of signature `function(x, ...)`.
`embellish_identity()`'s function returns its first argument `x`
unchanged. `embellish_symbol_to_string()`'s function returns a character
vector of column names captured from the unevaluated `x`.

## Details

- `embellish_identity()` returns the identity `function(x, ...) x` — the
  slot's default behaviour. The placeholder call becomes a no-op
  wrapper: it returns its argument unchanged.

- `embellish_symbol_to_string()` returns a function that captures its
  argument *unevaluated* and turns column references into a character
  vector of names. This is the pattern a column-selecting consumer needs
  so the naked expression works inside a tidyselect verb: tidyselect
  evaluates an unknown wrapper call in non-masked scope, where bare
  column symbols throw "object not found"; returning the names as
  strings sidesteps that because tidyselect accepts selection by name.

These helpers are *author-controlled* plain-R semantics, never derived —
only the author knows the intended live-R meaning of a placeholder.

## Examples

``` r
f <- embellish_identity()
f(5L)
#> [1] 5

g <- embellish_symbol_to_string()
g(c(mpg, hp))
#> [1] "mpg" "hp" 
g(mpg)
#> [1] "mpg"
```
