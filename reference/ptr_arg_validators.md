# Argument validators for placeholder definitions (`ptr_arg_*`)

These helpers are factories that return a closure of shape
`function(arg_expr) -> canonical_value | abort()`. The closure validates
the unevaluated R expression captured as a placeholder's positional
default argument and returns a canonical value, or aborts with a clear
message.

## Usage

``` r
ptr_arg_symbol_or_string(vector = FALSE)

ptr_arg_string(vector = FALSE)

ptr_arg_symbol(vector = FALSE)

ptr_arg_numeric(vector = FALSE, length = NULL)

ptr_arg_expression()
```

## Arguments

- vector:

  Logical scalar (default `FALSE`). When `TRUE`, the validator parses a
  `c(...)` literal element-by-element and returns the whole vector
  instead of a single scalar.

- length:

  Optional integer length required of the resulting numeric vector;
  honored only when `vector = TRUE`. `NULL` (the default) imposes no
  length check.

## Value

A closure that takes an unevaluated expression and returns the canonical
default value, or aborts.

## Details

The validators operate on AST only: they do not call
[`eval()`](https://rdrr.io/r/base/eval.html),
[`parse()`](https://rdrr.io/r/base/parse.html), or any
deparse-and-reparse cycle on their input. The numeric helper
`ptr_arg_numeric()` (scalar by default, vector via `vector = TRUE`)
walks the AST against the constant-fold allowlist registry (see
[`ptr_register_constant_fold()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_constant_fold_registry.md))
and then evaluate in a sealed environment whose only bindings are the
registered names.

Symbol policy is per-helper:

- `ptr_arg_symbol_or_string()` accepts a bareword symbol (returned as
  its character name, preserving non-syntactic / backticked names) or
  any single string literal (including the empty string).

- `ptr_arg_symbol()` accepts only a bareword symbol (returned as its
  character name); rejects string literals, numbers, and compound calls.

- `ptr_arg_string()` accepts only a single string literal (including the
  empty string); rejects symbols and numbers.

- `ptr_arg_numeric()` accepts any AST whose every node is a syntactic
  literal or a registered constant-fold name; in the default scalar mode
  the result must be a length-one non-NA numeric. For the vector form
  use `ptr_arg_numeric(vector = TRUE, length = NULL)`.

- `ptr_arg_expression()` is a verbatim store: it returns its input
  unchanged so it can later be evaluated in the data context. As a
  convenience it emits a one-shot warning if the user wraps the
  expression in [`quote()`](https://rdrr.io/r/base/substitute.html),
  [`bquote()`](https://rdrr.io/r/base/bquote.html), `rlang::ppExpr()`,
  or
  [`rlang::quo()`](https://rlang.r-lib.org/reference/defusing-advanced.html)
  (the wrapper is stored verbatim).

Each of `ptr_arg_symbol_or_string()`, `ptr_arg_symbol()`,
`ptr_arg_string()`, and `ptr_arg_numeric()` takes a `vector` flag. With
`vector = FALSE` (the default) the validator parses a single scalar
element and returns a length-one value. With `vector = TRUE` it parses a
`c(...)` literal element-by-element (each element subject to the
helper's scalar element rule) and returns the whole vector; a lone
element is treated as a length-one vector. For
`ptr_arg_numeric(vector = TRUE)` the optional `length` check (honored
only in vector mode) asserts the parsed vector's length.

## Examples

``` r
is_symbol_ok <- ptr_arg_symbol_or_string()
is_symbol_ok(quote(mpg))
#> [1] "mpg"
is_symbol_ok("mpg")
#> [1] "mpg"

is_num <- ptr_arg_numeric()
is_num(5)
#> [1] 5
is_num(quote(2 * pi))
#> [1] 6.283185

is_vec <- ptr_arg_numeric(vector = TRUE, length = 2L)
is_vec(quote(c(0, 1)))
#> [1] 0 1
```
