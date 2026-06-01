# Constant-fold allowlist registry

The numeric default-argument validator
[`ptr_arg_numeric()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_arg_validators.md)
(scalar by default, vector via `vector = TRUE`) walks the placeholder's
default-argument AST against an allowlist of function and constant
names. Authors can extend the allowlist with
`ptr_register_constant_fold()` when their placeholder definitions need
additional pure operators.

## Usage

``` r
ptr_register_constant_fold(name, value)

ptr_clear_constant_fold(name = NULL)

ptr_constant_fold_keywords()
```

## Arguments

- name:

  Character scalar function or constant name.

- value:

  Function or numeric constant to bind under `name`.

## Value

`ptr_register_constant_fold()` and `ptr_clear_constant_fold()` return
`invisible(NULL)`. `ptr_constant_fold_keywords()` returns a character
vector of currently registered names.

## Details

Built-in entries seeded at package load:

- Arithmetic: `-`, `+`, `*`, `/`, `^`, `%%`, `%/%`

- Sequence constructors: `:`, `c`, `seq`, `seq.int`, `seq_len`,
  `seq_along`

Syntactic literals (`TRUE`, `FALSE`, `NA`, `NA_integer_`, `NA_real_`,
`Inf`, `NaN`) are recognised by the walker directly and never need
registration. `pi` resolves through the registry's parent
([`baseenv()`](https://rdrr.io/r/base/environment.html)).

## Examples

``` r
ptr_register_constant_fold("log10", log10)
ptr_arg_numeric()(quote(log10(100)))
#> [1] 2
ptr_clear_constant_fold("log10")
```
