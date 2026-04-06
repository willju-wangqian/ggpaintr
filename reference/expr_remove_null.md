# Remove Placeholder Marker Symbols

Remove Placeholder Marker Symbols

## Usage

``` r
expr_remove_null(
  .expr,
  target = rlang::sym("_NULL_PLACEHOLDER"),
  current_path = numeric()
)
```

## Arguments

- .expr:

  An expression object.

- target:

  The placeholder marker symbol.

- current_path:

  Internal recursion path.

## Value

The cleaned expression.
