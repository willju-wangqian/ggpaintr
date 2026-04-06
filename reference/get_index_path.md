# Locate Placeholder Paths Inside an Expression

Locate Placeholder Paths Inside an Expression

## Usage

``` r
get_index_path(
  x,
  target = c("var", "text", "num", "expr", "upload"),
  current_path = numeric(),
  result = list()
)
```

## Arguments

- x:

  An expression to inspect.

- target:

  Placeholder symbols to detect.

- current_path:

  Internal recursion path.

- result:

  Internal accumulator.

## Value

A list of index paths.
