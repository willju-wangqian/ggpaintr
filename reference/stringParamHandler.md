# Module server for a `ggplot2` function which takes strings as arguments

Module server for a `ggplot2` function which takes strings as arguments

## Usage

``` r
stringParamHandler(id, module_id, param, FUN)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- module_id:

  the id of the ui elements (not including its prefix created by the
  name space) which gives input as arguments of `FUN()`

- param:

  parameters of
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) that
  correspond to the ui elements

- FUN:

  a `ggplot2` function which takes strings as arguments

## Value

the return of `FUN` and its code or `NULL`
