# Module server for `coord_flip()`

Module server for
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)

## Usage

``` r
flipHandler(id, module_id)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- module_id:

  the id of the ui element (not including its prefix created by the name
  space) which determines whether or not to call
  [`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)

## Value

[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)
and its code or `NULL`
