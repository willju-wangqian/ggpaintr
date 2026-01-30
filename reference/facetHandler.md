# Module server for `facet_grid()`

Module server for
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)

## Usage

``` r
facetHandler(id, module_id)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- module_id:

  the id of the ui element (not including its prefix created by the name
  space) which gives input to
  [`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)

## Value

[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)
and its code or `NULL`
