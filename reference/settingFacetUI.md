# ui component for selecting variables for `facet_grid()`

ui component for selecting variables for
[`facet_grid()`](https://ggplot2.tidyverse.org/reference/facet_grid.html)

## Usage

``` r
settingFacetUI(id, data_vars, ui_id = "miscFacet")
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- data_vars:

  variable names of the dataset

- ui_id:

  id of the ui element

## Value

a list with two elements

- ui: the ui element of this piece

- id: the id of this ui element
