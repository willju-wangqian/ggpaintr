# ui component for deciding whether or not to toggle `coord_flip()`

ui component for deciding whether or not to toggle
[`coord_flip()`](https://ggplot2.tidyverse.org/reference/coord_flip.html)

## Usage

``` r
settingFlipUI(id, data_vars, ui_id = "miscFlip")
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
