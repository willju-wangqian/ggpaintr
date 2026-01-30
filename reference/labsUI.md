# ui component for `labs()`

ui component for
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html)

## Usage

``` r
labsUI(id, labs_selected = TRUE)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- labs_selected:

  ector of selected parameters of
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html)

## Value

a list with two elements

- ui: the ui element of this piece

- id: the id of this ui element
