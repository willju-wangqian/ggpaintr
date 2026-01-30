# ui component for `theme()`

ui component for
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Usage

``` r
themeUI(id, theme_selected = TRUE)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- theme_selected:

  vector of selected parameters of
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Value

a list with two elements

- ui: the ui element of this piece

- id: the id of this ui element
