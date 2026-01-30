# Module server that provides several `ggplot2` themes

Module server that provides several `ggplot2` themes

## Usage

``` r
themeChooseHandler(id, module_id)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- module_id:

  the id of the ui elements (not including its prefix created by the
  name space)

## Value

an implemented `ggplot2` theme and its code or `NULL`

## Note

currently the following themes of `ggplot2` are implemented:

- [`theme_gray()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

- [`theme_classic()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

- [`theme_bw()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)

- [`theme_minimal()`](https://ggplot2.tidyverse.org/reference/ggtheme.html)
