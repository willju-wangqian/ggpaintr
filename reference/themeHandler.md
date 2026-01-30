# Module server for `theme()`

Module server for
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

## Usage

``` r
themeHandler(id, module_id, param)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- module_id:

  the id of the ui elements (not including its prefix created by the
  name space) which gives input as arguments of
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html)

- param:

  parameters of
  [`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) that
  correspond to the ui elements

## Value

[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) and its
code or `NULL`

## Note

the order of `param` should match the order of `module_id`. Currently
the following parameters of
[`theme()`](https://ggplot2.tidyverse.org/reference/theme.html) are
implemented

- `legend.position`

- `legend.direction`

- `legend.box`
