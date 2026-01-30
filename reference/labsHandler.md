# Module server for `labs()`

Module server for
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html)

## Usage

``` r
labsHandler(id, module_id, param)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- module_id:

  the id of the ui elements (not including its prefix created by the
  name space) which gives input as arguments of
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html)

- param:

  parameters of
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) that
  correspond to the ui elements

## Value

[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) and its
code or `NULL`

## Note

the order of `param` should match the order of `module_id`. Currently
the following parameters of
[`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) are
implemented

- `x`

- `y`

- `title`

- `subtitle`
