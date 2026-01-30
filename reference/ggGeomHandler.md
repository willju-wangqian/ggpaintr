# Generate the geom component of a `ggplot2` plot

Generate the geom component of a `ggplot2` plot

## Usage

``` r
ggGeomHandler(
  id,
  data,
  geom_FUN,
  id_list,
  params_list,
  color_fill = FALSE,
  color_group = FALSE,
  userFUN = NULL,
  ...
)
```

## Arguments

- id:

  An ID string that corresponds with the ID used to call the module's UI
  function.

- data:

  data

- geom_FUN:

  `geom_<chart>`

- id_list:

  list of id of all ui elements

- params_list:

  list of parameters that the ui elements should correspond to

- color_fill:

  bool; optional. Whether or not to use the same variable for both color
  and fill

- color_group:

  bool; optional. Whether or not to use the same variable for both color
  and group

- userFUN:

  a function that returns a named list, where the names of this named
  list are parameters (except for `mapping`) of `geom_<chart>`, and the
  elements of this list are arguments of the corresponding parameters

- ...:

  arguments that go into `userFUN`

## Value

list of plot and code of a geom component of a `ggplot2` plot
