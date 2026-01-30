# Connect parameters to their input obtained from the ui elements by their ids

Connect parameters to their input obtained from the ui elements by their
ids

## Usage

``` r
connect_param_id(
  session_input,
  id_list,
  params,
  color_fill = FALSE,
  color_group = FALSE
)
```

## Arguments

- session_input:

  input of a shiny session

- id_list:

  list of ids

- params:

  list of parameters

- color_fill:

  bool; optional. Whether or not to use the same variable for both color
  and fill

- color_group:

  bool; optional. Whether or not to use the same variable for both color
  and group

## Value

a named list which has parameters as names and ids as list elements
