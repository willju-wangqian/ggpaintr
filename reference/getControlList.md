# Get all the implemented keys and their corresponding ui functions or handler functions

Get all the implemented keys and their corresponding ui functions or
handler functions

## Usage

``` r
getControlList(scope = "mapping", type = "ui", show_all = FALSE)
```

## Arguments

- scope:

  optional. `scope` can be one of `mapping`, `geom_args`, or
  `plot_settings`

- type:

  optional. By default it's `ui`. Can be `ui` or `handler`

- show_all:

  optional. bool. `show_all = TRUE` prints all keys

## Value

a named list where names are ggpaintr keys and list elements are the
corresponding ui functions or handler functions
