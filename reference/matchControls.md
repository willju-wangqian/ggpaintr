# match key to its ui or handler function

match key to its ui or handler function

## Usage

``` r
matchControls(selected, scope = "mapping", type = "ui")
```

## Arguments

- selected:

  the key to be matched

- scope:

  optional. `scope` can be one of `mapping`, `geom_args`, or
  `plot_settings`

- type:

  optional. By default it's `ui`. Can be `ui` or `handler`

## Value

the ui or handler function of `selected` key
