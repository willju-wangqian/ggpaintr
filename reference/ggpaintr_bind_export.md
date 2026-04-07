# Bind Export Behavior into a Shiny App

Bind Export Behavior into a Shiny App

## Usage

``` r
ggpaintr_bind_export(output, paintr_state, ids = paintr_state$ids)
```

## Arguments

- output:

  A Shiny `output` object.

- paintr_state:

  A `ggpaintr_state` object.

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

## Value

Invisibly returns `paintr_state`.
