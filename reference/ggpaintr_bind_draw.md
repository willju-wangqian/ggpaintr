# Bind Draw Behavior into a Shiny App

Bind Draw Behavior into a Shiny App

## Usage

``` r
ggpaintr_bind_draw(input, paintr_state, ids = paintr_state$ids)
```

## Arguments

- input:

  A Shiny `input` object.

- paintr_state:

  A `ggpaintr_state` object.

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

## Value

Invisibly returns `paintr_state`.
