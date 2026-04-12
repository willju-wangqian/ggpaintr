# Bind Draw Behavior into a Shiny App

Bind Draw Behavior into a Shiny App

## Usage

``` r
ptr_register_draw(input, ptr_state, ids = ptr_state$ids)
```

## Arguments

- input:

  A Shiny `input` object.

- ptr_state:

  A `ptr_state` object.

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

## Value

Invisibly returns `ptr_state`.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
  ptr_setup_controls(input, output, ps)
  ptr_register_draw(input, ps)
  ptr_register_plot(output, ps)
}
} # }
```
