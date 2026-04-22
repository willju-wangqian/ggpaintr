# Bind Default Error Rendering into a Shiny App

Bind Default Error Rendering into a Shiny App

## Usage

``` r
ptr_register_error(output, ptr_state)
```

## Arguments

- output:

  A Shiny `output` object.

- ptr_state:

  A `ptr_state` object.

## Value

Invisibly returns `ptr_state`.

## Examples

``` r
if (FALSE) { # \dontrun{
server <- function(input, output, session) {
  ps <- ptr_server_state("ggplot(mtcars, aes(x = var)) + geom_histogram()")
  ptr_register_draw(input, ps)
  ptr_register_error(output, ps)
}
} # }
```
