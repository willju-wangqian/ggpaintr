# Bind the Generated Control Panel into a Shiny App

Register the dynamic `var` controls and render the standard tabbed
control panel into the target `uiOutput()`.

## Usage

``` r
ptr_setup_controls(input, output, ptr_state)
```

## Arguments

- input:

  A Shiny `input` object.

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
  ptr_setup_controls(input, output, ps)
  ptr_register_draw(input, ps)
  ptr_register_plot(output, ps)
}
} # }
```
