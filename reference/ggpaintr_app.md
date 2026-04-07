# Build a ggpaintr Shiny App

Create a Shiny app from a single ggplot-like formula string. The app
exposes generated controls, a draw button, inline error handling, code
output, and an export button for producing a standalone app script.

## Usage

``` r
ggpaintr_app(formula, envir = parent.frame(), copy_rules = NULL)
```

## Arguments

- formula:

  A single formula string using `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects when building the app.

- copy_rules:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

## Value

A `shiny.appobj`.

## Examples

``` r
app <- ggpaintr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
inherits(app, "shiny.appobj")
#> [1] TRUE
```
