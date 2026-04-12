# Build a ggpaintr Shiny App

Create a Shiny app from a single ggplot-like formula string. The app
exposes generated controls, a draw button, inline error handling, and
code output.

## Usage

``` r
ptr_app(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  placeholders = NULL,
  expr_check = TRUE
)
```

## Arguments

- formula:

  A single formula string using `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects when building the app.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

- expr_check:

  Controls `expr` placeholder validation. `TRUE` (default) applies the
  built-in denylist of dangerous functions. `FALSE` disables all
  checking. A named list with `deny_list` and/or `allow_list` character
  vectors supplies a custom check; when both are given, denied entries
  are removed from the allowlist.

## Value

A `shiny.appobj`.

## Examples

``` r
app <- ptr_app("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
inherits(app, "shiny.appobj")
#> [1] TRUE
```
