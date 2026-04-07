# Build Reactive Server State for ggpaintr

Create the shared reactive state used by the extensible `ggpaintr_*`
server helpers. This object can be passed to the bind helpers or
inspected directly inside a larger Shiny app.

## Usage

``` r
ggpaintr_server_state(
  formula,
  envir = parent.frame(),
  copy_rules = NULL,
  ids = ggpaintr_ids(),
  placeholders = NULL
)
```

## Arguments

- formula:

  A single formula string using `ggpaintr` placeholders.

- envir:

  Environment used to resolve local data objects when building the app.

- copy_rules:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

- ids:

  A `ggpaintr_ids` object describing the top-level Shiny ids used by the
  integration helpers.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry.

## Value

An object of class `ggpaintr_state`.

## Examples

``` r
state <- ggpaintr_server_state(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
is.function(state$runtime)
#> [1] TRUE
```
