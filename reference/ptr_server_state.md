# Build Reactive Server State for ggpaintr

Create the shared reactive state used by the extensible `ptr_*` server
helpers. This object can be passed to the bind helpers or inspected
directly inside a larger Shiny app.

## Usage

``` r
ptr_server_state(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  ids = ptr_build_ids(),
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

- ids:

  A `ptr_build_ids` object describing the top-level Shiny ids used by
  the integration helpers.

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

An object of class `ptr_state`.

## Examples

``` r
state <- ptr_server_state(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
is.function(state$runtime)
#> [1] TRUE
```
