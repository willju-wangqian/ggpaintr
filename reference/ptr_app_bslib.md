# Build a ggpaintr Shiny App with a bslib Theme

A themed variant of
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
that lays out the generated controls and outputs inside a
[`bslib::page_sidebar()`](https://rstudio.github.io/bslib/reference/page_sidebar.html)
shell with
[`bslib::card()`](https://rstudio.github.io/bslib/reference/card.html)
containers. Intended as a worked example of how to reskin `ggpaintr`
using only its public API: the wrapper calls
[`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md),
[`ptr_input_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_input_ui.md),
and
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
and composes outputs from the id contract with plain `shiny` primitives
— no internal helpers are touched.

## Usage

``` r
ptr_app_bslib(
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  placeholders = NULL,
  expr_check = TRUE,
  theme = NULL,
  title = "ggpaintr",
  ns = shiny::NS(NULL)
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

  Controls `expr` placeholder validation. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).

- theme:

  A `bslib` theme object. Defaults to a Bootstrap 5 Flatly bootswatch.
  Pass any
  [`bslib::bs_theme()`](https://rstudio.github.io/bslib/reference/bs_theme.html)
  result to customize.

- title:

  App title shown in the page header.

- ns:

  An optional namespace function (`character -> character`). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details.

## Value

A `shiny.appobj`.

## Details

Requires the `bslib` package. Install it with
`install.packages("bslib")`.

## Examples

``` r
if (FALSE) { # \dontrun{
ptr_app_bslib(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
} # }
```
