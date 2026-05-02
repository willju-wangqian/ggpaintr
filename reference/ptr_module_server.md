# Register ggpaintr Server Logic for a Shiny Module

Use this helper with
[`ptr_module_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_module_ui.md)
when you prefer Shiny modules for Level 2 integration. The function also
documents the module ID contract: server-side ids stay local to the
module, while UI generated from `renderUI()` is namespaced with
`session$ns`.

## Usage

``` r
ptr_module_server(
  id,
  formula,
  envir = parent.frame(),
  ui_text = NULL,
  placeholders = NULL,
  checkbox_defaults = NULL,
  expr_check = TRUE,
  safe_to_remove = character(),
  shared = list(),
  draw_trigger = NULL
)
```

## Arguments

- id:

  Module id.

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

- checkbox_defaults:

  Optional named list of initial checked states for layer checkboxes.
  See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md).

- expr_check:

  Controls `expr` placeholder validation. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md).

- safe_to_remove:

  Character vector of additional function names whose zero-argument
  calls should be dropped after placeholder substitution leaves them
  empty. See
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  for the curated default set and full semantics. Defaults to
  [`character()`](https://rdrr.io/r/base/character.html).

- shared:

  Named list of Shiny reactives (or empty list). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details.

- draw_trigger:

  Optional Shiny reactive (or `NULL`). See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  for details.

## Value

A `ptr_state` object containing reactive accessors named `obj`,
`runtime`, and `var_ui_list`, plus shared metadata used by the bind
helpers.

## Examples

``` r
ui <- shiny::fluidPage(ptr_module_ui("plot1"))
server <- function(input, output, session) {
  ptr_module_server(
    "plot1",
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
}
if (interactive()) {
  shiny::shinyApp(ui, server)
}
```
