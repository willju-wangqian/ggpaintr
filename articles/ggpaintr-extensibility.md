# ggpaintr Extensibility

## Overview

`ggpaintr` still ships the default
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
and
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
wrappers, but it now also exposes a supported integration layer for
users who already have a Shiny app and want to embed the generated
`ggpaintr` controls, runtime, and export behavior into their own UI.

The supported integration pieces are:

- [`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md)
- [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
- [`ptr_setup_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_setup_controls.md)
- [`ptr_register_draw()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_draw.md)
- [`ptr_register_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_plot.md)
- [`ptr_register_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_error.md)
- [`ptr_register_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_code.md)
- [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md)
- [`ptr_extract_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_error.md)
- [`ptr_extract_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_code.md)
- [`ptr_input_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_input_ui.md)
- [`ptr_output_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_output_ui.md)

To compose and distribute your own Shiny app, use
[`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
together with the `ptr_register_*` helpers; see
[`?ptr_app_bslib`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_bslib.md)
for a worked example.

## Choosing the right surface

The current package surface is intentionally layered.

- use
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  or
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  for most apps
- use
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md)
  plus the `ptr_register_*()` helpers when you already own a larger
  Shiny layout and want to embed `ggpaintr`
- use the low-level `ptr_*` runtime helpers, together with
  [`ptr_runtime_input_spec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_runtime_input_spec.md),
  when you are writing tests, tooling, or package-level extensions
  around parsed formulas

That split is a little opinionated, but it keeps the beginner path small
while still exposing a supported integration seam for more advanced
Shiny work.

## Recipe 1: Embed `ggpaintr` with the default binders

``` r
library(ggpaintr)
library(shiny)

ui <- fluidPage(
  titlePanel("Embedded ggpaintr"),
  sidebarLayout(
    sidebarPanel(
      ptr_input_ui()
    ),
    mainPanel(
      ptr_output_ui()
    )
  )
)

server <- function(input, output, session) {
  # Parse the formula and create reactive state — holds the placeholder map,
  # the last runtime result, and the current input spec.
  ptr_state <- ptr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) +
      geom_point() +
      labs(title = text)"
  )

  # Register dynamic var-column selectors and the file upload handler.
  # Must be called before ptr_register_draw() so data is available to var inputs.
  ptr_setup_controls(input, output, ptr_state)

  # Observe the draw button; re-run ptr_exec() and update ptr_state$runtime()
  # each time it is clicked.
  ptr_register_draw(input, ptr_state)

  # renderPlot(): reads ptr_state$runtime()$plot and renders it.
  ptr_register_plot(output, ptr_state)

  # renderText() / renderUI(): reads ptr_state$runtime()$error and shows it.
  ptr_register_error(output, ptr_state)

  # renderText(): reads ptr_state$runtime()$code_text and shows generated code.
  ptr_register_code(output, ptr_state)
}

shinyApp(ui, server)
```

This is the closest supported equivalent to
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md),
but it lets you place the controls and outputs inside your own page
layout.

## Recipe 2: Customize top-level ids

Use
[`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md)
when your app already has its own naming scheme or you want to mount
`ggpaintr` outputs in specific UI containers.

``` r
ids <- ptr_build_ids(
  # The top-level ids that ggpaintr uses for its standard UI elements.
  # Provide custom strings when your app already uses those default id names
  # for something else, or when you want ggpaintr outputs in specific containers.
  control_panel  = "builder_controls",  # tabsetPanel that holds per-layer control tabs
  draw_button    = "render_plot",        # actionButton that triggers plot re-render
  plot_output    = "main_plot",          # plotOutput widget
  error_output   = "main_error",         # output for parse / plot error messages
  code_output    = "main_code"           # verbatimTextOutput for the generated ggplot code
)

ids
```

``` r
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ptr_input_ui(ids = ids)
    ),
    mainPanel(
      ptr_output_ui(ids = ids)
    )
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
    ids = ids
  )

  ptr_setup_controls(input, output, ptr_state, ids = ids)
  ptr_register_draw(input, ptr_state, ids = ids)
  ptr_register_plot(output, ptr_state, ids = ids)
  ptr_register_error(output, ptr_state, ids = ids)
  ptr_register_code(output, ptr_state, ids = ids)
}
```

Only the six top-level ids are configurable in this phase. Internal
placeholder ids and dynamic `var-*` output ids remain package-owned.

## Recipe 3: Customize the returned plot in `renderPlot()`

The default
[`ptr_register_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_register_plot.md)
binder preserves the current wrapper behavior. If you want to modify the
built plot before rendering, write your own
[`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) and use
[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md).

``` r
server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
  )

  ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)

  output$outputPlot <- renderPlot({
    # ptr_state$runtime() is a reactive value updated each time the draw button
    # is clicked. It holds the full result of ptr_exec() for the current inputs.
    # ptr_extract_plot() pulls the ggplot object out of that result,
    # returning NULL when the plot failed or the draw button hasn't been clicked.
    plot_obj <- ptr_extract_plot(ptr_state$runtime())

    if (is.null(plot_obj)) {
      plot.new()           # show a blank canvas instead of an error frame
      return(invisible(NULL))
    }

    # Modify the ggplot object freely before it is rendered.
    # Here we apply a theme; you could also add layers, scales, or annotations.
    plot_obj + ggplot2::theme_minimal()
  })
}
```

[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md)
returns the raw `ggplot` object on success and `NULL` otherwise. That
keeps the advanced customization seam side-effect free and lets you
decide how to render failure states in your own app.

## Pure helpers versus bind helpers

Use the bind helpers when you want the standard `ggpaintr` behavior with
custom layout or custom top-level ids.

Use the pure value helpers when you want to own the rendering details:

- [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md)
  for plot customization
- [`ptr_extract_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_error.md)
  for custom [`renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html)
  workflows
- [`ptr_extract_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_code.md)
  for custom
  [`renderText()`](https://rdrr.io/pkg/shiny/man/renderPrint.html) or
  downstream processing

## How to improve the advanced surface

The current advanced surface is powerful, but it is still closer to
package developer tooling than to a polished app-builder DSL. The next
improvements should stay incremental:

- improve docs and helper discoverability first
- add higher-level helper constructors for common placeholder patterns
  only after the current low-level registry contract has settled a bit
  more
- consider an optional Shiny module wrapper on top of the current
  id-based helpers rather than replacing the existing supported surface
- do not introduce a separate declarative end-user spec in this phase

## Roadmap for the formula-string runtime

The formula-string model is still the author-facing interface. To make
the runtime easier to reason about without giving up that authoring
model, the next hardening step should happen internally:

- keep formula strings as the public authoring format
- compile each parsed `ptr_obj` once into a richer internal runtime
  contract
- have runtime completion consume that compiled contract instead of
  repeatedly relying on raw expression walks and companion-id
  conventions

That approach would tighten the runtime semantics without forcing users
to rewrite formulas into a separate declarative language.
