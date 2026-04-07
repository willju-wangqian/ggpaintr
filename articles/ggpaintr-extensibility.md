# ggpaintr Extensibility

## Overview

`ggpaintr` still ships the default
[`ggpaintr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_app.md)
and
[`ggpaintr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_server.md)
wrappers, but it now also exposes a supported integration layer for
users who already have a Shiny app and want to embed the generated
`ggpaintr` controls, runtime, and export behavior into their own UI.

The supported integration pieces are:

- [`ggpaintr_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_ids.md)
- [`ggpaintr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_server_state.md)
- [`ggpaintr_bind_control_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_control_panel.md)
- [`ggpaintr_bind_draw()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_draw.md)
- [`ggpaintr_bind_export()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_export.md)
- [`ggpaintr_bind_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_plot.md)
- [`ggpaintr_bind_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_error.md)
- [`ggpaintr_bind_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_code.md)
- [`ggpaintr_plot_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_plot_value.md)
- [`ggpaintr_error_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_error_value.md)
- [`ggpaintr_code_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_code_value.md)
- [`ggpaintr_controls_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_controls_ui.md)
- [`ggpaintr_outputs_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_outputs_ui.md)

## Recipe 1: Embed `ggpaintr` with the default binders

``` r
library(ggpaintr)
library(shiny)

ui <- fluidPage(
  titlePanel("Embedded ggpaintr"),
  sidebarLayout(
    sidebarPanel(
      ggpaintr_controls_ui()
    ),
    mainPanel(
      ggpaintr_outputs_ui()
    )
  )
)

server <- function(input, output, session) {
  paintr_state <- ggpaintr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) +
      geom_point() +
      labs(title = text)"
  )

  ggpaintr_bind_control_panel(input, output, paintr_state)
  ggpaintr_bind_draw(input, paintr_state)
  ggpaintr_bind_export(output, paintr_state)
  ggpaintr_bind_plot(output, paintr_state)
  ggpaintr_bind_error(output, paintr_state)
  ggpaintr_bind_code(output, paintr_state)
}

shinyApp(ui, server)
```

This is the closest supported equivalent to
[`ggpaintr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_server.md),
but it lets you place the controls and outputs inside your own page
layout.

## Recipe 2: Customize top-level ids

Use
[`ggpaintr_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_ids.md)
when your app already has its own naming scheme or you want to mount
`ggpaintr` outputs in specific UI containers.

``` r
ids <- ggpaintr_ids(
  control_panel = "builder_controls",
  draw_button = "render_plot",
  export_button = "export_app",
  plot_output = "main_plot",
  error_output = "main_error",
  code_output = "main_code"
)

ids
```

``` r
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      ggpaintr_controls_ui(ids = ids)
    ),
    mainPanel(
      ggpaintr_outputs_ui(ids = ids)
    )
  )
)

server <- function(input, output, session) {
  paintr_state <- ggpaintr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
    ids = ids
  )

  ggpaintr_bind_control_panel(input, output, paintr_state, ids = ids)
  ggpaintr_bind_draw(input, paintr_state, ids = ids)
  ggpaintr_bind_export(output, paintr_state, ids = ids)
  ggpaintr_bind_plot(output, paintr_state, ids = ids)
  ggpaintr_bind_error(output, paintr_state, ids = ids)
  ggpaintr_bind_code(output, paintr_state, ids = ids)
}
```

Only the six top-level ids are configurable in this phase. Internal
placeholder ids and dynamic `var-*` output ids remain package-owned.

## Recipe 3: Customize the returned plot in `renderPlot()`

The default
[`ggpaintr_bind_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_bind_plot.md)
binder preserves the current wrapper behavior. If you want to modify the
built plot before rendering, write your own
[`renderPlot()`](https://rdrr.io/pkg/shiny/man/renderPlot.html) and use
[`ggpaintr_plot_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_plot_value.md).

``` r
server <- function(input, output, session) {
  paintr_state <- ggpaintr_server_state(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
  )

  ggpaintr_bind_control_panel(input, output, paintr_state)
  ggpaintr_bind_draw(input, paintr_state)
  ggpaintr_bind_export(output, paintr_state)
  ggpaintr_bind_error(output, paintr_state)
  ggpaintr_bind_code(output, paintr_state)

  output$outputPlot <- renderPlot({
    plot_obj <- ggpaintr_plot_value(paintr_state$runtime())

    if (is.null(plot_obj)) {
      plot.new()
      return(invisible(NULL))
    }

    plot_obj + ggplot2::theme_minimal()
  })
}
```

[`ggpaintr_plot_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_plot_value.md)
returns the raw `ggplot` object on success and `NULL` otherwise. That
keeps the advanced customization seam side-effect free and lets you
decide how to render failure states in your own app.

## Pure helpers versus bind helpers

Use the bind helpers when you want the standard `ggpaintr` behavior with
custom layout or custom top-level ids.

Use the pure value helpers when you want to own the rendering details:

- [`ggpaintr_plot_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_plot_value.md)
  for plot customization
- [`ggpaintr_error_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_error_value.md)
  for custom [`renderUI()`](https://rdrr.io/pkg/shiny/man/renderUI.html)
  workflows
- [`ggpaintr_code_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_code_value.md)
  for custom
  [`renderText()`](https://rdrr.io/pkg/shiny/man/renderPrint.html) or
  downstream processing
