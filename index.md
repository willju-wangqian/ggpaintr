# ggpaintr

`ggpaintr` is an R package for the maintained `ggpaintr` workflow. It
turns a single ggplot-like formula string into a small Shiny app with
generated controls, rendered plots, generated code, upload support, and
inline runtime feedback.

## Installation

``` r
# install.packages("pak")
pak::pkg_install("willju-wangqian/ggpaintr")
```

## The `ggpaintr` model

Supported placeholders inside a formula string are:

- `var`
- `text`
- `num`
- `expr`
- `upload`

`upload` currently supports `.csv` and `.rds`.

## Quick start

``` r
library(ggpaintr)
library(ggplot2)

obj <- paintr_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) +
    geom_point(size = num) +
    labs(title = text)"
)

runtime <- paintr_build_runtime(
  obj,
  list(
    "ggplot+3+2" = "mpg",
    "ggplot+3+3" = "disp",
    "geom_point+2" = 2,
    "labs+2" = "Mtcars scatter",
    "geom_point+checkbox" = TRUE,
    "labs+checkbox" = TRUE
  )
)

runtime$code_text
#> [1] "ggplot(data = mtcars, aes(x = mpg, y = disp)) +\n  geom_point(size = 2) +\n  labs(title = \"Mtcars scatter\")"
inherits(runtime$plot, "ggplot")
#> [1] TRUE
```

## Launch an app

``` r
library(ggpaintr)

ggpaintr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

## Integrate `ggpaintr` into an existing app

The default wrappers stay available, but you can now embed `ggpaintr`
into your own Shiny layout with custom ids and custom server wiring.

``` r
library(ggpaintr)
library(ggplot2)
library(shiny)

ids <- ggpaintr_ids(
  control_panel = "builder_controls",
  draw_button = "render_plot",
  export_button = "export_app",
  plot_output = "main_plot",
  error_output = "main_error",
  code_output = "main_code"
)

ui <- fluidPage(
  titlePanel("Custom ggpaintr integration"),
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
    "ggplot(data = iris, aes(x = var, y = var)) +
      geom_point() +
      labs(title = text)",
    ids = ids
  )

  ggpaintr_bind_control_panel(input, output, paintr_state, ids = ids)
  ggpaintr_bind_draw(input, paintr_state, ids = ids)
  ggpaintr_bind_export(output, paintr_state, ids = ids)
  ggpaintr_bind_error(output, paintr_state, ids = ids)
  ggpaintr_bind_code(output, paintr_state, ids = ids)

  output[[ids$plot_output]] <- renderPlot({
    plot_obj <- ggpaintr_plot_value(paintr_state$runtime())

    if (is.null(plot_obj)) {
      plot.new()
      return(invisible(NULL))
    }

    plot_obj + ggplot2::theme_minimal()
  })
}

shinyApp(ui, server)
```

## Export a standalone app

``` r
obj <- paintr_formula(
  "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
)

generate_shiny(obj, list(), tempfile(fileext = ".R"))
```

## Runtime behavior

- Structural formula errors fail at parse time.
- `var` with no data source fails while preparing UI.
- Missing local data objects are deferred to draw-time plot errors.
- Render-time ggplot failures are shown through the same inline error
  channel.

See
[`vignette("ggpaintr-workflow")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-workflow.md)
for the main workflow and
[`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md)
for integration recipes.
