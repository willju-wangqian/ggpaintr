# ggpaintr in the Wild

``` r

library(ggpaintr)
library(ggplot2)
library(shiny)
library(rlang)
```

## 1. What this vignette is

The other vignettes teach the workflow piece by piece. This one shows
what the pieces add up to. Every section pairs a complete `ggplot2` (or
extension-package) graphic with the `ggpaintr` formula that turns it
into an interactive Shiny app. Read the original to know what is being
built; read the formula to see which arguments become widgets.

Code chunks in this vignette are marked `eval = interactive()` — they
will not run during vignette build, but every block is runnable as-is at
the R prompt. Copy a chunk into a session, install the extension package
named at the top of the section if you do not have it, and run it.

The examples assume
[`library(ggpaintr); library(ggplot2); library(shiny)`](https://willju-wangqian.github.io/ggpaintr/)
are already loaded.

## 2. Three reusable custom placeholders

Three custom placeholders show up across the gallery: `range` (a slider
returning a length-2 numeric), `cols` (a multi-select for column names),
and `date` (a date-range picker). Define them once and pass the same
registry to every later example.

``` r

range_ph <- ptr_define_placeholder(
  keyword = "range",
  build_ui = function(id, copy, meta, context) {
    sliderInput(id, copy$label, min = 0, max = 100,
                value = c(0, 100), step = 0.1)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value)) return(ptr_missing_expr())
    rlang::expr(c(!!value[1], !!value[2]))
  },
  copy_defaults = list(label = "Range for {param}")
)

cols_ph <- ptr_define_placeholder(
  keyword = "cols",
  build_ui = function(id, copy, meta, context) {
    uiOutput(paste0(id, "_container"))
  },
  bind_ui = function(input, output, metas, context) {
    eval_env  <- context$eval_env
    data_expr <- context$ptr_obj$expr_list[["ggplot"]]$data
    find_source <- function(e) {
      if (is.symbol(e)) return(e)
      if (is.call(e) && length(e) >= 2L) return(find_source(e[[2]]))
      NULL
    }
    src <- find_source(data_expr)
    df <- if (!is.null(src) && !is.null(eval_env)) {
      tryCatch(eval(src, envir = eval_env), error = function(e) NULL)
    }
    choices <- if (is.data.frame(df)) names(df) else character()
    for (meta in metas) local({
      m <- meta
      output[[paste0(m$id, "_container")]] <- renderUI({
        selectInput(m$id, paste("Columns for", m$param),
                    choices = choices, multiple = TRUE)
      })
    })
    invisible(NULL)
  },
  resolve_expr = function(value, meta, context) {
    if (length(value) == 0) return(ptr_missing_expr())
    rlang::expr(dplyr::all_of(!!value))
  },
  copy_defaults = list(label = "Columns for {param}")
)

date_ph <- ptr_define_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    dateRangeInput(id, copy$label,
                   start = "1970-01-01", end = "2015-04-01")
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value)) return(ptr_missing_expr())
    rlang::expr(c(as.Date(!!as.character(value[1])),
                  as.Date(!!as.character(value[2]))))
  },
  copy_defaults = list(label = "Date range for {param}")
)

registry <- ptr_merge_placeholders(list(
  range = range_ph, cols = cols_ph, date = date_ph
))
```

The walker that powers `cols`’s `bind_ui` — chasing the `data` argument
back to its source symbol through any pipe chain — is the same trick
explained in
[`vignette("ggpaintr-placeholder-registry")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-placeholder-registry.md).

## 3. A realistic ggplot2 graphic, parameterized

A typical exploratory plot stacks several layers and reads several
hard-coded constants. Here is a multi-layer fuel-economy chart from the
`mpg` dataset:

``` r

ggplot(mpg, aes(displ, hwy, color = class)) +
  geom_point(alpha = 0.6, size = 2) +
  geom_smooth(method = "loess", se = FALSE, span = 0.75) +
  facet_wrap(~ drv) +
  scale_color_brewer(palette = "Set2") +
  labs(title = "Highway MPG vs engine displacement",
       x = "Engine displacement (L)", y = "Highway MPG") +
  coord_cartesian(xlim = c(1, 7), ylim = c(10, 45))
```

Replace each tuning knob with a placeholder token. `var` becomes
`selectInput`s populated from the data; `num` becomes `numericInput`s;
`text` becomes `textInput`s; the custom `range` becomes a slider:

``` r

ptr_app(
  "ggplot(data = mpg, aes(x = var, y = var, color = var)) +
     geom_point(alpha = num, size = num) +
     geom_smooth(method = 'loess', se = FALSE, span = num) +
     facet_wrap(~ var) +
     scale_color_brewer(palette = 'Set2') +
     labs(title = text, x = text, y = text) +
     coord_cartesian(xlim = range, ylim = range)",
  placeholders = registry
)
```

Every aesthetic, alpha, size, span, and the two coordinate ranges is now
a widget. Rebuilding the original graphic from the running app is a
matter of picking those values; saving the generated code lifts the
selections back into a static script.

## 4. Data-cleaning pipelines into `ggplot()`

`ggpaintr` parses any expression that evaluates to a `ggplot` object,
including the common “clean, then plot” pipeline where the data is piped
into [`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html):

``` r

library(dplyr)

mpg |>
  dplyr::filter(displ > 1.5) |>
  dplyr::group_by(class) |>
  dplyr::filter(dplyr::n() > 5) |>
  dplyr::ungroup() |>
  ggplot(aes(displ, hwy, color = class)) +
  geom_point(alpha = 0.5)
```

The same pipeline parameterized — the two `filter` thresholds and the
point alpha become widgets:

``` r

library(dplyr)

ptr_app(
  "mpg |>
     dplyr::filter(displ > num) |>
     dplyr::group_by(class) |>
     dplyr::filter(dplyr::n() > num) |>
     dplyr::ungroup() |>
     ggplot(aes(displ, hwy, color = class)) +
     geom_point(alpha = num) +
     labs(title = text)"
)
```

You can put the cleaning steps inside `data = ...` instead — both shapes
are accepted:

``` r

ptr_app(
  "ggplot(data = mpg |>
                   dplyr::filter(displ > num) |>
                   dplyr::group_by(class) |>
                   dplyr::filter(dplyr::n() > num) |>
                   dplyr::ungroup(),
          aes(displ, hwy, color = class)) +
     geom_point(alpha = num)"
)
```

**Caveat — `var` and piped data.** The built-in `var` placeholder
populates its dropdown by introspecting the data argument, and that
introspection currently only follows a plain symbol (`data = mtcars`),
not a piped chain. If you write
`mtcars |> dplyr::filter(...) |> ggplot(aes(x = var, y = var))`, the
formula parses but the runtime cannot resolve the `var` widget at draw
time. Two ways out:

- **Bind data-dependent aesthetics to literal column names** instead of
  `var`, as in the examples above (`aes(displ, hwy)`).
- **Use a custom placeholder that does its own data lookup** — for
  example, the `cols` placeholder defined in section 2 walks the data
  argument back to its source symbol via `bind_ui`, so it works through
  pipe chains. See sections 5.1 and 6 for the pattern.

## 5. Interactive output packages

`ggpaintr` does not lock you into `renderPlot`. The runtime exposes the
fitted `ggplot` object via
[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md),
which feeds any output backend that accepts a `ggplot` — `plotly` and
`ggiraph` are the two most common.

### 5.1 plotly tooltips

Original — a hover-tooltipped scatter:

``` r

library(plotly)

p <- ggplot(mpg, aes(displ, hwy, color = class,
                     text = paste(manufacturer, model, sep = " "))) +
       geom_point(size = 3, alpha = 0.7) +
       coord_cartesian(xlim = c(1, 7), ylim = c(10, 45))

plotly::ggplotly(p, tooltip = "text")
```

ggpaintr version — embed at Level 2 so we own the output sink:

``` r

library(plotly)

formula <- "ggplot(data = mpg,
                   aes(x = var, y = var, color = var,
                       text = paste(manufacturer, model, sep = ' '))) +
              geom_point(size = num, alpha = num) +
              coord_cartesian(xlim = range, ylim = range)"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(ptr_input_ui()),
    mainPanel(plotly::plotlyOutput("interactive_plot"),
              verbatimTextOutput(ptr_build_ids()$code_output))
  )
)

server <- function(input, output, session) {
  state <- ptr_server(
    input, output, session,
    formula      = formula,
    placeholders = registry
  )
  output$interactive_plot <- plotly::renderPlotly({
    p <- ptr_extract_plot(state$runtime())
    if (is.null(p)) return(NULL)
    plotly::ggplotly(p, tooltip = "text")
  })
}

shinyApp(ui, server)
```

[`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract_plot.md)
returns `NULL` when the runtime is in an error state, so the
`if (is.null(p)) return(NULL)` guard keeps the output quiet between
draws.

### 5.2 ggiraph tooltips and click handling

Original — interactive points with a colour-picked highlight:

``` r

library(ggiraph)

ggplot(mpg, aes(displ, hwy)) +
  geom_point_interactive(aes(tooltip = model), size = 3, color = "#e63946")
```

ggpaintr version with a colour-picker placeholder so the user can
recolour the layer:

``` r

library(ggiraph)
library(colourpicker)

color_ph <- ptr_define_placeholder(
  keyword = "color",
  build_ui = function(id, copy, meta, context) {
    colourpicker::colourInput(id, copy$label, value = "#e63946")
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(value, "")) return(ptr_missing_expr())
    rlang::expr(!!as.character(value))
  },
  copy_defaults = list(label = "Highlight colour for {param}")
)

reg2 <- ptr_merge_placeholders(c(registry, list(color = color_ph)))

formula <- "ggplot(data = mpg, aes(x = var, y = var)) +
              geom_point_interactive(aes(tooltip = var),
                                     size = num, color = color)"

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(ptr_input_ui()),
    mainPanel(ggiraph::girafeOutput("interactive_plot"),
              verbatimTextOutput(ptr_build_ids()$code_output))
  )
)

server <- function(input, output, session) {
  state <- ptr_server(input, output, session,
                     formula = formula, placeholders = reg2)
  output$interactive_plot <- ggiraph::renderGirafe({
    p <- ptr_extract_plot(state$runtime())
    if (is.null(p)) return(NULL)
    ggiraph::girafe(code = print(p))
  })
}

shinyApp(ui, server)
```

## 6. ggplot2 extension packages

`ggpaintr` does not know about extension geoms — it sees them as
ordinary layer calls and parameterizes their arguments the same way it
parameterizes core geoms. The five examples below cover a parallel-
coordinates plot, ridge densities, repelled labels, alluvial flows, and
`ggdist` distribution slabs.

### 6.1 ggpcp — parallel coordinates

Original (uses the `flea` dataset shipped by `GGally`):

``` r

library(ggpcp)
data(flea, package = "GGally")

flea |>
  pcp_select(species, tars1, tars2, head, aede1, aede2, aede3) |>
  pcp_scale(method = "uniminmax") |>
  pcp_arrange() |>
  ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = species))
```

ggpaintr version — let the user pick which columns to put on the
parallel axes via the `cols` placeholder:

``` r

library(ggpcp)
data(flea, package = "GGally")

ptr_app(
  "ggplot(data = flea |>
                  pcp_select(cols) |>
                  pcp_scale(method = 'uniminmax') |>
                  pcp_arrange(),
          mapping = aes_pcp()) +
     geom_pcp_axes() +
     geom_pcp(aes(colour = species)) +
     coord_cartesian(xlim = range, ylim = range)",
  placeholders = registry
)
```

### 6.2 ggridges — ridge densities

Original (uses `lincoln_weather` shipped by `ggridges`):

``` r

library(ggridges)

ggplot(lincoln_weather,
       aes(x = `Mean Temperature [F]`, y = Month, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01,
                               bandwidth = 3) +
  scale_fill_viridis_c(option = "C")
```

ggpaintr version — bandwidth, scale, and the colour aesthetic become
widgets:

``` r

library(ggridges)

ptr_app(
  "ggplot(data = lincoln_weather, aes(x = var, y = var, fill = ..x..)) +
     geom_density_ridges_gradient(scale = num,
                                  rel_min_height = num,
                                  bandwidth = num) +
     scale_fill_viridis_c(option = 'C') +
     labs(title = text)",
  placeholders = registry
)
```

### 6.3 ggrepel — labelled scatter

Original — label cars by row name, repelling overlaps:

``` r

library(ggrepel)

mt <- data.frame(mtcars, model = rownames(mtcars))

ggplot(mt, aes(wt, mpg, label = model)) +
  geom_point(size = 3, color = "steelblue") +
  geom_text_repel(size = 3.5, max.overlaps = 12, box.padding = 0.3)
```

ggpaintr version — `max.overlaps`, `size`, and `box.padding` all turn
into numeric inputs. The label aesthetic is bound by `var`:

``` r

library(ggrepel)

mt <- data.frame(mtcars, model = rownames(mtcars))

ptr_app(
  "ggplot(data = mt, aes(x = var, y = var, label = var)) +
     geom_point(size = num, color = 'steelblue') +
     geom_text_repel(size = num,
                     max.overlaps = num,
                     box.padding = num) +
     labs(title = text)",
  placeholders = registry,
  envir = environment()
)
```

`envir = environment()` is the seam that lets the runtime see the local
`mt` data frame.
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
defaults to [`parent.frame()`](https://rdrr.io/r/base/sys.parent.html),
which would pick up `mt` here automatically; passing
[`environment()`](https://rdrr.io/r/base/environment.html) is explicit.

### 6.4 ggalluvial — flow diagrams

Original (uses the `vaccinations` dataset shipped by `ggalluvial`):

``` r

library(ggalluvial)

ggplot(vaccinations,
       aes(x = survey, stratum = response, alluvium = subject,
           y = freq, fill = response)) +
  geom_flow(alpha = 0.7) +
  geom_stratum() +
  scale_x_discrete(expand = c(0.1, 0.1))
```

ggpaintr version — alpha and the fill aesthetic become widgets:

``` r

library(ggalluvial)

ptr_app(
  "ggplot(data = vaccinations,
          aes(x = survey, stratum = response, alluvium = subject,
              y = freq, fill = var)) +
     geom_flow(alpha = num) +
     geom_stratum() +
     scale_x_discrete(expand = c(0.1, 0.1)) +
     labs(title = text)",
  placeholders = registry
)
```

### 6.5 ggdist — distribution visualizations

Original — half-eye distributions per cylinder count:

``` r

library(ggdist)

ggplot(mtcars, aes(x = factor(cyl), y = mpg)) +
  stat_halfeye(adjust = 0.5, justification = -0.2,
               .width = 0, point_colour = NA, slab_alpha = 0.6) +
  geom_boxplot(width = 0.15, outlier.shape = NA) +
  coord_flip()
```

ggpaintr version — every visual constant of `stat_halfeye()` becomes a
slider or numeric input:

``` r

library(ggdist)

ptr_app(
  "ggplot(data = mtcars, aes(x = factor(cyl), y = var)) +
     stat_halfeye(adjust = num,
                  justification = num,
                  .width = num,
                  point_colour = NA,
                  slab_alpha = num) +
     geom_boxplot(width = num, outlier.shape = NA) +
     coord_flip() +
     labs(title = text)",
  placeholders = registry
)
```

## 7. Shared placeholders — one widget, two graphics

[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)
is the turn-key entry point for multi-plot apps; it is limited to the
built-in placeholder keywords (`var`, `text`, `num`, `expr`, `upload`).
To share a *custom* placeholder across plots — a multi-column selector
driving both a parallel-coordinates plot and a faceted boxplot, for
example — drop down to Level 2 and wire the shared reactive yourself.

The mechanism: render one widget at the top of your UI; pass
`shared = list(<id> = reactive(input$<widget_id>))` to every
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
call; annotate the placeholder in each formula with `shared = "<id>"`.
The registry’s `resolve_expr` for that keyword still turns the shared
value into the right expression. The registry’s `build_ui`/`bind_ui` are
bypassed because the widget is yours now.

Original — two static views of the iris dataset coupled by a shared
column choice:

``` r

library(dplyr)
library(tidyr)
library(ggpcp)

picked <- c("Sepal.Length", "Sepal.Width", "Petal.Length")

p1 <- iris |>
  pcp_select(Species, dplyr::all_of(picked)) |>
  pcp_scale(method = "uniminmax") |>
  pcp_arrange() |>
  ggplot(aes_pcp()) +
    geom_pcp_axes() +
    geom_pcp(aes(colour = Species))

p2 <- iris |>
  dplyr::select(Species, dplyr::all_of(picked)) |>
  tidyr::pivot_longer(-Species, names_to = "metric") |>
  ggplot(aes(metric, value, fill = Species)) +
    geom_boxplot()

# Display side-by-side with patchwork::wrap_plots(p1, p2) or similar.
```

ggpaintr version — one shared `selectInput` at the top, two embedded
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
modules below, each formula referencing `cols(shared = "vars")`. Picking
columns in the shared control updates both plots together:

``` r

library(ggpcp)

ns_left  <- NS("left")
ns_right <- NS("right")

ui <- fluidPage(
  titlePanel("Shared column control"),
  wellPanel(
    selectInput("shared_cols", "Columns to plot",
                choices  = setdiff(names(iris), "Species"),
                selected = c("Sepal.Length", "Sepal.Width", "Petal.Length"),
                multiple = TRUE)
  ),
  fluidRow(
    column(6,
      h4("Parallel coordinates"),
      ptr_input_ui(ns = ns_left),
      ptr_output_ui(ns = ns_left)),
    column(6,
      h4("Faceted boxplot"),
      ptr_input_ui(ns = ns_right),
      ptr_output_ui(ns = ns_right))
  )
)

server <- function(input, output, session) {
  shared_reactives <- list(vars = reactive(input$shared_cols))

  ptr_server(
    input, output, session,
    formula = "ggplot(data = iris |>
                        pcp_select(Species, cols(shared = 'vars')) |>
                        pcp_scale(method = 'uniminmax') |>
                        pcp_arrange(),
                      mapping = aes_pcp()) +
                 geom_pcp_axes() +
                 geom_pcp(aes(colour = Species))",
    placeholders = registry,
    shared       = shared_reactives,
    ns           = ns_left
  )

  ptr_server(
    input, output, session,
    formula = "ggplot(data = iris |>
                        dplyr::select(Species, cols(shared = 'vars')) |>
                        tidyr::pivot_longer(-Species, names_to = 'metric'),
                      aes(x = metric, y = value, fill = Species)) +
                 geom_boxplot()",
    placeholders = registry,
    shared       = shared_reactives,
    ns           = ns_right
  )
}

shinyApp(ui, server)
```

The two
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
calls each see the same `shared_reactives` list, so editing the top
`selectInput` propagates to both plots through the shared `cols`
placeholder. Each plot still has its own draw button and its own
non-shared widgets (none in this formula, but they would render in the
per-plot input panel if the formula introduced them).

## 8. Where to go next

If a section above introduced an unfamiliar piece of the API, the
canonical reference is one of:

- [`vignette("ggpaintr-workflow")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-workflow.md)
  — the Level-1
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  story end-to-end.
- [`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md)
  — Level-2 (embed) and Level-3 (headless).
- [`vignette("ggpaintr-placeholder-registry")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-placeholder-registry.md)
  — the placeholder hook contract, including `bind_ui` and
  `prepare_eval_env`.
