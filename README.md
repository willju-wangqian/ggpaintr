
# ggpaintr

`ggpaintr` turns one ggplot-like formula string into a small Shiny app
with generated controls, rendered plots, generated code, upload support,
copy customization, custom Shiny integration hooks, custom placeholder
registries, and export helpers.

## Installation

``` r
# install.packages("pak")
pak::pkg_install("willju-wangqian/ggpaintr")
```

## Core concepts

`ggpaintr` treats a ggplot-like formula string as a template with
placeholders that become Shiny inputs.

  - `var` selects a data column in the generated UI
  - `text` collects free text
  - `num` collects numeric input
  - `expr` collects raw R code for places like faceting or labels
  - `upload` lets the app use uploaded data
  - `ggpaintr_normalize_column_names()` cleans local column names before
    `var` selection when the source data is not already syntactic
  - you can register your own placeholder types per app with
    `ggpaintr_placeholder()`

`upload` currently supports `.csv` and `.rds`.

``` r
"ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(size = num, color = text) +
  labs(title = text) +
  facet_wrap(expr)"
```

## Why this design

`ggpaintr` keeps the author-facing input as one formula string on
purpose.

  - formula strings stay close to the way many R users already sketch
    ggplot code
  - the same parsed object can drive generated controls, runtime
    completion, generated code, and standalone export
  - one placeholder registry powers parse, UI, runtime, copy rules, and
    export for both built-in and custom placeholders
  - the package gives you both a default wrapper and a supported Shiny
    embedding layer, so you can start simple and grow into a more custom
    app
  - exported apps stay explicit `ui <- ...` and `server <-
    function(...)` files so they remain readable and editable outside
    the package wrapper

## Quick start

Most users should start with `ggpaintr_app()`.

``` r
library(ggpaintr)

ggpaintr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

## Supported public API

The maintained public path is intentionally narrow.

  - Start with `ggpaintr_app()` for the default app wrapper.
  - Use `ggpaintr_server()`, `ggpaintr_server_state()`,
    `ggpaintr_ids()`, and the `ggpaintr_bind_*()` helpers when you need
    to embed `ggpaintr` in a larger Shiny app.
  - Use `ggpaintr_placeholder()` and `ggpaintr_effective_placeholders()`
    for custom placeholder types.
  - Use `ggpaintr_runtime_input_spec()`, `paintr_formula()`,
    `paintr_build_runtime()`, and `paintr_get_plot()` for advanced
    runtime or testing workflows.
  - Use `generate_shiny()` to export an explicit standalone app script.

Other helpers in `R/` are internal implementation support rather than
part of the maintained community-facing API.

## Feature tour

### Work with uploaded data

Use `upload` when the app should let users supply a dataset at runtime.

``` r
ggpaintr_app("
ggplot(data = upload, aes(x = var, y = var)) +
  geom_point(size = num) +
  labs(title = text)
")
```

The upload control currently supports `.csv` and `.rds`.

Uploads are normalized through the same column-name helper automatically
after read-in. `.rds` uploads must already be tabular or be coercible
with `as.data.frame()`.

### Prepare local data with non-syntactic column names

If your local data uses spaces or punctuation in column names, normalize
it once before you build the app.

``` r
messy_sales <- data.frame(
  left = 1:4,
  right = c(2, 4, 6, 8),
  check.names = FALSE
)
names(messy_sales) <- c("first column", "second-column")

sales <- ggpaintr_normalize_column_names(messy_sales)
names(sales)

ggpaintr_app("
ggplot(data = sales, aes(x = var, y = var)) +
  geom_point()
")
```

### Put transforms around `var` in the formula

`var` is a column picker. When you want derived mappings, wrap the
placeholder in the formula itself and let the selected column slot into
that expression.

``` r
ggpaintr_app("
ggplot(data = mtcars, aes(x = var + 1, y = log(var))) +
  geom_point()
")
```

### Customize control text with `copy_rules`

Use `copy_rules` to change user-facing labels without changing the
runtime behavior of the generated app.

``` r
custom_copy <- list(
  shell = list(
    draw_button = list(label = "Render plot"),
    export_button = list(label = "Save app")
  ),
  params = list(
    x = list(var = list(label = "X axis variable")),
    y = list(var = list(label = "Y axis variable")),
    title = list(text = list(label = "Plot title"))
  )
)

ggpaintr_app(
  "ggplot(data = iris, aes(x = var, y = var)) +
     geom_point() +
     labs(title = text)",
  copy_rules = custom_copy
)
```

### Export a standalone app

Use `generate_shiny()` when you want an explicit, editable Shiny app
file that you can continue customizing outside the package wrapper.

``` r
obj <- paintr_formula(
  "ggplot(data = upload, aes(x = var, y = var)) +
    geom_point() +
    labs(title = text)"
)

app_file <- tempfile(fileext = ".R")
generate_shiny(obj, app_file)

# The generated file keeps an explicit ui, an explicit server, and visible
# copy_rules/placeholders hooks so you can keep editing it as a normal Shiny app.
```

### Embed `ggpaintr` in your own Shiny app

The supported integration layer lets you keep your own layout while
reusing the `ggpaintr` state and bind helpers.

``` r
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
  sidebarLayout(
    sidebarPanel(ggpaintr_controls_ui(ids = ids)),
    mainPanel(ggpaintr_outputs_ui(ids = ids))
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
  ggpaintr_bind_plot(output, paintr_state, ids = ids)
  ggpaintr_bind_error(output, paintr_state, ids = ids)
  ggpaintr_bind_code(output, paintr_state, ids = ids)
}

shinyApp(ui, server)
```

### Customize the plot in your own `renderPlot()`

If you want to keep the `ggpaintr` runtime but take over plot rendering,
use `ggpaintr_plot_value()` inside your own `renderPlot()`.

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

### Register a custom placeholder type

Use `ggpaintr_placeholder()` and `ggpaintr_effective_placeholders()`
when you want to add a new control type without editing package
internals.

``` r
sales <- data.frame(
  day = as.Date("2024-01-01") + 0:4,
  value = c(10, 13, 12, 16, 18)
)

date_placeholder <- ggpaintr_placeholder(
  keyword = "date",
  build_ui = function(id, copy, meta, context) {
    shiny::dateInput(id, copy$label)
  },
  resolve_expr = function(value, meta, context) {
    if (is.null(value) || identical(as.character(value), "")) {
      return(ggpaintr_missing_expr())
    }

    rlang::expr(as.Date(!!as.character(value)))
  },
  copy_defaults = list(label = "Choose a date for {param}")
)

placeholders <- ggpaintr_effective_placeholders(
  list(date = date_placeholder)
)

obj <- paintr_formula(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)

names(obj$placeholders)
#> [1] "var"    "text"   "num"    "expr"   "upload" "date"
```

``` r
ggpaintr_app(
  "ggplot(data = sales, aes(x = day, y = value)) +
    geom_line() +
    geom_vline(xintercept = date)",
  placeholders = placeholders
)
```

If you want to export a standalone app with a custom placeholder, define
the placeholder hooks inline inside the `ggpaintr_placeholder()` call.
The export path serializes that stored call into the generated app, so
helper functions that only exist in your current session are not
exportable today.

### Advanced developer workflow

Use the low-level runtime helpers directly when you want to inspect
generated code, write tests, or build developer tooling around parsed
formulas.

`ggpaintr_runtime_input_spec()` is the supported way to discover the
runtime input ids needed by `paintr_build_runtime()`.

``` r
obj <- paintr_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) +
    geom_point(size = num) +
    labs(title = text)"
)

spec <- ggpaintr_runtime_input_spec(obj)
spec
#>              input_id           role layer_name keyword param_key    source_id
#> 1          ggplot+3+2    placeholder     ggplot     var         x   ggplot+3+2
#> 2          ggplot+3+3    placeholder     ggplot     var         y   ggplot+3+3
#> 3        geom_point+2    placeholder geom_point     num      size geom_point+2
#> 4              labs+2    placeholder       labs    text     title       labs+2
#> 5 geom_point+checkbox layer_checkbox geom_point    <NA>      <NA>         <NA>
#> 6       labs+checkbox layer_checkbox       labs    <NA>      <NA>         <NA>

inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
checkbox_rows <- spec$role == "layer_checkbox"
inputs[checkbox_rows] <- rep(list(TRUE), sum(checkbox_rows))
inputs[["ggplot+3+2"]] <- "mpg"
inputs[["ggplot+3+3"]] <- "disp"
inputs[["geom_point+2"]] <- 2
inputs[["labs+2"]] <- "Mtcars scatter"

runtime <- paintr_build_runtime(obj, inputs)

runtime$code_text
#> [1] "ggplot(data = mtcars, aes(x = mpg, y = disp)) +\n  geom_point(size = 2) +\n  labs(title = \"Mtcars scatter\")"
inherits(runtime$plot, "ggplot")
#> [1] TRUE
```

For upload-backed formulas, the spec also includes the derived
dataset-name input that accompanies each upload control.

``` r
upload_obj <- paintr_formula(
  "ggplot(data = upload, aes(x = var, y = var)) + geom_point()"
)

ggpaintr_runtime_input_spec(upload_obj)
#>              input_id           role layer_name keyword param_key  source_id
#> 1            ggplot+2    placeholder     ggplot  upload      data   ggplot+2
#> 2       ggplot+2+name    upload_name     ggplot  upload      data   ggplot+2
#> 3          ggplot+3+2    placeholder     ggplot     var         x ggplot+3+2
#> 4          ggplot+3+3    placeholder     ggplot     var         y ggplot+3+3
#> 5 geom_point+checkbox layer_checkbox geom_point    <NA>      <NA>       <NA>
```

## Current stability guarantees

  - built-in placeholders are `var`, `text`, `num`, `expr`, and `upload`
  - custom placeholders share the same parse, UI, runtime, copy-rule,
    and export path as built-ins
  - `upload` currently supports `.csv` and `.rds`
  - the supported app-facing surface is the current `ggpaintr_*` wrapper
    and Shiny-integration layer
  - exported apps keep the current explicit-file shape built around
    `ggpaintr_server()`
  - only the six top-level ids exposed by `ggpaintr_ids()` are
    configurable
  - runtime failures are labeled as `Input error:` or `Plot error:` and
    stay on the shared inline error path

## Not guaranteed / implementation details

  - raw placeholder ids such as `"ggplot+3+2"` are not a stable
    hand-authored API; discover them with
    `ggpaintr_runtime_input_spec()`
  - deeper traversal details such as `index_path` encoding and internal
    companion id conventions remain package internals
  - unsupported upload formats are outside the current boundary
  - custom placeholders whose hooks are not defined inline inside
    `ggpaintr_placeholder()` are not exportable as standalone apps today
  - the formula-string model is the current author-facing interface;
    future hardening should compile it into a richer internal runtime
    contract rather than replace that authoring model outright

## Current behavior boundary

  - Structural formula errors fail early during parsing.
  - `var` with no data source fails while preparing the UI.
  - `var` is a column picker. Formula-level transforms such as `var + 1`
    or `log(var)` are supported inside the formula text, not as direct
    input values.
  - For local data with non-syntactic names, call
    `ggpaintr_normalize_column_names()` first; uploads apply the same
    normalization automatically.
  - Missing local data objects are deferred to draw-time inline errors.
  - Advanced integrations can customize the plot through
    `ggpaintr_plot_value()`.

## Where to go next

See `vignette("ggpaintr-workflow")` for the main workflow and
`vignette("ggpaintr-extensibility")` for supported Shiny integration
recipes. See `vignette("ggpaintr-placeholder-registry")` for the
placeholder registry API, hook contract, and export guidance.
