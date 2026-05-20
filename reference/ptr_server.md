# Server for a `ggpaintr` Formula

The **single public server-side entry point** for a `ggpaintr` formula,
used identically at L2 (default layout via
[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md))
and L3 (your own hand-composed layout from the bare `ptr_ui_*` pieces).
It namespaces and wires the whole reactive engine, registers the
built-in `ptr_plot` / `ptr_error` / `ptr_code` outputs (a piece you
never place in the UI is a harmless no-op), and **returns the
`ptr_state`** so you can drive a custom renderer. Additional arguments
are forwarded to
[`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md)
(e.g. `shared`, `draw_trigger`, `expr_check`, `safe_to_remove`,
`ui_text`, `checkbox_defaults`).

## Usage

``` r
ptr_server(
  formula,
  id = NULL,
  envir = parent.frame(),
  ...,
  shared_state = NULL
)
```

## Arguments

- formula:

  A single formula string with `ggpaintr` placeholders.

- id:

  Optional module id; must match the id passed to
  [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
  or to the bare L3 pieces. Defaults to `NULL` (identity namespace,
  single-instance use).

- envir:

  Environment used to resolve local data objects.

- ...:

  Forwarded to
  [`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md).

- shared_state:

  Optional `ptr_shared_state` returned by
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md).
  When supplied, populates `shared`, `draw_trigger`, and
  `shared_resolutions` defaults. Required when the formula declares a
  `shared = "..."` placeholder driven by a cross-formula
  [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
  and the equivalent `...` arguments are not supplied directly.

## Value

The `ptr_state` list from
[`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md).
This is the **supported L3 custom-render handle**:
`state$runtime()$plot` / `$code` / `$error`. (Its `server_ns_fn` /
`ui_ns_fn` slots are internal plumbing — not a public escape hatch.)

## Custom render (L3)

Custom rendering is **UI-side**: place your own output widget (e.g.
[`plotly::plotlyOutput()`](https://rdrr.io/pkg/plotly/man/plotly-shiny.html))
at `shiny::NS(id)("my_plot")`, never place
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
and read the live plot off the returned state — there is **no**
user-authored `moduleServer` wrapping any ggpaintr engine and **no**
lower-level server function to reach for:

    # server:
    state <- ptr_server(formula, "p")
    output$my_plot <- plotly::renderPlotly(state$runtime()$plot)
    # ui: plotly::plotlyOutput(shiny::NS("p")("my_plot"))

`state$runtime()` is reactive; `$plot` is the built ggplot/ggplot-like
object, `$code` the generated source string, `$error` any inline error.
See
[`vignette("ggpaintr-use-cases")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-use-cases.md).

For cross-formula coordination — multiple ggpaintr instances driven by
one widget — build the coordinator with
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md)
and pass the returned `ptr_shared_state` as `shared_state =`. The
state's `shared` / `draw_trigger` / `shared_resolutions` slots are
unpacked and forwarded to
[`ptr_init_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_init_state.md);
if an explicit `shared = ...` / `draw_trigger = ...` /
`shared_resolutions = ...` is also passed via `...`, that explicit value
wins. A single formula with `shared = "..."` placeholders needs no
`shared_state` — `ptr_server()` self-binds every declared key under its
own namespace, matching what
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
does.

## See also

[`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md),
[`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md),
[`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md),
[`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md),
[`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md).

## Examples

``` r
if (interactive()) {
  f <- "ggplot(mtcars, aes(x = var, y = var)) + geom_point()"
  # L2: default layout
  shiny::shinyApp(
    ui = shiny::fluidPage(ptr_ui(f, "p")),
    server = function(input, output, session) {
      ptr_server(f, "p")
    }
  )
  # L3: own the render path off the returned state
  shiny::shinyApp(
    ui = ptr_ui_page(
      ptr_ui_controls(formula = f, id = "p"),
      plotly::plotlyOutput(shiny::NS("p")("my_plot"))
    ),
    server = function(input, output, session) {
      state <- ptr_server(f, "p")
      output[[shiny::NS("p")("my_plot")]] <-
        plotly::renderPlotly(state$runtime()$plot)
    }
  )
}
```
