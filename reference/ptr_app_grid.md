# Multi-Plot Shiny App With Shared Placeholder Controls

Build a single Shiny app that hosts multiple ggpaintr formulas in one
session and drives them from a top region of shared controls. Each
formula becomes its own ptr_obj with its own draw button and plot
output. Placeholders carrying a `shared = "<id>"` annotation read their
value from the matching widget in `shared_ui`; non-shared placeholders
still render their per-instance widgets in each plot's panel as usual.

## Usage

``` r
ptr_app_grid(
  plots,
  shared_ui = list(),
  envir = parent.frame(),
  title = "ggpaintr grid",
  draw_all_label = "Draw all",
  expr_check = TRUE
)
```

## Arguments

- plots:

  Character vector or list of formula strings; one ptr_obj per entry.

- shared_ui:

  Named list of `function(id) -> shiny.tag`. Each name must match a
  `shared = "<id>"` annotation used by at least one of the `plots`
  formulas. The function is called with the shared name as the widget
  input id; the resulting widget is rendered once at the top of the app
  and its `input[[id]]` is wired into every plot's `shared` argument.

- envir:

  Environment used to resolve local data objects.

- title:

  Title displayed in the app header.

- draw_all_label:

  Label for the top-level "draw all" button that triggers a redraw of
  every plot in the grid. The button is rendered alongside the shared
  widgets so changes to those widgets can be broadcast to all plots in
  one click. Defaults to `"Draw all"`.

- expr_check:

  Controls `expr` placeholder validation. See
  [`ptr_server_state()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server_state.md).

## Value

A `shiny.appobj`. Run with
[`shiny::runApp()`](https://rdrr.io/pkg/shiny/man/runApp.html) or print
at the REPL.

## Details

Layout is fixed: shared controls in a top `wellPanel`, then one
equally-sized column per plot below. For more flexible arrangements, use
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
directly inside your own custom Shiny UI.

## Examples

``` r
if (interactive()) {
  app <- ptr_app_grid(
    plots = list(
      'ggplot(data = mtcars, aes(x = wt, y = mpg)) +
         geom_point(size = num(shared = "sz"))',
      'ggplot(data = mtcars, aes(x = hp, y = mpg)) +
         geom_point(size = num(shared = "sz"))'
    ),
    shared_ui = list(
      sz = function(id) shiny::sliderInput(id, "Size", 1, 10, value = 3)
    )
  )
  shiny::runApp(app)
}
```
