# Level 2 — multiple ggpaintr instances and shared widgets

Use when: more than one ggpaintr instance lives in the same Shiny
session — two tabs, side-by-side comparison, multi-plot dashboard.

There is **no `ns =` argument** on Level-2 helpers. Namespacing happens
through the **module `id`** you pass to `ptr_module_ui()` /
`ptr_module_server()` (or to the `id` argument of `ptr_controls_ui()` /
`ptr_outputs_ui()` and the `moduleServer()` wrapping `ptr_server()`).
Each `id` is its own namespace; widget ids in different modules never
collide.

## Disjoint embeds — different `id`s

```r
library(shiny); library(ggpaintr)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("A", ptr_module_ui(
      "plot_a",
      "ggplot(mtcars, aes(x = var, y = var)) + geom_point()"
    )),
    tabPanel("B", ptr_module_ui(
      "plot_b",
      "ggplot(iris, aes(x = var, y = var)) + geom_point()"
    ))
  )
)

server <- function(input, output, session) {
  ptr_module_server("plot_a",
    "ggplot(mtcars, aes(x = var, y = var)) + geom_point()")
  ptr_module_server("plot_b",
    "ggplot(iris, aes(x = var, y = var)) + geom_point()")
}

shinyApp(ui, server)
```

Two formulas, two ids, two independent ggpaintr widgets sets. Nothing
else to wire — the `id` does the namespacing.

## Sharing widgets across instances — `ptr_shared_ui()` + `ptr_shared_server()`

When two or more embeds need to **share** a placeholder value (one X
column drives both plots, for example), use the `shared = "<key>"`
annotation on the placeholder occurrences and render one top-level
shared panel:

```r
library(shiny); library(ggpaintr)

formula_a <- "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Length,
                                fill = Species)) + geom_boxplot()"
formula_b <- "ggplot(iris, aes(x = var(shared = 'metric'), y = Sepal.Width,
                                fill = Species)) + geom_violin()"
formulas  <- list(formula_a, formula_b)

ui <- fluidPage(
  ptr_shared_ui(formulas),                           # one shared panel
  fluidRow(
    column(6, ptr_module_ui("plot_a", formula_a)),
    column(6, ptr_module_ui("plot_b", formula_b))
  )
)

server <- function(input, output, session) {
  shared <- ptr_shared_server(formulas)              # one shared server
  ptr_module_server("plot_a", formula_a, shared_state = shared)
  ptr_module_server("plot_b", formula_b, shared_state = shared)
}

shinyApp(ui, server)
```

What happens:

- `ptr_shared_ui(formulas)` scans every formula for `shared = "..."`
  annotations and emits **one** wellPanel with one widget per distinct
  key. A "Draw all" button appears when `length(formulas) >= 2`.
- `ptr_shared_server(formulas)` builds the matching reactives, runs the
  shared-consumer resolution, and returns a `ptr_shared_state`.
- `ptr_module_server(..., shared_state = shared)` threads that state
  into each module, so the per-module pickers read from the shared
  inputs instead of rendering their own copy.

`shared_ui = list(<key> = function(id) <tag>)` on `ptr_shared_ui()` lets
you replace the auto-built widget for a specific key (e.g. swap the
auto-built column picker for a `sliderInput`).

## When to skip `ptr_shared_*()`

`ptr_shared_ui()` errors if no formula declares a `shared = "..."`
annotation. For instances that don't need to share anything, use plain
`ptr_module_ui()` / `ptr_module_server()` with distinct `id`s — no
shared helpers required.

## Failure modes

- **Same `id` on two instances.** Shiny silently keeps only the last
  `output[[id]] <-` assignment; one instance appears empty, no error.
  Always give each `ptr_module_ui()` / `ptr_module_server()` a distinct
  literal `id`.
- **`shared = "..."` declared but `shared_state` not threaded.**
  `ptr_module_server()` aborts with a clear "build a `ptr_shared_state`
  with `ptr_shared_server()`" message.
- **`shared_ui` builder produces the wrong widget kind.** The runtime
  validates that the resolved value is compatible with each downstream
  placeholder type — e.g. a slider feeding a `var` consumer will
  surface as an inline validation error.
