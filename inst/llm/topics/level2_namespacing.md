# Level 2 — multiple ggpaintr instances in one session with `ns`

Use when: more than one ggpaintr instance lives in the same Shiny
session — two tabs, a side-by-side comparison, or a module instantiated
several times. Without `ns`, their widget ids collide.

## `ns` — what it is and where it goes

`ns` is a function of shape `character -> character`. The default
`shiny::NS(NULL)` is the identity (no prefix), reproducing pre-`ns`
behavior. Pass `shiny::NS("my_prefix")` to prefix every widget id
ggpaintr owns — top-level ids on `ptr_state$ids`, per-placeholder
input ids, checkbox ids, and dynamic `var` output ids — with
`"my_prefix-"`.

The same `ns` must be threaded through three call sites per instance:

- `ptr_input_ui(ns = ns)`
- `ptr_output_ui(ns = ns)`
- `ptr_server_state(..., ns = ns)` (or `ptr_server(..., ns = ns)`)

The `ids =` argument is independent. Sharing the same `custom_ids`
across instances is safe — `ns` is what disambiguates them.

## Example — two tabs, disjoint ids

```r
library(shiny); library(ggpaintr)

ns_a <- shiny::NS("plot_a")
ns_b <- shiny::NS("plot_b")

ui <- fluidPage(
  tabsetPanel(
    tabPanel("A",
      fluidRow(
        column(4, ptr_input_ui(ns = ns_a)),
        column(8, ptr_output_ui(ns = ns_a))
      )),
    tabPanel("B",
      fluidRow(
        column(4, ptr_input_ui(ns = ns_b)),
        column(8, ptr_output_ui(ns = ns_b))
      ))
  )
)

server <- function(input, output, session) {
  ptr_server(
    input, output, session,
    formula = "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    ns      = ns_a
  )
  ptr_server(
    input, output, session,
    formula = "ggplot(data = iris, aes(x = var, y = var)) + geom_point()",
    ns      = ns_b
  )
}

shinyApp(ui, server)
```

## Pattern — inside `shiny::moduleServer()`

Pass `session$ns`. It is a namespace function identical in shape to
`shiny::NS(id)` for the enclosing module id, so the UI (built with
`shiny::NS(id)`) and the server agree. ggpaintr applies `ns` exactly
once to raw ids, so no double-prefixing occurs.

```r
tab_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(ptr_input_ui(ns = ns), ptr_output_ui(ns = ns))
}

tab_server <- function(id, formula) {
  shiny::moduleServer(id, function(input, output, session) {
    ptr_server(input, output, session, formula = formula, ns = session$ns)
  })
}
```

## Notes

- **Failure mode:** reusing the same `ns` (or leaving it default) on
  two instances in one session — the resolved ids collide and Shiny
  silently keeps only the last `output[[id]] <-` assignment. One
  instance appears empty. No error is raised.
- **Sanity check** in the console after wiring:

  ```r
  state_a <- ptr_server_state(formula_a, ns = ns_a)
  state_b <- ptr_server_state(formula_b, ns = ns_b)
  length(intersect(unlist(state_a$ids), unlist(state_b$ids)))  # expect 0
  ```
- **Do not pre-namespace ids yourself.** Pass raw ids (from
  `ptr_build_ids()`) plus `ns =`; ggpaintr applies `ns` once
  internally. Passing already-prefixed ids together with a non-identity
  `ns` double-prefixes.
- `ns` is accepted by: `ptr_server_state()`, `ptr_input_ui()`,
  `ptr_output_ui()`, `ptr_server()`, `ptr_app()`, `ptr_app_bslib()`.
