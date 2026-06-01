# Level 2 — multiple instances and the shared coordinator trio

Use when: more than one ggpaintr instance lives in the same Shiny session — two tabs, side-by-side comparison, multi-plot dashboard — and (optionally) one widget should drive several of them.

There is **no `ns =` argument** on Level-2 helpers. Namespacing happens through the **`id`** you pass to `ptr_ui()` / `ptr_server()`. Each `id` is its own namespace; widget ids in different modules never collide.

## Disjoint embeds — different `id`s, nothing shared

```r
library(shiny); library(ggpaintr)

ui <- fluidPage(
  tabsetPanel(
    tabPanel("A", ptr_ui(
      "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()", "plot_a"
    )),
    tabPanel("B", ptr_ui(
      "ggplot(iris, aes(x = ppVar, y = ppVar)) + geom_point()", "plot_b"
    ))
  )
)

server <- function(input, output, session) {
  ptr_server("ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()", "plot_a")
  ptr_server("ggplot(iris, aes(x = ppVar, y = ppVar)) + geom_point()", "plot_b")
}

shinyApp(ui, server)
```

Two formulas, two ids, two independent widget sets. The `id` does the namespacing — nothing else to wire when nothing is shared.

## Sharing widgets across instances — the coordinator trio

When two or more embeds need one widget to drive several of them, build the **coordinator** once and feed it to the consumers. The coordinator is a pure, non-reactive object built from the full formula set; it computes the per-key **partition** and is the single source of truth, so UI and server can never disagree.

- `ptr_shared(formulas, id = NULL, ui_text = NULL, expr_check = TRUE, draw_all_label = "Draw all")` → `obj`. Pass the **same list of formula strings** you hand to the modules. There is **no `shared_ui` argument** — a built-in shared key (`ppVar`/`ppNum`/`ppText`/…) auto-builds its widget; to customise the look, register a custom placeholder whose `build_ui` draws the control you want (see below). `obj` carries the computed partition (`obj$panel_keys`, `obj$local_keys_by_formula`).
- `ptr_shared_panel(obj, css = NULL)` — the **one** standalone panel, holding exactly `obj`'s cross-formula keys. Self-contained (owns its `.ptr-app` scope + assets); place it once, anywhere on the page. `css =` is the L2 restyle hook.
- `ptr_shared_server(obj)` — call **once at the top level** of your server (never inside `moduleServer()`). Returns a `ptr_shared_state` bundle.
- `ptr_ui(formula, id, shared = obj)` / `ptr_server(formula, id, shared_state = <bundle>)` — pass the coordinator to each per-plot UI (`shared =`) and the bundle to each per-plot server (`shared_state =`). Each module renders its own formula-local shared keys inline and reads the panel keys from the bundle.

```r
library(shiny); library(ggpaintr)

# Custom consumer placeholder: render the shared metric picker as a selectInput.
ppMetric <- ptr_define_placeholder_consumer(
  keyword        = "ppMetric",
  build_ui       = function(node, cols = character(), label = NULL,
                            selected = character(0), ...) {
    selectInput(node$id, label = label %||% "Metric", choices = cols,
                selected = if (length(selected)) selected[[1L]] else NULL)
  },
  resolve_expr   = function(value, node, ...) {
    if (length(value) == 0L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  parse_positional_arg = ptr_arg_symbol_or_string()
)

plots <- list(
  "ggplot(iris, aes(x = ppMetric(shared = 'metric'), y = Sepal.Length, fill = Species)) + geom_boxplot()",
  "ggplot(iris, aes(x = ppMetric(shared = 'metric'), y = Sepal.Width,  fill = Species)) + geom_violin()"
)

obj <- ptr_shared(formulas = plots)

ui <- fluidPage(
  titlePanel("My host app"),
  ptr_shared_panel(obj),
  fluidRow(
    column(6, ptr_ui(plots[[1]], "plot_1", shared = obj)),
    column(6, ptr_ui(plots[[2]], "plot_2", shared = obj))
  )
)

server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "plot_1", shared_state = sh)
  ptr_server(plots[[2]], "plot_2", shared_state = sh)
}

shinyApp(ui, server)
```

`ptr_shared_panel()` emits a **Draw all** button (top-level input id `ptr_shared_draw_all`) whenever the coordinator spans two or more formulas; the per-module **Update plot** buttons still work independently. Here `metric` is used in both formulas, so the partition rule sends it to the standalone panel.

## The partition rule in action

A shared key referenced in **exactly one** formula stays inline in *that* module's shared section; a key referenced in **two or more** goes to the standalone panel. A built-in `ppVar`/`ppNum` shared key needs no custom registration:

```r
plots <- list(
  "ggplot(iris, aes(x = ppVar(shared = 'ax1'), y = ppVar(shared = 'ax1'),
                    color = Species)) + geom_point(size = ppNum(shared = 'sz'))",
  "ggplot(iris, aes(x = ppVar(shared = 'ax2'), y = Sepal.Width,
                    color = Species)) + geom_point(size = ppNum(shared = 'sz'))"
)

obj <- ptr_shared(formulas = plots)
# sz → both formulas → standalone panel.  ax1 → only plot_1's inline section.
# ax2 → only plot_2's inline section.
obj$panel_keys                       # "sz"

ui <- fluidPage(
  ptr_shared_panel(obj),             # holds sz only
  fluidRow(
    column(6, ptr_ui(plots[[1]], "plot_1", shared = obj)),  # ax1 inline here
    column(6, ptr_ui(plots[[2]], "plot_2", shared = obj))   # ax2 inline here
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "plot_1", shared_state = sh)
  ptr_server(plots[[2]], "plot_2", shared_state = sh)
}

shinyApp(ui, server)
```

## When to skip the coordinator

`ptr_shared()` errors if no formula in `formulas` declares a `shared = "..."` annotation — drop the coordinator and use plain `ptr_ui()` / `ptr_server()` with distinct `id`s if nothing is shared. A **single** instance never builds the coordinator either: its shared keys are formula-local by definition and auto-render inline (that is L1 `ptr_app()` or one `ptr_ui()` / `ptr_server()` block — see `level2_module`).

## Failure modes

- **Same `id` on two instances.** Shiny silently keeps only the last `output[[id]] <-` assignment; one instance appears empty, no error. Always give each module a distinct literal `id`.
- **`shared = "..."` declared, no coordinator wired.** `ptr_server()` aborts with a clear "build the coordinator with `ptr_shared()` / `ptr_shared_server()`" message.
- **`ptr_shared_server()` called inside `moduleServer()`.** It must run once at the **top level** of the server; the panel uses global, un-namespaced ids and N copies collide.
- **A custom shared `build_ui` produces the wrong widget kind.** The runtime validates the resolved value against each downstream placeholder type — e.g. a slider feeding a `ppVar` consumer surfaces as an inline validation error.

For the same coordinator feeding **custom** renderers (Plotly / ggiraph), see `level3_custom_render`; for the bare-piece (hand-placed) layout with shared widgets, see `level3_layout`.
