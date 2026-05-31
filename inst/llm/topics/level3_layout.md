# Level 3 — own the layout: bare pieces, combinators, the optional shell

Use when: the user wants more than ggpaintr's default-layout block — every pane hand-placed in its own region, a custom page root, or the familiar slide-out code window inside their own layout. (Owning the *render path* — your own Plotly/ggiraph renderer — is the sibling topic `level3_custom_render`.)

At L3 every piece of ggpaintr's UI has its own **bare** exported function (the suffixed `ptr_ui_<x>` half of the naming convention — emits only its widgets, no wrapper, no assets). Pick the pieces you want, place each anywhere in your own Shiny. The server contract is unchanged from L2: the pieces write to / read from the same canonical ids `ptr_server()` targets, so a piece you never place is simply a no-op.

## The bare pieces and combinators

The pieces are **truly orthogonal** — they take an `id` (and, where relevant, a `formula`) and nothing else. There are **no** `error=` / `code_toggle=` flags on `ptr_ui_plot()`; layered behaviour is added by **combinators** that wrap already-built pieces.

| Function | What it emits | Key arguments |
|---|---|---|
| `ptr_ui_page(…, page, css)` | Optional page shell: a Bootstrap-3 page + the single `.ptr-app` theme scope + the deduped asset bundle, wrapping the pieces you pass in `…` | `page` (default `shiny::fluidPage`), `css` |
| `ptr_ui_header(title)` | The branded header bar (logo + title) | `title` (default `"ggpaintr"`) |
| `ptr_ui_controls(formula, id, …)` | Generated control widgets (layer picker, per-layer panels, *Update plot*, inline shared section) | `formula`, `id`, `ui_text`, `expr_check`, `shared` |
| `ptr_ui_plot(id)` | The bare plot card (`ptr_plot`) — no error slot, no toggle | `id` |
| `ptr_ui_error(id)` | The bare inline error slot (`ptr_error`) | `id` |
| `ptr_ui_code(id, style)` | The bare generated-code pane (`ptr_code`) | `style = "panel"` (plain, always visible — default) or `"window"` (slide-out chrome, only meaningful via the toggle combinator) |
| `ptr_ui_inline_error(plot, error)` | **Combinator**: nests an error piece inside a plot piece's card body | `plot`, `error` |
| `ptr_ui_toggle_code(plotish, code)` | **Combinator**: wraps a plot-ish piece + code piece into the `</>` slide-out toggle layout | `plotish`, `code` |
| `ptr_ui_shared_panel(obj)` | The bare cross-formula shared panel (L3 counterpart of `ptr_shared_panel()`; no `css`) | `obj` |
| `ptr_ui_assets(css)` | The CSS/JS bundle, **escape hatch only** for roots `ptr_ui_page()` can't cover (`navbarPage`, bslib/BS5) | `css` |

Two facts shape how the bare pieces behave:

1. **`ptr_ui_page()` is the only scaffolding to remember — and it is optional.** ggpaintr's controls use `shinyWidgets::pickerInput()` and the Bootstrap grid, which need Bootstrap's own CSS/JS (Shiny loads those only when the outermost UI object is a Bootstrap page builder). The bundled theme is scoped under a single `.ptr-app`, and the pieces are deliberately bare. `ptr_ui_page()` resolves both at once: it *is* the Bootstrap page and owns the single `.ptr-app` scope + the deduped assets. You may instead write your own bare Shiny — but then you own that scaffolding (the `navbarPage`/bslib recipe below shows exactly what to reproduce).
2. **Combinators add behaviour; flags do not.** A standalone `ptr_ui_code()` is a plain, always-visible panel that needs no wiring. The inline error and the draggable slide-out code window are produced by composing pieces through `ptr_ui_inline_error()` and `ptr_ui_toggle_code()` — pure DOM-structure helpers with **no server coupling** (the server registers `ptr_plot`/`ptr_error`/`ptr_code` regardless). They **nest**: `ptr_ui_toggle_code(ptr_ui_inline_error(ptr_ui_plot(id), ptr_ui_error(id)), ptr_ui_code(id))` is byte-for-byte the output block `ptr_app()` and `ptr_ui()` render internally.

Ids must line up: pass the same `id` to every piece and to `ptr_server(formula, id)`. The single-embedding case needs neither the `id` nor any wrapper. Keep the whole `ptr_`-prefixed top-level id set reserved — see `level2_custom_ids` for the full list and the namespacing fix.

## A fully hand-laid-out page

Header, controls, plot, error, and code each in their own region — bare pieces, no flags:

```r
formula <- "ggplot(iris, aes(x = ppVar, y = ppVar, color = ppVar)) + geom_point()"

ui <- ptr_ui_page(                          # Bootstrap page + single .ptr-app + assets
  ptr_ui_header("Iris explorer"),
  shiny::fluidRow(
    shiny::column(4, ptr_ui_controls(formula = formula)),
    shiny::column(8, ptr_ui_plot())         # bare plot card; error placed below
  ),
  ptr_ui_error(),                           # error banner in its own row
  ptr_ui_code()                             # plain, always-visible code card
)
server <- function(input, output, session) {
  ptr_server(formula)   # binds ptr_plot / ptr_error / ptr_code
}

shiny::shinyApp(ui, server)
```

Swap the page builder with `page =` for a different Bootstrap-3 root — `ptr_ui_page(…, page = shiny::fillPage)` for edge-to-edge, for instance. The contract is *any* BS3 page builder whose `…` are tag children (`fluidPage` (default), `fixedPage`, `fillPage`, `bootstrapPage`, `basicPage`). It is **not** `navbarPage` and **not** a bslib/BS5 page (the bundled CSS is Bootstrap-3-scoped, a bslib/BS5 concern). All three panes are empty until the first *Update plot* click, exactly as in the bundled app. Do not bury `ptr_ui_code()` inside a collapsed `tags$details()`, or only the disclosure summary shows.

## The familiar slide-out code window — the combinator recipe

Want the slide-out code window and inline error while still owning the surrounding layout? Compose the pieces with the two combinators — this nested call is exactly what `ptr_app()` / `ptr_ui()` render internally:

```r
ui <- ptr_ui_page(
  ptr_ui_header("Iris explorer"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(ptr_ui_controls(formula = formula)),
    shiny::mainPanel(
      ptr_ui_toggle_code(                                  # </> slide-out toggle …
        ptr_ui_inline_error(ptr_ui_plot(), ptr_ui_error()),# … around plot + inline error
        ptr_ui_code()                                      # … wrapped as the slide-out window
      )
    )
  )
)
# server unchanged
```

`ptr_ui_inline_error()` nests the error slot in the plot card body; `ptr_ui_toggle_code()` injects the `</>` button and wraps the code piece in the draggable `.ptr-code-window`. The combinators are DOM-only — no extra server wiring; `ptr_server()` already registers all three outputs.

## Custom or `navbarPage` roots — decompose by hand

`ptr_ui_page()` deliberately does **not** cover `navbarPage()` (positional `title` then `tabPanel()` children, not free tags) or bslib/BS5 pages (hand-build a bslib page yourself). For those, hand-build what `ptr_ui_page()` expands to — a Bootstrap page, `ptr_ui_assets()` **once** (its only sanctioned use — the navbar/bslib escape hatch, never part of normal L2/L3 composition), and one `div(class = "ptr-app")` around the pieces:

```r
ui <- shiny::navbarPage(
  "Iris explorer",                            # navbarPage needs a positional title
  shiny::tabPanel(
    "Plot",
    ptr_ui_assets(),                          # the bundle, once (self-deduping) — escape hatch
    shiny::tags$div(
      class = "ptr-app",                      # the single theme scope
      shiny::sidebarLayout(
        shiny::sidebarPanel(ptr_ui_controls(formula = formula)),
        shiny::mainPanel(
          ptr_ui_toggle_code(
            ptr_ui_inline_error(ptr_ui_plot(), ptr_ui_error()),
            ptr_ui_code()
          )
        )
      )
    )
  ),
  shiny::tabPanel("About", "Built with ggpaintr.")
)
# server unchanged
```

This is exactly the body of `ptr_ui_page()` (`page(ptr_ui_assets(css), div(class = "ptr-app", …))`) inlined into the one tab. Keep `ptr_ui_assets()` and the `.ptr-app` wrapper *inside* each tab that hosts ggpaintr pieces; the asset bundle is self-deduping (htmlDependency), so repeating it across tabs costs nothing.

## L3 with shared widgets — default panes

To run multiple linked instances with the bare default panes (not a custom renderer): build `obj` with `ptr_shared()`, place `ptr_ui_shared_panel(obj)` once, give each plot its own `ptr_ui_controls(formula, id, shared = obj)` + bare plot piece, and run one top-level `ptr_shared_server(obj)` threaded into each plot's `ptr_server()`. The panel-owned keys are excluded from each `ptr_ui_controls(formula, id, shared = obj)` automatically.

```r
plots <- list(
  "ggplot(iris, aes(x = ppVar(shared = 'metric'), y = Sepal.Length, fill = Species)) + geom_boxplot()",
  "ggplot(iris, aes(x = ppVar(shared = 'metric'), y = Sepal.Width,  fill = Species)) + geom_violin()"
)

obj <- ptr_shared(formulas = plots)

ui <- ptr_ui_page(
  ptr_ui_shared_panel(obj),
  shiny::fluidRow(
    shiny::column(6, ptr_ui_controls(plots[[1]], "p1", shared = obj), ptr_ui_plot("p1")),
    shiny::column(6, ptr_ui_controls(plots[[2]], "p2", shared = obj), ptr_ui_plot("p2"))
  )
)
server <- function(input, output, session) {
  sh <- ptr_shared_server(obj)
  ptr_server(plots[[1]], "p1", shared_state = sh)
  ptr_server(plots[[2]], "p2", shared_state = sh)
}

shiny::shinyApp(ui, server)
```

`ptr_shared_server(obj)` runs **once at the top level** (never inside `moduleServer()`); its bundle threads into each plot's `ptr_server()` via `shared_state =`. Swap each `ptr_ui_plot(id)` for your own output container to own the render path too — see `level3_custom_render`.
