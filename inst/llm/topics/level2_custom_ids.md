# Level 2 — id collisions and the `ptr_` reserved namespace

Use when: a ggpaintr embed lives in a Shiny host that already uses
overlapping input/output ids, or when the user wants to read or
override a specific ggpaintr-generated id.

There is **no `ptr_build_ids()` helper.** Top-level output ids are
package-owned; the way to disambiguate ggpaintr ids from host ids is the
module `id` you already pass to `ptr_module_ui()` / `ptr_module_server()`
(or `ptr_controls_ui()` / `ptr_outputs_ui()` / a `moduleServer()`
wrapping `ptr_server()`).

## What `id` namespaces

Every Shiny input and output ggpaintr emits — top-level outputs, the
draw button, the layer picker / tabset, per-layer checkboxes, and
per-placeholder input ids — is rendered under the module `id`, i.e.
the namespace function `shiny::NS(id)`. Different `id`s never collide.

```r
ui <- fluidPage(
  tabsetPanel(
    tabPanel("A", ptr_module_ui("plot_a", formula_a)),
    tabPanel("B", ptr_module_ui("plot_b", formula_b))
  )
)
server <- function(input, output, session) {
  ptr_module_server("plot_a", formula_a)
  ptr_module_server("plot_b", formula_b)
}
```

`plot_a-ptr_plot`, `plot_b-ptr_plot`, etc. — guaranteed disjoint.

## Top-level package-owned ids

Inside any module namespace, the framework writes to a fixed set of
`ptr_`-prefixed ids:

| Id                  | Kind                 | Role                              |
|---------------------|----------------------|-----------------------------------|
| `ptr_plot`          | `plotOutput`         | rendered plot                     |
| `ptr_error`         | `uiOutput`           | inline error display              |
| `ptr_code`          | `verbatimTextOutput` | generated R code                  |
| `ptr_update_plot`   | `actionButton`       | "Update plot" trigger             |
| `ptr_layer_select`  | `pickerInput`        | layer picker (internal nav)       |
| `ptr_layer_tabset`  | `tabsetPanel`        | hidden tabset (internal nav)      |

These are **not user-configurable**. Treat the whole `ptr_` prefix
as reserved within any module namespace.

## Per-placeholder input ids

Generated input ids follow `<layer>_<path>_<keyword>_<shared-or-NA>`,
where `<path>` is the underscore-joined positional index path into the
call. Examples:

| Formula fragment                          | Resulting input id                |
|-------------------------------------------|-----------------------------------|
| `ggplot(aes(x = var, y = var))`           | `ggplot_1_1_var_NA`, `ggplot_1_2_var_NA` |
| `var(shared = "x_col")` inside `ggplot`   | `ggplot_1_1_var_x_col`            |
| second `geom_point(...)` in formula       | layer name `geom_point-2`         |
| `<layer>_checkbox`                        | layer-include toggle              |
| `<layer>_<path>_stage_enabled`            | pipeline stage on/off toggle      |

Layer names repeat with `-2`, `-3`, … suffixes when a layer is reused.
Inputs are deterministic, so the same formula always produces the same
ids — useful for tests, snapshot fixtures, or targeted control-panel
overrides.

## Reading or driving generated ids

For host code that needs to read or override one:

```r
ns <- shiny::NS("p")     # the module id you used
input[[ns("ggplot_1_1_var_NA")]]   # current "x" picker value
updateNumericInput(session, ns("geom_point_2_size_NA"), value = 5)
```

Always reach for `ns("<raw_id>")` — Shiny does the prefixing once. Do
not hand-write `"p-ggplot_…"`.

## Best practice: namespace each embed

The cleanest fix for any id worry is to make sure every ggpaintr embed
gets its own unique module `id` (and never a literal that the host app
itself uses elsewhere). Once embeds are namespaced, the only ids the
host has to avoid are the package-owned `ptr_` ones at the **top
level** of the same Shiny session.
