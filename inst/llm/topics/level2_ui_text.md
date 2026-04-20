# Level 2 — copy overrides with `ui_text`

`ui_text` is the single argument for customising every user-visible
string ggpaintr renders. Pass it to `ptr_server_state()`, `ptr_server()`,
`ptr_app()`, or `ptr_app_bslib()`.

## The four leaf fields

| Field         | Purpose                                           |
|---------------|---------------------------------------------------|
| `label`       | Primary widget label (above the input).           |
| `help`        | Help text rendered below the widget.              |
| `placeholder` | Grey hint inside a text input.                    |
| `empty_text`  | Fallback text when a `var` picker is empty.       |

Unused fields are ignored (e.g. `empty_text` on a `text` widget is
accepted but never rendered).

Per-placeholder support (verified against `R/paintr-ui.R`):

| Placeholder    | label | help | placeholder | empty_text |
|----------------|:-----:|:----:|:-----------:|:----------:|
| `var`          |  ✅   |  ✅  |      —      |     ✅     |
| `text`         |  ✅   |  ✅  |     ✅      |      —     |
| `num`          |  ✅   |  ✅  |      —      |      —     |
| `expr`         |  ✅   |  ✅  |     ✅      |      —     |
| `upload` file  |  ✅   |  —   |      —      |      —     |
| `upload` name  |  ✅   |  ✅  |     ✅      |      —     |

## Six top-level sections

Three chrome sections (**1-to-1** mapping, no cascade):

| Key              | Role                                              |
|------------------|---------------------------------------------------|
| `shell`          | Page `title` and `draw_button` labels.            |
| `upload`         | Paired `file` + `name` widgets for `upload`.      |
| `layer_checkbox` | The per-layer "include this layer" toggle.        |

Three per-placeholder sections (**cascade** — least specific → most):

1. `defaults[[keyword]]`
2. `params[[param_key]][[keyword]]`  (param_key = canonical arg name)
3. `layers[[layer_name]][[keyword]][[param_key]]`

Merging is deep: you can override one `label` without clobbering sibling
`help`.

**Normalization rules:** `colour → color`, `size → linewidth`. Unnamed
positional args resolve under the literal key `__unnamed__` at the
`layers` level.

## Worked example — three scopes, one placeholder

```r
library(ggpaintr)

ui_text <- list(
  defaults = list(
    text = list(label = "Enter text", help = "Generic help")
  ),
  params = list(
    title = list(
      text = list(label = "Plot title", help = "Arg-specific help")
    )
  ),
  layers = list(
    labs = list(
      text = list(
        title = list(
          label       = "labs(title = …)",
          help        = "Layer-specific help",
          placeholder = "e.g. Weight vs MPG"
        )
      )
    )
  )
)
```

Resolution for `labs(title = text)` picks the `layers` entry; for
`geom_text(label = text)` it falls back to `defaults` (no param/layer
match). Inspect with `ptr_resolve_ui_text()`:

```r
ptr_resolve_ui_text("control", keyword = "text",
                    param = "title", layer_name = "labs",
                    ui_text = ui_text)
```

## Chrome example

```r
ui_text <- list(
  shell = list(
    title       = list(label = "My app"),
    draw_button = list(label = "Redraw")
  ),
  upload = list(
    file = list(label = "Pick .csv or .rds"),
    name = list(label = "Name it", placeholder = "e.g. sales",
                help  = "Accepted: .csv, .rds")
  ),
  layer_checkbox = list(label = "Include this layer")
)
```

`shell` only accepts `title` / `draw_button`; `upload` only accepts
`file` / `name`. Any other key at those levels is a validation error.

## Validating a `ui_text` list

Pipe it through `ptr_merge_ui_text(ui_text)`. It validates structure and
leaf-field names (errors), and warns on `params` keys that do not match
any placeholder in a parsed formula (warnings). Returns a `ptr_ui_text`
object recognised downstream so the merge is not repeated.

## Full embed with copy overrides

```r
library(shiny); library(ggpaintr)

ui_text <- list(
  shell  = list(title = list(label = "mtcars explorer"),
                draw_button = list(label = "Redraw")),
  params = list(x = list(var = list(label = "X-axis column"))),
  layers = list(labs = list(text = list(title = list(
    label = "Plot title", placeholder = "e.g. Weight vs MPG"))))
)

ui <- fluidPage(
  titlePanel(ptr_resolve_ui_text("title", ui_text = ui_text)$label),
  sidebarLayout(
    sidebarPanel(ptr_input_ui(ui_text = ui_text)),
    mainPanel(ptr_output_ui())
  )
)

server <- function(input, output, session) {
  ptr_state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) +
       geom_point() + labs(title = text)",
    ui_text = ui_text
  )
  ptr_setup_controls(input, output, ptr_state)
  ptr_register_draw(input, ptr_state)
  ptr_register_plot(output, ptr_state)
  ptr_register_error(output, ptr_state)
  ptr_register_code(output, ptr_state)
}
shinyApp(ui, server)
```
