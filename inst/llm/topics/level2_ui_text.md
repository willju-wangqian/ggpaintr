# Level 2 — copy overrides with `ui_text`

`ui_text` is the single argument for customising every user-visible string ggpaintr renders. Pass it to `ptr_app()`, `ptr_app_bslib()`, `ptr_app_grid()`, `ptr_ui()`, `ptr_server()`, `ptr_init_state()`, the L3 bare piece `ptr_ui_controls()`, or the coordinator `ptr_shared()` (which threads it into the shared panel and each module). The cascade keys are the **placeholder keywords** (`ppVar`, `ppText`, …).

## The four leaf fields

| Field         | Purpose                                           |
|---------------|---------------------------------------------------|
| `label`       | Primary widget label (above the input).           |
| `help`        | Help text rendered below the widget.              |
| `placeholder` | Grey hint inside a text input.                    |
| `empty_text`  | Fallback text when a `ppVar` picker is empty.     |

Unused fields are ignored (e.g. `empty_text` on a `ppText` widget is accepted but never rendered).

## Top-level sections

Chrome sections (**1-to-1** mapping, no cascade):

| Key                            | Role                                              |
|--------------------------------|---------------------------------------------------|
| `shell$title`                  | Page title.                                       |
| `shell$draw_button`            | "Update plot" button.                             |
| `shell$draw_all_button`        | Cross-formula "Draw all" button.                  |
| `shell$layer_picker`           | Layer-select dropdown.                            |
| `shell$data_subtab`            | "Data" subtab label inside a layer panel.         |
| `shell$controls_subtab`        | "Controls" subtab label inside a layer panel.     |
| `upload$file`                  | `ppUpload` keyword's file picker.                 |
| `upload$name`                  | `ppUpload` keyword's "Optional dataset name" input. |
| `layer_checkbox`               | Per-layer "include this layer" toggle.            |

Three per-placeholder sections (**cascade** — least specific → most), keyed by keyword:

1. `defaults[[keyword]]`
2. `params[[param_key]][[keyword]]`  (param_key = canonical arg name)
3. `layers[[layer_name]][[keyword]][[param_key]]`

Merging is deep: you can override one `label` without clobbering sibling `help`.

**Normalization rules:** `colour → color`, `size → linewidth`. Unnamed positional args resolve under the literal key `__unnamed__` at the `layers` level.

## Worked example — three scopes, one placeholder

```r
library(ggpaintr)

ui_text <- list(
  defaults = list(
    ppText = list(label = "Enter text", help = "Generic help")
  ),
  params = list(
    title = list(
      ppText = list(label = "Plot title", help = "Arg-specific help")
    )
  ),
  layers = list(
    labs = list(
      ppText = list(
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

Resolution for `labs(title = ppText)` picks the `layers` entry; for `geom_text(label = ppText)` it falls back to `defaults` (no param/layer match). Inspect with `ptr_resolve_ui_text()`:

```r
ptr_resolve_ui_text("control", keyword = "ppText",
                    param = "title", layer_name = "labs",
                    ui_text = ui_text)
```

`component = "control"` resolves the placeholder cascade above; other component names (`"title"`, `"draw_button"`, `"layer_checkbox"`, `"upload_file"`, `"upload_name"`, etc.) read directly from the chrome paths.

## Chrome example

```r
ui_text <- list(
  shell = list(
    title       = list(label = "My app"),
    draw_button = list(label = "Redraw")
  ),
  upload = list(
    file = list(label = "Pick a data file"),
    name = list(label = "Name it", placeholder = "e.g. sales",
                help  = "Accepted: .csv, .tsv, .rds, .xlsx, .xls, .json")
  ),
  layer_checkbox = list(label = "Include this layer")
)
```

Any leaf field name outside the four allowed (`label`, `help`, `placeholder`, `empty_text`) is a validation error.

## Validating a `ui_text` list

`ptr_ui_text(ui_text)` runs the validator, normalizes aliases, deep-merges your overrides over the defaults, and returns a `ptr_ui_text` object. Pass it through if you want to inspect the merged tree or surface errors ahead of time; downstream calls also accept the bare `list` and run the same validation themselves, so this is optional.

## Full embed with copy overrides

```r
library(shiny); library(ggpaintr)

ui_text <- list(
  shell  = list(title = list(label = "mtcars explorer"),
                draw_button = list(label = "Redraw")),
  params = list(x = list(ppVar = list(label = "X-axis column"))),
  layers = list(labs = list(ppText = list(title = list(
    label = "Plot title", placeholder = "e.g. Weight vs MPG"))))
)

formula <- "ggplot(data = mtcars, aes(x = ppVar, y = ppVar)) +
              geom_point() + labs(title = ppText)"

ui <- fluidPage(
  titlePanel(ptr_resolve_ui_text("title", ui_text = ui_text)$label),
  ptr_ui(formula, "p", ui_text = ui_text)
)

server <- function(input, output, session) {
  ptr_server(formula, "p", ui_text = ui_text)
}

shinyApp(ui, server)
```

Pass the same `ui_text` to both the UI and the server — `ptr_ui()` builds the widget labels from it; `ptr_server()` forwards it to `ptr_init_state()` so any server-side renders (validation messages, dynamic widget refreshes) read the same merged tree. At L3 the same `ui_text` goes to `ptr_ui_controls(formula, id, ui_text = ...)` and the matching `ptr_server(..., ui_text = ...)`; for multi-instance shared widgets, set it once on `ptr_shared(formulas, ui_text = ...)`.
