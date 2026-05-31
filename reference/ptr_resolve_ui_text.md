# Resolve copy for one ggpaintr control or app element

Looks up the effective label/help/placeholder for a single UI element,
applying the `defaults -> params -> layers` specificity chain and
interpolating the `{param}` / `{layer}` tokens. Placeholder authors can
call this inside a custom `build_ui` hook so their control is labelled
through the same override chain as the built-in controls.

## Usage

``` r
ptr_resolve_ui_text(
  component,
  keyword = NULL,
  param = NULL,
  layer_name = NULL,
  ui_text = NULL
)
```

## Arguments

- component:

  One of `title`, `draw_button`, `draw_all_button`, `layer_picker`,
  `data_subtab`, `controls_subtab`, `upload_file`, `upload_name`,
  `layer_checkbox`, or `control` (for a placeholder control, in which
  case `keyword` is required).

- keyword:

  Placeholder keyword (e.g. `"ppVar"`, `"ppNum"`); required when
  `component = "control"`.

- param:

  Optional parameter / aesthetic name (e.g. `"x"`); only used when
  `component = "control"`.

- layer_name:

  Optional layer name (e.g. `"facet_wrap"`); only used when
  `component = "control"`, to pick up a `layers$<layer>$...` override.

- ui_text:

  `NULL`, a list of overrides, or an already-merged `ptr_ui_text` object
  (see
  [`ptr_ui_text()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_text.md)).

## Value

A named list with `label`, `help`, `placeholder`, and `empty_text`.

## Examples

``` r
# Resolve copy for the title element
ptr_resolve_ui_text("title")
#> $label
#> [1] "ggpaintr Plot Builder"
#> 

# Resolve copy for a var control on the x-axis
ptr_resolve_ui_text("control", keyword = "ppVar", param = "x")
#> $label
#> [1] "Choose the x-axis column"
#> 
#> $empty_text
#> [1] "Choose one column"
#> 

# Inside a custom `build_ui` hook, label the control through the same
# override chain ggpaintr uses for built-in controls:
my_build_ui <- function(node, label, ...) {
  copy <- ptr_resolve_ui_text(
    "control",
    keyword = node$keyword,
    param = node$param,
    ui_text = list(...)$ui_text
  )
  shiny::textInput(node$id, label = copy$label %||% label)
}
```
