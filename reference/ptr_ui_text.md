# Inspect, validate, and pre-merge ggpaintr UI copy

Returns the effective copy tree ggpaintr uses to label every generated
control. Call it with no arguments to see the current defaults, or pass
`ui_text =` a list of overrides to get back a validated, merged
`ptr_ui_text` object. That object can be reused across
[`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
/
[`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
calls — those entry points short-circuit when handed an already-merged
`ptr_ui_text`, so the overrides are validated once.

## Usage

``` r
ptr_ui_text(ui_text = NULL)
```

## Arguments

- ui_text:

  `NULL` (return defaults), a named list of overrides, or an
  already-merged `ptr_ui_text` object (returned unchanged).

## Value

A `ptr_ui_text` object containing the merged copy rules.

## Details

Use it to (1) discover the override schema (`names(ptr_ui_text())` and
the section below), and (2) fail fast on a malformed override list
before launching an app — `ptr_ui_text()` raises on unknown sections,
keywords, or leaf fields.

## UI text schema

Override lists mirror the structure of `ptr_ui_text()`. Recognised paths
(every leaf is a named list of the fields below):

- `shell$title$<leaf>`

- `shell$draw_button$<leaf>`

- `shell$draw_all_button$<leaf>`

- `shell$layer_picker$<leaf>`

- `shell$data_subtab$<leaf>`

- `shell$controls_subtab$<leaf>`

- `upload$file$<leaf>`

- `upload$name$<leaf>`

- `layer_checkbox$<leaf>`

- `defaults$<keyword>$<leaf>` — per placeholder keyword (`var`, `text`,
  `num`, `expr`, `upload`, ...)

- `params$<param>$<keyword>$<leaf>` — per aesthetic/argument name (`x`,
  `y`, `color`, ...); aliases (`colour`, `size`) are normalized

- `layers$<layer_name>$<keyword>$<param>$<leaf>` — per layer override
  (use `__unnamed__` as `<param>` for positional arguments)

Leaf fields are `label`, `help`, `placeholder`, and `empty_text`. Leaf
strings may use the `{param}` and `{layer}` tokens, which are
interpolated at resolve time.

## Examples

``` r
# Default rules
rules <- ptr_ui_text()
rules$shell$title$label
#> [1] "ggpaintr Plot Builder"

# Override the draw button label
rules <- ptr_ui_text(
  ui_text = list(shell = list(draw_button = list(label = "Render")))
)
rules$shell$draw_button$label
#> [1] "Render"
```
