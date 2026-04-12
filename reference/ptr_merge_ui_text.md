# Build Effective Copy Rules

Build Effective Copy Rules

## Usage

``` r
ptr_merge_ui_text(ui_text = NULL, placeholders = NULL, known_param_keys = NULL)
```

## Arguments

- ui_text:

  Optional user-supplied rules.

- placeholders:

  Optional custom placeholder definitions or an effective placeholder
  registry.

- known_param_keys:

  Optional character vector of parameter keys present in the formula.
  When supplied, any key in `ui_text$params` that is not in this set
  triggers a
  [`cli::cli_warn()`](https://cli.r-lib.org/reference/cli_abort.html) so
  the user can catch misspellings.

## Value

A `ptr_ui_text` object containing the merged copy rules.

## Examples

``` r
# Default rules
rules <- ptr_merge_ui_text()
rules$shell$title$label
#> [1] "ggpaintr Plot Builder"

# Override the draw button label
rules <- ptr_merge_ui_text(
  ui_text = list(shell = list(draw_button = list(label = "Render")))
)
rules$shell$draw_button$label
#> [1] "Render"
```
