# Generate a Standalone Shiny App Script

Generate a single-file Shiny app that uses the same `ggpaintr` runtime
helpers as the package.

## Usage

``` r
ptr_generate_shiny(
  ptr_obj,
  output_file,
  style = TRUE,
  ui_text = NULL,
  placeholders = NULL,
  ids = ptr_build_ids()
)
```

## Arguments

- ptr_obj:

  A `ptr_obj`.

- output_file:

  Path to the generated `.R` script.

- style:

  Whether to style the generated file with `styler` when available.

- ui_text:

  Optional named list of copy overrides for UI labels, helper text, and
  placeholders.

- placeholders:

  Optional custom placeholder definitions or an existing placeholder
  registry. Exported custom placeholders must define their hooks inline
  inside
  [`ptr_define_placeholder()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder.md)
  so the generated app can stay standalone.

- ids:

  A `ptr_build_ids` object controlling the Shiny element IDs used in the
  generated app. Defaults to
  [`ptr_build_ids()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_build_ids.md).

## Value

Invisibly returns `output_file`.

## Examples

``` r
obj <- ptr_parse_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
out_file <- tempfile(fileext = ".R")
ptr_generate_shiny(obj, out_file, style = FALSE)
file.exists(out_file)
#> [1] TRUE
```
