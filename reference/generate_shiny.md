# Generate a Standalone Shiny App Script

Generate a single-file Shiny app that uses the same `ggpaintr` runtime
helpers as the package.

## Usage

``` r
generate_shiny(paintr_obj, var_ui, output_file, style = TRUE)
```

## Arguments

- paintr_obj:

  A `paintr_obj`.

- var_ui:

  Deprecated legacy argument retained for backward compatibility. It is
  ignored.

- output_file:

  Path to the generated `.R` script.

- style:

  Whether to style the generated file with `styler` when available.

## Value

Invisibly returns `output_file`.

## Examples

``` r
obj <- paintr_formula(
  "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
)
out_file <- tempfile(fileext = ".R")
generate_shiny(obj, list(), out_file, style = FALSE)
file.exists(out_file)
#> [1] TRUE
```
