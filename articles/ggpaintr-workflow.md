# ggpaintr Workflow

## Overview

`ggpaintr` now focuses on the `ggpaintr` workflow. A plot is described
as one ggplot-like formula string. Placeholder tokens inside that string
are converted into Shiny controls, completed with current inputs,
evaluated into a plot, and rendered with generated code and readable
runtime feedback.

Supported placeholders are:

- `var`
- `text`
- `num`
- `expr`
- `upload`

`upload` currently supports `.csv` and `.rds`.

## Parsing a formula

``` r
library(ggpaintr)
library(ggplot2)

# Each placeholder token in the formula becomes a Shiny input widget:
#   var   → selectInput  (column chooser; populated from the active dataset)
#   num   → numericInput
#   text  → textInput    (value used as a string literal)
#   expr  → textInput    (value is parsed as an R expression at runtime)
#   upload → fileInput + textInput (upload a .csv or .rds and name it)
obj <- ptr_parse_formula(
  "ggplot(data = iris, aes(x = var, y = var)) +
    geom_point(aes(color = var), size = num) +
    labs(title = text) +
    facet_wrap(expr)"
)

names(obj$expr_list)          # one entry per ggplot layer
#> [1] "ggplot"     "geom_point" "labs"       "facet_wrap"
names(obj$keywords_list$ggplot)  # placeholder locations inside the ggplot() call
#> [1] "ggplot+3+2" "ggplot+3+3"
```

## Building runtime output

``` r
# ptr_runtime_input_spec() returns a data frame describing every input that
# ptr_exec() expects. Key columns:
#   input_id   — the Shiny input id string (treat as opaque; discover via spec)
#   role       — "layer_checkbox" (toggle a layer on/off) or "placeholder" (a widget value)
#   layer_name — the ggplot layer this input belongs to (e.g. "geom_point")
#   param_key  — the ggplot argument name (e.g. "x", "color"); NA for unnamed placeholders
#   keyword    — the placeholder type ("var", "num", "text", "expr")
spec <- ptr_runtime_input_spec(obj)
spec
#>              input_id           role layer_name keyword   param_key
#> 1          ggplot+3+2    placeholder     ggplot     var           x
#> 2          ggplot+3+3    placeholder     ggplot     var           y
#> 3      geom_point+2+2    placeholder geom_point     var       color
#> 4        geom_point+3    placeholder geom_point     num   linewidth
#> 5              labs+2    placeholder       labs    text       title
#> 6        facet_wrap+2    placeholder facet_wrap    expr __unnamed__
#> 7 geom_point+checkbox layer_checkbox geom_point    <NA>        <NA>
#> 8       labs+checkbox layer_checkbox       labs    <NA>        <NA>
#> 9 facet_wrap+checkbox layer_checkbox facet_wrap    <NA>        <NA>
#>        source_id
#> 1     ggplot+3+2
#> 2     ggplot+3+3
#> 3 geom_point+2+2
#> 4   geom_point+3
#> 5         labs+2
#> 6   facet_wrap+2
#> 7           <NA>
#> 8           <NA>
#> 9           <NA>

# Build a named list keyed by input_id — the same shape as Shiny's input object.
inputs <- setNames(vector("list", nrow(spec)), spec$input_id)

# layer_checkbox inputs control whether a layer is included in the plot at all.
# Set them all TRUE here to render every layer.
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))

# For each placeholder, look up its input_id by filtering spec on layer_name + param_key
# (or keyword for unnamed placeholders like num/expr), then assign the desired value.
inputs[[spec$input_id[spec$layer_name == "ggplot"    & spec$param_key == "x"]]]      <- "Sepal.Length"
inputs[[spec$input_id[spec$layer_name == "ggplot"    & spec$param_key == "y"]]]      <- "Sepal.Width"
inputs[[spec$input_id[spec$layer_name == "geom_point" & spec$param_key == "color"]]] <- "Species"
# num has no param_key (it fills the `size` argument positionally); filter by keyword
inputs[[spec$input_id[spec$layer_name == "geom_point" & spec$keyword == "num"]]]     <- 2.5
inputs[[spec$input_id[spec$layer_name == "labs"       & spec$param_key == "title"]]] <- "Iris scatter"
# expr values are strings; ptr_exec() parses them into R expressions internally
inputs[[spec$input_id[spec$layer_name == "facet_wrap" & spec$keyword == "expr"]]]    <- "~ Species"

runtime <- ptr_exec(obj, inputs)

runtime$code_text               # completed ggplot call as a formatted string
#> NULL
inherits(runtime$plot, "ggplot") # TRUE when the plot rendered without error
#> [1] FALSE
```

[`ptr_runtime_input_spec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_runtime_input_spec.md)
is the supported way to discover the low-level runtime inputs consumed
by
[`ptr_exec()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_exec.md).
Raw ids still appear in the returned data, but they should be treated as
implementation details rather than as hand-authored examples.

## Launching an app

``` r
ptr_app("
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

The generated app:

- renders one control tab per layer
- delays `var` selectors until upload-backed data is available
- clears the plot on failure
- shows `Input error:` and `Plot error:` messages inline
- preserves generated code when completion succeeds but plotting fails

## Exporting a standalone app

``` r
out_file <- tempfile(fileext = ".R")
# style = FALSE skips styler formatting of the generated code (faster; useful
# in automated contexts). Set style = TRUE to produce a cleanly formatted app.R.
ptr_generate_shiny(obj, out_file, style = FALSE)
file.exists(out_file)
#> [1] TRUE
```

## Current behavior boundary

- Structural parse failures still stop the app before launch.
- `var` placeholders still require a data source during UI preparation.
- Missing local data objects such as `unknown_object` are deferred to
  draw time.
- Render-time ggplot failures such as missing faceting variables are
  surfaced by the same runtime error path as other plot failures.

For recipes that embed `ggpaintr` into an existing Shiny app with custom
ids or custom plot rendering, see
[`vignette("ggpaintr-extensibility")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-extensibility.md).
