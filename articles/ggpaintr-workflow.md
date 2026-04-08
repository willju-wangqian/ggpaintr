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

obj <- paintr_formula(
  "ggplot(data = iris, aes(x = var, y = var)) +
    geom_point(aes(color = var), size = num) +
    labs(title = text) +
    facet_wrap(expr)"
)

names(obj$expr_list)
#> [1] "ggplot"     "geom_point" "labs"       "facet_wrap"
names(obj$keywords_list$ggplot)
#> [1] "ggplot+2"   "ggplot+3+2" "ggplot+3+3"
```

## Building runtime output

``` r
spec <- ggpaintr_runtime_input_spec(obj)
spec
#>              input_id           role layer_name keyword   param_key
#> 1          ggplot+3+2    placeholder     ggplot     var           x
#> 2          ggplot+3+3    placeholder     ggplot     var           y
#> 3      geom_point+2+2    placeholder geom_point     var       color
#> 4        geom_point+3    placeholder geom_point     num        size
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

inputs <- setNames(vector("list", nrow(spec)), spec$input_id)
inputs[spec$role == "layer_checkbox"] <- rep(list(TRUE), sum(spec$role == "layer_checkbox"))
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "x"]]] <- "Sepal.Length"
inputs[[spec$input_id[spec$layer_name == "ggplot" & spec$param_key == "y"]]] <- "Sepal.Width"
inputs[[spec$input_id[spec$layer_name == "geom_point" & spec$param_key == "color"]]] <- "Species"
inputs[[spec$input_id[spec$layer_name == "geom_point" & spec$keyword == "num"]]] <- 2.5
inputs[[spec$input_id[spec$layer_name == "labs" & spec$param_key == "title"]]] <- "Iris scatter"
inputs[[spec$input_id[spec$layer_name == "facet_wrap" & spec$keyword == "expr"]]] <- "~ Species"

runtime <- paintr_build_runtime(
  obj,
  inputs
)

runtime$code_text
#> NULL
inherits(runtime$plot, "ggplot")
#> [1] FALSE
```

[`ggpaintr_runtime_input_spec()`](https://willju-wangqian.github.io/ggpaintr/reference/ggpaintr_runtime_input_spec.md)
is the supported way to discover the low-level runtime inputs consumed
by
[`paintr_build_runtime()`](https://willju-wangqian.github.io/ggpaintr/reference/paintr_build_runtime.md).
Raw ids still appear in the returned data, but they should be treated as
implementation details rather than as hand-authored examples.

## Launching an app

``` r
ggpaintr_app("
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
generate_shiny(obj, list(), out_file, style = FALSE)
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
