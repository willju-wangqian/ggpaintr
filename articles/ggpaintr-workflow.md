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
runtime <- paintr_build_runtime(
  obj,
  list(
    "ggplot+3+2" = "Sepal.Length",
    "ggplot+3+3" = "Sepal.Width",
    "ggplot+3+4" = "Species",
    "geom_point+2" = 2.5,
    "labs+2" = "Iris scatter",
    "facet_wrap+2" = "~ Species",
    "geom_point+checkbox" = TRUE,
    "labs+checkbox" = TRUE,
    "facet_wrap+checkbox" = TRUE
  )
)

runtime$code_text
#> [1] "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +\n  geom_point(aes()) +\n  labs(title = \"Iris scatter\") +\n  facet_wrap(~Species)"
inherits(runtime$plot, "ggplot")
#> [1] TRUE
```

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
