# ggpaintr

## Overview

ggpaintr turns a ggplot-like formula string into a running Shiny app.
You write a single
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
as text, drop placeholder keywords (`ppVar`, `ppText`, `ppNum`,
`ppExpr`, `ppUpload`) anywhere a value would normally go, and ggpaintr
does the rest: each keyword becomes an input widget, the same parsed
object drives the UI, the plot, and a live code pane, and editing any
widget re-renders the plot.

No Shiny UI or server code required. If you can write a ggplot call, you
can ship an interactive version of it.

## Installation

``` r

# Install the development version from GitHub:
# install.packages("pak")
pak::pkg_install("willju-wangqian/ggpaintr")
```

## Usage

``` r

library(ggpaintr)

ptr_app("
ggplot(data = iris, aes(x = ppVar, y = ppVar)) +
  geom_point(aes(color = ppVar), size = ppNum) +
  labs(title = ppText) +
  facet_wrap(ppExpr)
")
```

That single call returns a running Shiny app. Each placeholder in the
formula becomes one widget:

- the three `ppVar` tokens → column pickers populated from `iris`,
- `ppNum` → a numeric input (point size),
- `ppText` → a text input (plot title),
- `ppExpr` → a code box for the facet spec (e.g. `vars(Species)`).

[`library(ggpaintr)`](https://willju-wangqian.github.io/ggpaintr/) also
attaches `ggplot2`, so bare
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) /
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) / `geom_*()`
calls work directly inside formula strings. For a grid layout, use
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md).
To swap in a custom page shell or theme, write a thin wrapper on top of
the public primitives.

## More topics

- **[`vignette("ggpaintr-tutorial")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-tutorial.md)
  — *Tutorial*.** A guided walk from a one-line
  [`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  formula, through defining your own placeholders (every argument of the
  three constructors
  [`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
  / `_consumer()` / `_source()`), to embedding several plots in your own
  Shiny app with
  [`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
  /
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md)
  and sharing one control across them via
  [`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  /
  [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
  /
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md).
  Opens with a tour of the five built-in placeholder keywords.

- **[`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md)
  — *Safety*.** What `expr_check` does and when (never) to turn it off,
  the denylist + AST-walker safety model, and the upload trust boundary.

- **[`vignette("ggpaintr-llm")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-llm.md)
  — *Using ggpaintr from an LLM with ellmer*.** Wire ggpaintr up as an
  LLM-callable tool with ellmer: register the primer and the
  topic-lookup tool, inspect what the model sees, swap providers/models,
  and test without spending tokens.

The [pkgdown reference](https://willju-wangqian.github.io/ggpaintr/)
lists every exported function. A longer, book-length introduction to
ggpaintr is planned.
