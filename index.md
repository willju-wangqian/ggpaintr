# ggpaintr

## Overview

ggpaintr turns a ggplot-like formula string into a running Shiny app.
You write a single
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) call
as text, drop placeholder keywords (`var`, `text`, `num`, `expr`,
`upload`) anywhere a value would normally go, and ggpaintr does the
rest: each keyword becomes an input widget, the same parsed object
drives the UI, the plot, and a live code pane, and editing any widget
re-renders the plot.

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
ggplot(data = iris, aes(x = var, y = var)) +
  geom_point(aes(color = var), size = num) +
  labs(title = text) +
  facet_wrap(expr)
")
```

That single call returns a running Shiny app. Each placeholder in the
formula becomes one widget:

- the three `var` tokens → column pickers populated from `iris`,
- `num` → a numeric input (point size),
- `text` → a text input (plot title),
- `expr` → a code box for the facet spec (e.g. `vars(Species)`).

[`library(ggpaintr)`](https://willju-wangqian.github.io/ggpaintr/) also
attaches `ggplot2`, so bare
[`ggplot()`](https://ggplot2.tidyverse.org/reference/ggplot.html) /
[`aes()`](https://ggplot2.tidyverse.org/reference/aes.html) / `geom_*()`
calls work directly inside formula strings. For a grid layout, use
[`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md).
To swap in a custom page shell or theme, write a thin wrapper on top of
the public primitives — see
[`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
§ “Writing your own wrapper”.

## More topics

- **[`vignette("ggpaintr-use-cases")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-use-cases.md)
  — *Use cases*.** The three-level ladder for how to use ggpaintr: L1
  all-in-one
  ([`ptr_app()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app.md)
  /
  [`ptr_app_grid()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_app_grid.md)),
  L2 embed in your own Shiny app with the self-contained pair
  ([`ptr_ui()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui.md)
  /
  [`ptr_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_server.md))
  plus the shared-coordinator trio
  ([`ptr_shared()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared.md)
  /
  [`ptr_shared_panel()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_panel.md)
  /
  [`ptr_shared_server()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_shared_server.md))
  for cross-plot widgets, and L3 own every piece of the UI — bare
  builders
  ([`ptr_ui_header()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_header.md)
  /
  [`ptr_ui_controls()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_controls.md)
  /
  [`ptr_ui_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_plot.md)
  /
  [`ptr_ui_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_error.md)
  /
  [`ptr_ui_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_code.md)
  /
  [`ptr_ui_assets()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_assets.md))
  plus the nestable combinators
  ([`ptr_ui_inline_error()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_inline_error.md)
  /
  [`ptr_ui_toggle_code()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_toggle_code.md))
  and the optional
  [`ptr_ui_page()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_ui_page.md)
  shell, including custom rendering off the returned `state` via
  [`ptr_extract_plot()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_extract.md)
  / `_code()` / `_error()`. Opens with a compact tour of the 5
  placeholder keywords.

- **[`vignette("ggpaintr-customization")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-customization.md)
  — *Customization*.** Rewrite ggpaintr’s built-in copy with `ui_text`,
  register your own widget types with the three placeholder constructors
  ([`ptr_define_placeholder_value()`](https://willju-wangqian.github.io/ggpaintr/reference/ptr_define_placeholder_value.md)
  / `_consumer()` / `_source()`), and share one widget across multiple
  plots.

- **[`vignette("ggpaintr-safety")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-safety.md)
  — *Safety*.** What `expr_check` does and when (never) to turn it off,
  the denylist + AST-walker safety model, the upload trust boundary, and
  a public-deployment checklist for shinyapps.io / Posit Connect.

- **[`vignette("ggpaintr-gallery")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-gallery.md)
  — *ggpaintr in the Wild*.** A gallery of complete ggplot2 and
  extension-package graphics, each paired with the ggpaintr formula that
  turns it into a Shiny app. Also the canonical worked-example reference
  for plugging interactive output packages (plotly, ggiraph) into your
  own app.

- **[`vignette("ggpaintr-llm")`](https://willju-wangqian.github.io/ggpaintr/articles/ggpaintr-llm.md)
  — *Using ggpaintr from an LLM with ellmer*.** Wire ggpaintr up as an
  LLM-callable tool with ellmer: register the primer and the
  topic-lookup tool, inspect what the model sees, swap providers/models,
  and test without spending tokens.

The [pkgdown reference](https://willju-wangqian.github.io/ggpaintr/)
lists every exported function. A longer, book-length introduction to
ggpaintr is planned.
