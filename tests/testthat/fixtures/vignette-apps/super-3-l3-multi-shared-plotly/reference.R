# Path-B reference (ADR-0016): the naked-ggplot expressions from app.R's
# two L3 cells, evaluable as plain R outside `ptr_app()`. Each cell's
# formula collapses to its positional defaults under the built-in identity
# runtimes + the custom ppRange's identity resolve_expr.
library(ggpaintr)
library(ggplot2)
library(shiny)

ppRange <- ptr_define_placeholder_value(
  keyword     = "ppRange",
  default_arg = ptr_arg_numeric_vector(length = 2),
  build_ui    = function(node, label = NULL, selected = NULL, ...) {
    val <- if (!is.null(selected) && length(selected) == 2L) selected else c(0, 100)
    shiny::sliderInput(node$id, label %||% "Range",
                       min = -100, max = 100, value = val, step = 0.1)
  },
  resolve_expr = function(value, node, ...) {
    if (is.null(value) || length(value) != 2L) return(NULL)
    value <- as.double(value)
    rlang::expr(c(!!value[1], !!value[2]))
  }
)

# Cell A — plain ggplot output.
formula_a <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(mpg), y = ppVar(wt),
                     color = ppVar(cyl, shared = "linked"))) +
    geom_point(size = ppNum(2), alpha = ppNum(0.7)) +
    scale_x_continuous(limits = ppRange(c(10, 40))) +
    labs(title = ppText("Cell A: ggplot"))
)

# Cell B — plotly-wrapped output (Path-B yields the underlying ggplot; the
# plotly conversion is an app-only concern).
formula_b <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(hp), y = ppVar(qsec),
                     color = ppVar(cyl, shared = "linked"))) +
    geom_point(size = ppNum(2)) +
    geom_smooth(method = ppText("lm"), linewidth = ppNum(1)) +
    labs(title = ppText("Cell B: plotly"))
)

eval(formula_a)
eval(formula_b)
