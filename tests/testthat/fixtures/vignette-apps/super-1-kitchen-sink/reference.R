# Path-B reference (ADR-0016): the naked-ggplot expression from app.R,
# evaluable as plain R without `ptr_app()`. Built-in pp* runtime fns are
# `function(x, ...) x` (identity); the custom ppRange has identity
# resolve_expr — so every `pp*(default)` in the formula collapses to its
# positional default at eval time. The resulting plot is the same shape as
# the original (default-equivalent) reference, modulo labels.
#
# Run via:  source("reference.R")        # in a session with the package loaded
library(ggpaintr)
library(ggplot2)

ppRange <- ptr_define_placeholder_value(
  keyword = "ppRange",
  build_ui = function(node, label = "Range", selected = NULL, ...) {
    v <- if (is.numeric(selected) && length(selected) == 2L) {
      as.numeric(selected)
    } else c(0, 1)
    shiny::sliderInput(node$id, label, min = 0, max = 100, value = v)
  },
  resolve_expr = function(value, ...) value,
  default_arg = ptr_arg_numeric_vector(length = 2)
)

my_linewidth <- 1
color_var <- rlang::expr(cyl)

formula1 <- rlang::expr(
  ggplot(
    mtcars |>
      dplyr::filter(ppExpr(hp >= 75)) |>
      dplyr::mutate(adj = ppExpr(mpg / wt)) |>
      ppVerbSwitch(dplyr::slice_max(mpg, n = 15), TRUE, label = "Top 15 by mpg"),
    aes(x = ppVar(mpg), y = ppVar(adj), color = ppVar(!!color_var, shared = "grp"))
  ) +
    geom_point(size = ppNum(2), alpha = ppNum(0.7)) +
    geom_smooth(method = ppText("lm"), linewidth = ppNum(!!my_linewidth, shared = "lw")) +
    ppLayerOff(geom_line(linewidth = ppNum(!!my_linewidth, shared = "lw")), TRUE) +
    facet_wrap(vars(ppVar(!!color_var, shared = "grp"))) +
    scale_y_continuous(limits = ppRange(c(0, 50))) +
    labs(title = ppText("Title"), subtitle = ppText(""))
)

# Evaluate as plain ggplot — Path B.
eval(formula1)
