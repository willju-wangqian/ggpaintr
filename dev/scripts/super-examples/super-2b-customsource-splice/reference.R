# Path-B reference (ADR-0016): the naked-ggplot expression from app.R,
# evaluable as plain R outside `ptr_app()`. The custom ppSample passes a
# `runtime =` that resolves dataset names to data frames; ppFactor + ppCoef
# auto-gen identity runtimes; df_rug is bound locally so the formula
# resolves.
library(ggpaintr)
library(ggplot2)
library(shiny)

ppCoef <- ptr_define_placeholder_value(
  keyword       = "ppCoef",
  build_ui      = function(node, label = NULL, selected = NULL, ...) {
    sliderInput(node$id, label = label %||% "Coef",
                min = 0, max = 1,
                value = selected %||% node$default %||% 0.5,
                step = 0.01)
  },
  resolve_expr  = function(value, node, ...) {
    if (is.null(value)) return(NULL)
    rlang::expr(!!value)
  },
  default_arg   = ptr_default_numeric()
)

ppFactor <- ptr_define_placeholder_consumer(
  keyword       = "ppFactor",
  build_ui      = function(node, cols = character(), label = NULL, ...) {
    shinyWidgets::pickerInput(node$id, label = label %||% "Factor",
                              choices = cols)
  },
  resolve_expr  = function(value, node, ...) {
    if (length(value) == 0L || !nzchar(value)) return(NULL)
    rlang::sym(value)
  },
  default_arg   = ptr_default_symbol_or_string()
)

ppSample <- ptr_define_placeholder_source(
  keyword       = "ppSample",
  build_ui      = function(node, label = NULL, selected = NULL, ...) {
    selectInput(node$id, label = label %||% "Sample dataset",
                choices = c("iris", "mtcars", "ChickWeight", "ToothGrowth"),
                selected = selected %||% node$default %||% "iris")
  },
  resolve_data  = function(value, ...) {
    if (is.null(value) || !nzchar(value)) return(NULL)
    get(value, envir = asNamespace("datasets"))
  },
  runtime       = function(x, ...) {
    if (missing(x) || is.null(x) || !nzchar(x)) {
      rlang::abort("`ppSample()` needs a dataset name (e.g., \"iris\").")
    }
    get(x, envir = asNamespace("datasets"))
  },
  default_arg   = ptr_default_string()
)

smooth_template <- rlang::expr(
  geom_smooth(method = ppText("lm"), linewidth = ppNum(1), alpha = ppCoef(0.3))
)

df_rug <- mtcars

formula <- rlang::expr(
  ggplot(ppSample("iris"),
         aes(x = ppVar(Sepal.Length),
             y = ppVar(Sepal.Width),
             color = ppFactor(Species, shared = "fac"))) +
    geom_point(size = ppNum(2)) +
    !!smooth_template +
    geom_rug(data = ppUpload(df_rug),
             aes(x = ppVar(mpg), y = ppVar(wt)),
             inherit.aes = FALSE) +
    facet_wrap(vars(ppFactor(Species, shared = "fac"))) +
    labs(title = ppText("ppSample + splice + layer-upload"))
)

eval(formula)
