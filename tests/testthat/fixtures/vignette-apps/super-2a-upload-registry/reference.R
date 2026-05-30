# Path-B reference (ADR-0016): the naked-ggplot expression from app.R,
# evaluable as plain R outside `ptr_app()`. df_main and df_aux are the
# datasets that ppUpload would normally pull from the file widgets — bound
# locally so the formula resolves.
library(ggpaintr)
library(ggplot2)
library(shiny)

ppPower <- ptr_define_placeholder_value(
  keyword = "ppPower",
  build_ui = function(node, label = "Power", selected = NULL, ...) {
    shiny::numericInput(node$id, label,
                        value = selected %||% node$default %||% 0.7,
                        min = 0, max = 1, step = 0.01)
  },
  resolve_expr = function(value, ...) rlang::call2("^", value, 2),
  positional_arg = ptr_arg_numeric()
)

ppMultiVar <- ptr_define_placeholder_consumer(
  keyword = "ppMultiVar",
  build_ui = function(node, cols = character(), label = "Group by",
                      selected = character(0), ...) {
    shinyWidgets::pickerInput(node$id, label, choices = cols,
                              selected = intersect(selected, cols),
                              multiple = TRUE)
  },
  resolve_expr = function(value, ...) {
    if (length(value) == 0L) return(rlang::missing_arg())
    if (length(value) == 1L) return(rlang::sym(value))
    rlang::call2("interaction", !!!rlang::syms(value))
  },
  positional_arg = ptr_arg_symbol_or_string()
)

df_main <- mtcars
df_aux  <- mtcars

# The same formula source string as app.R, just `eval(parse(text = ...))`-ed
# here. Built-in identity runtimes + the custom placeholders above + the data
# bindings make this Path-B evaluable.
ppUpload(df_main) |>
  dplyr::filter(ppExpr(hp >= 75)) |>
  dplyr::mutate(adj = ppExpr(mpg / wt)) |>
  ggplot(aes(x = ppVar(mpg), y = ppVar(adj), color = ppVar(cyl, shared = "grp"))) +
  geom_point(aes(group = ppMultiVar(cyl)), size = ppNum(2), alpha = ppPower(0.7)) +
  geom_smooth(data = ppUpload(df_aux), aes(x = ppVar(mpg), y = ppVar(wt)),
              method = ppText("lm"), inherit.aes = FALSE) +
  facet_wrap(vars(ppVar(cyl, shared = "grp"))) +
  labs(title = ppText("Title"), subtitle = ppText(""))
