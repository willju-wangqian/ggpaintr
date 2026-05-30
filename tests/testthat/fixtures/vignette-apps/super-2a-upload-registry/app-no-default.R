pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-2a-upload-registry (no-default variant): same formula structure as
# app.R but every `pp*(default)` is stripped of its positional default.
# Path-A pressure-test variant per ADR-0016. ppUpload's bareword companion
# names (df_main / df_aux) are STRUCTURAL identifiers, not default values
# — they stay.
library(shiny)

ppPower <- ptr_define_placeholder_value(
  keyword = "ppPower",
  build_ui = function(node, label = "Power", selected = NULL, ...) {
    v <- if (is.numeric(selected) && length(selected) == 1L && !is.na(selected)) {
      as.numeric(selected)
    } else if (is.numeric(node$default) && length(node$default) == 1L) {
      as.numeric(node$default)
    } else 0.7
    shiny::numericInput(node$id, label, value = v, min = 0, max = 1, step = 0.01)
  },
  resolve_expr = function(value, ...) {
    rlang::call2("^", value, 2)
  },
  validate_input = function(value, ctx) {
    if (is.numeric(value) && length(value) == 1L &&
          !is.na(value) && value >= 0 && value <= 1) TRUE
    else "must be in [0,1]"
  },
  positional_arg = ptr_arg_numeric()
)

ppMultiVar <- ptr_define_placeholder_consumer(
  keyword = "ppMultiVar",
  build_ui = function(node, cols = character(), label = "Group by",
                      selected = character(0), ...) {
    shinyWidgets::pickerInput(
      inputId = node$id,
      label = label,
      choices = cols,
      selected = intersect(selected, cols),
      multiple = TRUE,
      options = shinyWidgets::pickerOptions(noneSelectedText = label)
    )
  },
  resolve_expr = function(value, ...) {
    if (length(value) == 0L) return(rlang::missing_arg())
    if (length(value) == 1L) return(rlang::sym(value))
    rlang::call2("interaction", !!!rlang::syms(value))
  },
  validate_input = function(value, ctx) {
    if (length(value) >= 1L) TRUE else NULL
  }
)

ptr_app(
  paste(
    "ppUpload() |>",
    "dplyr::filter(ppExpr()) |>",
    "dplyr::mutate(adj = ppExpr()) |>",
    "ggplot(aes(x = ppVar(), y = ppVar(), color = ppVar(shared = 'grp'))) +",
    "geom_point(aes(group = ppMultiVar()), size = ppNum(), alpha = ppPower()) +",
    "geom_smooth(data = ppUpload(), aes(x = ppVar(), y = ppVar()), method = ppText(), inherit.aes = FALSE) +",
    "facet_wrap(vars(ppVar(shared = 'grp'))) +",
    "labs(title = ppText(), subtitle = ppText())",
    sep = " "
  ),
  ui_text = list(
    defaults = list(ppNum = list(label = "{param}")),
    params = list(method = list(ppText = list(label = "Smoothing method")))
  )
)
