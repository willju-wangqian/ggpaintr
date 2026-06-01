pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-2a-upload-registry: ADR 0013 §App-2a multi-data-source + custom-registry app.
# Slot picks: ptr_app() · ppUpload mid-pipeline head + ppUpload in geom_smooth(data = )
# · G1 string capture · window code panel · default theme.
# Roster (per ADR §App-2a): ppUpload×2 · ppVar×6 -> 5 widgets after shared "grp"
# collapse · ppNum×2 · ppText×3 · ppExpr×2 · ppPower×1 · ppMultiVar×1.
# ppPower (value) + ppMultiVar (consumer) are registered locally in this child
# process so the parent test process is never contaminated (project memory
# `shinytest2-appdir-pkgload`).
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
    # Non-identity resolve_expr: rewrite value -> a CALL expression `v^2`
    # so the final-mode deparser emits the literal `0.42^2`, not 0.1764.
    rlang::call2("^", value, 2)
  },
  validate_session_input = function(value, ctx) {
    if (is.numeric(value) && length(value) == 1L &&
          !is.na(value) && value >= 0 && value <= 1) TRUE
    else "must be in [0,1]"
  },
  parse_positional_arg = ptr_arg_numeric()
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
  validate_session_input = function(value, ctx) {
    if (length(value) >= 1L) TRUE else NULL
  },
  # default_arg accepts a positional bare symbol so the canonical formula can
  # carry a default (`ppMultiVar(cyl)`) and remain Path-B evaluable (ADR-0016).
  parse_positional_arg = ptr_arg_symbol_or_string()
)

ptr_app(
  paste(
    "ppUpload(df_main) |>",
    "dplyr::filter(ppExpr(hp >= 75)) |>",
    "dplyr::mutate(adj = ppExpr(mpg / wt)) |>",
    "ggplot(aes(x = ppVar(mpg), y = ppVar(adj), color = ppVar(cyl, shared = 'grp'))) +",
    "geom_point(aes(group = ppMultiVar(cyl)), size = ppNum(2), alpha = ppPower(0.7)) +",
    "geom_smooth(data = ppUpload(df_aux), aes(x = ppVar(mpg), y = ppVar(wt)), method = ppText('lm'), inherit.aes = FALSE) +",
    "facet_wrap(vars(ppVar(cyl, shared = 'grp'))) +",
    "labs(title = ppText('Title'), subtitle = ppText(''))",
    sep = " "
  ),
  ui_text = list(
    defaults = list(ppNum = list(label = "{param}")),
    params = list(method = list(ppText = list(label = "Smoothing method")))
  )
)
