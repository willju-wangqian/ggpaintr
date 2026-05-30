pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-1-kitchen-sink: ADR 0013 §App-1 canonical-host baseline.
# Slot picks: ptr_app() + literal mtcars + G2 bare-expression + window code panel + default theme.
# Roster (per ADR §App-1): ppVar×4 (shared "grp" collapses to 2 widgets) ·
# ppNum×4 (shared "lw" collapses to 3 widgets) · ppText×3 · ppExpr×2 · ppRange×1.
# ppRange is a custom value placeholder registered locally in this child process
# (the test process never registers it, so no cross-test contamination — see
# project memory `shinytest2-appdir-pkgload`).

ppRange <- ptr_define_placeholder_value(
  keyword = "ppRange",
  # Accept `selected`: the orchestrator (paintr-build-ui.R:744-765) injects
  # `node$default` into it at boot, so a positional default in the formula
  # (`ppRange(c(0, 50))`) seeds the slider's initial range. Falling back to
  # `c(0, 1)` keeps the widget usable when the formula omits a default.
  build_ui = function(node, label = "Range", selected = NULL, ...) {
    v <- if (is.numeric(selected) && length(selected) == 2L) {
      as.numeric(selected)
    } else c(0, 1)
    shiny::sliderInput(node$id, label, min = 0, max = 100, value = v)
  },
  resolve_expr = function(value, ...) value,
  default_arg = ptr_arg_numeric_vector(length = 2)
)

my_linewidth = 1
color_var = expr(cyl)

formula1 <- expr(
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

ptr_app(
  !!formula1,
  ui_text = list(
    defaults = list(
      ppNum  = list(label = "{param}"),
      ppText = list(label = "{param} text")
    ),
    params = list(
      linewidth = list(ppNum = list(label = "Line thickness"))
    )
  )
)
