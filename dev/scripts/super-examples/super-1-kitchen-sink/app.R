# Playable copy. Run from repo root: devtools::load_all(".") then shiny::runApp("<this dir>"). See dev/scripts/super-examples/README.md.
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
  positional_arg = ptr_arg_numeric_vector(length = 2)
)

my_linewidth = 1
color_var = expr(cyl)

formula1 <- expr(
  ggplot(
    mtcars |>
      dplyr::filter(ppExpr(hp >= 75)) |>
      dplyr::mutate(adj = ppExpr(mpg / wt)) |>
      ppVerbSwitch(dplyr::slice_max(mpg, n = 15), TRUE, label = "Top 15 by mpg"),
    aes(x = ppVar(mpg), y = ppVar(adj), color = (ppVar(!!color_var, shared = "grp")))
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
  ),
  # spec = ptr_spec
)


formula2 <- expr(ggplot(
  data = mtcars |>
    dplyr::filter(ppExpr(hp >= 75)) |>
    dplyr::mutate(adj = ppExpr(mpg/wt + hp/10)) |>
    dplyr::slice_max(mpg, n = 15),
  aes(x = ppVar(mpg), y = ppVar(adj), color = factor(ppVar(am, shared = "grp")))
) +
  geom_point(size = ppNum(2), alpha = ppNum(0.7)) +
  geom_smooth(method = ppText("lm"), linewidth = ppNum(1, shared = "lw")) +
  geom_line(linewidth = ppNum(1, shared = "lw")) +
  facet_wrap(vars(ppVar(am, shared = "grp"))) +
  scale_y_continuous(limits = ppRange(c(0, 50))) +
  labs(title = ppText("Title"), subtitle = ppText))

ptr_spec <- list(
  `ggplot_2_1_ppExpr_NA` = "hp >= 75",
  `ggplot_3_1_ppExpr_NA` = "mpg/wt + hp / 10",
  `ggplot_1_1_ppVar_NA` = "mpg",
  `ggplot_1_2_ppVar_NA` = "adj",
  `shared_grp` = "am",
  `geom_point_1_ppNum_NA` = 2L,
  `geom_point_2_ppNum_NA` = 0.7,
  `geom_smooth_1_ppText_NA` = "lm",
  `shared_lw` = 1L,
  `scale_y_continuous_1_ppRange_NA` = c(0L, 50L),
  `labs_1_ppText_NA` = "Title",
  `labs_2_ppText_NA` = "",
  `geom_point_checkbox` = FALSE,
  `geom_line_checkbox` = TRUE,
  `ggplot_4_stage_enabled` = FALSE
)

ptr_app(!!formula2,
        spec = ptr_spec)

formula3 <- expr(ggplot(
  data = mtcars |>
    dplyr::filter(ppExpr(hp >= 75)) |>
    dplyr::mutate(adj = ppExpr(mpg/wt + hp/10)) |>
    dplyr::slice_max(mpg, n = 15),
  aes(x = ppVar(mpg), y = ppVar(adj), color = factor(ppVar(am, shared = "grp")))
) +
  geom_point(size = ppNum(2), alpha = ppNum(0.7)) +
  geom_smooth(method = ppText("lm"), linewidth = ppNum(1, shared = "lw")) +
  geom_line(linewidth = ppNum(1, shared = "lw")) +
  facet_wrap(vars(ppVar(am, shared = "grp"))) +
  scale_y_continuous(limits = ppRange(c(0, 50))) +
  labs(title = ppText("Title"), subtitle = ppText)
)

ptr_spec <- list(
  `ggplot_2_1_ppExpr_NA` = "hp >= 75",
  `ggplot_3_1_ppExpr_NA` = "mpg/wt + hp / 10",
  `ggplot_1_1_ppVar_NA` = "mpg",
  `ggplot_1_2_ppVar_NA` = "adj",
  `shared_grp` = "am",
  `geom_point_1_ppNum_NA` = 2L,
  `geom_point_2_ppNum_NA` = 0.7,
  `geom_smooth_1_ppText_NA` = "lm",
  `shared_lw` = 1L,
  `scale_y_continuous_1_ppRange_NA` = c(0L, 50L),
  `labs_1_ppText_NA` = "Title",
  `labs_2_ppText_NA` = "",
  `geom_point_checkbox` = FALSE,
  `ggplot_2_stage_enabled` = FALSE
)
ptr_app(!!formula3, spec= ptr_spec)
