pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
# super-1-kitchen-sink: ADR 0013 §App-1 canonical-host baseline.
# Slot picks: ptr_app() + literal mtcars + G2 bare-expression + window code panel + default theme.
# Roster (per ADR §App-1): ppVar×4 (shared "grp" collapses to 2 widgets) ·
# ppNum×4 (shared "lw" collapses to 3 widgets) · ppText×3 · ppExpr×2 · ppRange×1.
# ppRange is a custom value placeholder registered locally in this child process
# (the test process never registers it, so no cross-test contamination — see
# project memory `shinytest2-appdir-pkgload`).

# Force the builtin placeholder registry to initialize *before* registering
# ppRange. `ptr_define_placeholder_value()` only assigns into the registry;
# the lazy `ensure_registry_initialized()` short-circuits if ppRange is
# already present, leaving the built-in keywords (ppNum/ppText/etc.) absent
# from `ptr_ui_text_keywords()` and tripping `ui_text$defaults` validation.
ptr_registry_keywords()

ptr_define_placeholder_value(
  keyword = "ppRange",
  build_ui = function(node, label = "Range", ...) {
    shiny::sliderInput(node$id, label, min = 0, max = 100, value = c(0, 1))
  },
  resolve_expr = function(value, ...) value,
  default_arg = ptr_default_numeric_vector(length = 2)
)

ptr_app(
  ggplot(
    mtcars |>
      dplyr::filter(ppExpr(hp >= 75)) |>
      dplyr::mutate(adj = ppExpr(mpg / wt)),
    aes(x = ppVar(mpg), y = ppVar(adj), color = ppVar(cyl, shared = "grp"))
  ) +
    geom_point(size = ppNum(2), alpha = ppNum(0.7)) +
    geom_smooth(method = ppText("lm"), linewidth = ppNum(1, shared = "lw")) +
    geom_line(linewidth = ppNum(shared = "lw")) +
    facet_wrap(vars(ppVar(cyl, shared = "grp"))) +
    scale_y_continuous(limits = ppRange(c(0, 50))) +
    labs(title = ppText("Title"), subtitle = ppText()),
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
