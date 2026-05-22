# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the ADR 0012 §3.6 /
# PLAN-06 spec-apply-session-boot e2e (round-trip closes the loop). The
# `spec=` arg threads through `ptr_app()` -> `ptr_app_components()` ->
# `ptr_make_app_server()` -> `ptr_server_internal()` and is consumed by
# `apply_spec_at_boot()` inside a single `session$onFlushed(once = TRUE)`.
#
# Formula chosen so that no ppVar picker ends up under a suspended layer
# subtab:
#   - Layer `ggplot` (the host) has NO pipeline of its own (data is the
#     literal `mtcars`), so its in-aes ppVar pickers (ids
#     `ggplot_1_1_ppVar_NA` / `ggplot_1_2_ppVar_NA`) render in a bare
#     tagList — no tabset, no suspension. They are bound at first flush,
#     so `shinyWidgets::updatePickerInput()` from inside the onFlushed
#     callback hits a live widget and the chosen column appears as the
#     selected option.
#   - The `geom_point()` layer carries the pipeline (`data = mtcars |>
#     dplyr::filter(carb > ppNum)`), which gives us a stage-enabled
#     checkbox (`geom_point_0_stage_enabled`) and a layer checkbox
#     (`geom_point_checkbox`). The layer checkbox renders in the layer
#     header (outside the subtab system) and is bound at first flush —
#     same dispatch path as `stage_enabled` (both → `updateCheckboxInput`).
#
# Spec entries cover:
#   - var picker (`ggplot_1_1_ppVar_NA = "carb"`)
#   - layer checkbox (`geom_point_checkbox = FALSE`)
#   - num input (`geom_point_2_2_ppNum_NA = 5`)
#   - unknown id (`no_such_widget_id_99 = "x"`) — silently dropped with a
#     `cli::cli_inform`, no crash.
ptr_app(
  "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(data = mtcars |> dplyr::filter(carb > ppNum))",
  spec = list(
    `ggplot_1_1_ppVar_NA`     = "carb",
    `geom_point_checkbox`     = FALSE,
    `geom_point_2_2_ppNum_NA` = 5,
    `no_such_widget_id_99`    = "x"
  )
)
