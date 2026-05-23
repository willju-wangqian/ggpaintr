# Boot scaffolding (NOT vignette code): load the in-development package into
# this child app process so the e2e test exercises dev source, not a stale
# system install. See `.claude/rules/testing.md` "Browser e2e (shinytest2)" /
# project memory `shinytest2-appdir-pkgload`.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)
# EXCEPT: this fixture is NOT a vignette mirror — it is the ADR 0012 §3.6 /
# PLAN-06 + PLAN-01 (Bug B) spec-apply-session-boot e2e (round-trip closes
# the loop). The `spec=` arg threads through `ptr_app()` ->
# `ptr_app_components()` -> `ptr_make_app_server()` -> `ptr_server_internal()`
# and is consumed by `apply_spec_at_boot()`. After ADR 0012 / PLAN-01:
#
#   - Placeholder rows (every keyword except ppUpload) are seeded via
#     `state$spec_seed[[bare_id]]` synchronously at boot, BEFORE the
#     `session$onFlushed(once = TRUE)` dispatch callback. The renderUI
#     bodies in `ptr_setup_value_uis()` / `ptr_setup_source_uis()` /
#     `ptr_setup_consumer_uis()` read the seed on first fire as
#     `extra$selected`, so the rendered HTML carries the spec value
#     (e.g. `<option value="carb" selected>` for ppVar, `value="5"` for
#     ppNum).
#   - Framework-internal rows (layer_checkbox / stage_enabled) still go
#     through the deferred `updateCheckboxInput()` dispatch path inside
#     the onFlushed callback -- they have no renderUI choke-point.
#   - Seed-applied rows are SKIPPED in the dispatch path (PLAN-02
#     collapse) so custom keywords (no built-in `updateXyz`) no longer
#     aggregate into the spurious "Skipped N spec entries" warning.
#
# Layer-tab visibility:
#   - Layer `ggplot` (the host) has NO pipeline of its own (data is the
#     literal `mtcars`), so its in-aes ppVar pickers (ids
#     `ggplot_1_1_ppVar_NA` / `ggplot_1_2_ppVar_NA`) render in a bare
#     tagList — no tabset, no suspension. They are bound at first flush,
#     so `shinyWidgets::updatePickerInput()` from inside the onFlushed
#     callback hits a live widget and the chosen column appears as the
#     selected option.
#   - The `geom_point()` layer carries a lifted 2-stage pipeline (source
#     `mtcars` + `dplyr::filter(carb > ppNum)`), which gives us a stage-
#     enabled checkbox (`geom_point_2_stage_enabled`) and a layer checkbox
#     (`geom_point_checkbox`). The layer checkbox renders in the layer
#     header (outside the subtab system) and is bound at first flush —
#     same dispatch path as `stage_enabled` (both → `updateCheckboxInput`).
#
# Spec entries cover:
#   - var picker (`ggplot_1_1_ppVar_NA = "carb"`)
#   - layer checkbox (`geom_point_checkbox = FALSE`)
#   - num input inside the lifted filter stage
#     (`geom_point_2_1_2_ppNum_NA = 5`)
#   - unknown id (`no_such_widget_id_99 = "x"`) — silently dropped with a
#     `cli::cli_inform`, no crash.
ptr_app(
  "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point(data = mtcars |> dplyr::filter(carb > ppNum))",
  spec = list(
    `ggplot_1_1_ppVar_NA`       = "carb",
    `geom_point_checkbox`       = FALSE,
    `geom_point_2_1_2_ppNum_NA` = 5,
    `no_such_widget_id_99`      = "x"
  )
)
