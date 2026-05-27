# ADR 0012 §3.7 / PLAN-04 — bug-3a e2e regression.
#
# Boots `tests/testthat/fixtures/vignette-apps/adr12-bug-3a/` (formula:
# `ppUpload |> dplyr::filter(ppVar > ppNum) |> ggplot(aes(ppVar, ppVar)) +
# geom_point()`), uploads `penguins.csv`, and asserts:
#   1. The in-filter ppVar picker (id `ggplot_2_1_1_ppVar_NA`) shows the
#      uploaded data's columns as options (non-empty choices including
#      "bill_length_mm").
#   2. The in-aes ppVar pickers (ids `ggplot_1_1_ppVar_NA` and
#      `ggplot_1_2_ppVar_NA`) show the same columns.
#   3. Both populate from the SAME upstream resolution path (mechanically:
#      both choice-set HTML contains "bill_length_mm").
#
# This proves the per-layer fast-path deletion did not break in-aes
# picker population. Picker ids were discovered by inspecting the
# translated tree's consumer ids (see fixture header note); they are stable
# under ADR 0012's positional-id convention.

test_that("adr12 / PLAN-04 / bug-3a: in-filter ppVar picker populates after upload under `|>` input", {
  app <- boot_vignette_app("adr12-bug-3a")

  # Upload the CSV. shinytest2's `upload_file` argument triggers the
  # same input observer as the browser file picker.
  app$upload_file(ggplot_1_ppUpload_NA = testthat::test_path(
    "fixtures", "vignette-apps", "adr12-bug-3a", "penguins.csv"
  ))

  # Companion text input — explicitly set so the source observer's
  # `binding_name` resolution finds a non-NULL companion on first eval.
  # The browser auto-fills it from the uploaded filename via
  # `ptr_bind_source_autoname()`, but the explicit set guards against
  # ordering races inside AppDriver.
  set_input(app, "ggplot_1_ppUpload_NA_shortcut", "penguins")

  # Switch to the layer's Controls subtab so the suspended `renderUI`s
  # for the var pickers (in-filter ppVar and in-aes ppVars) bind — they
  # live in Controls, not Data, per the existing e2e pattern in
  # test-e2e-vignette-examples-shinytest2.R. (Project memory:
  # `shinytest2-appdir-pkgload` — "`var` pickers for source/consumer
  # placeholders are suspended" until their subtab is shown.)
  set_input(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)

  # In-filter ppVar — verifies the post-deletion `ptr_resolve_upstream`
  # path runs end-to-end for an in-stage consumer (the ppVar in
  # `filter(ppVar > ppNum)` sees the uploaded data's columns).
  expect_picker_populated(app, "ggplot_2_1_1_ppVar_NA", "bill_length_mm")
  # In-aes ppVars — verifies the in-aes branch also routes through
  # `ptr_resolve_upstream` correctly after fast-path deletion. The two
  # aes ppVars share the same upstream resolution path.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "bill_length_mm")
  expect_picker_populated(app, "ggplot_1_2_ppVar_NA", "bill_length_mm")
})
