# Authoritative boot-time gate for FINDING #1 + FINDING #7 from
# dev/notes/placeholder-role-coverage2.html (v7).
#
# `spec = list(shared_<k> = ...)` entries that target a panel-shared
# placeholder (key referenced in >=2 formulas → host-owned un-namespaced
# input id) must drive the widget's boot value the same way they do for
# per-instance ids. Pre-fix, both paths dropped the spec value: the panel-
# consumer fell back to shared_widget_default(), and the panel-value's
# `output$shared_<k>_ui` was never even registered at host scope (the per-
# instance shared-value loop registered under the namespaced id, which the
# un-namespaced UI div could not reach). This is now fixed; the assertions
# below lock the post-fix contract.
#
# These tests embody the post-fix contract. Pre-fix they FAIL; that
# failure IS the gate Phase 1 of /diagnose called for.

test_that("finding #1: spec= for a panel-shared consumer drives the picker at boot", {
  app <- boot_vignette_app("spec-panel-consumer")

  app$wait_for_idle(timeout = 15 * 1000)

  # `shared_col` lives at the host's un-namespaced root. Post-fix the
  # spec entry overrides shared_widget_default() (= "cyl", the first-
  # occurrence default in the formula). The input binding sends the
  # currently selected value back to the server; assert against that.
  expect_equal(
    app$get_value(input = "shared_col"),
    "mpg",
    label = paste0(
      "panel-shared consumer respects spec= at boot ",
      "(should be \"mpg\" from spec, not \"cyl\" from shared_widget_default())"
    )
  )

  expect_no_inline_error(app, "ptr_error")
})

test_that("finding #7: spec= for a panel-shared value drives the widget at boot", {
  app <- boot_vignette_app("spec-panel-value")

  app$wait_for_idle(timeout = 15 * 1000)

  # `shared_lw` lives at the host's un-namespaced root. Post-fix:
  #   - the host-scope renderUI registers `output$shared_lw_ui` and reads
  #     a host spec_seed bag, so the numericInput materializes with the
  #     spec value (2), not the formula's first-occurrence default (1).
  expect_equal(
    app$get_value(input = "shared_lw"),
    2,
    label = paste0(
      "panel-shared value widget respects spec= at boot ",
      "(should be 2 from spec, not 1 from shared_widget_default())"
    )
  )

  expect_no_inline_error(app, "ptr_error")
})
