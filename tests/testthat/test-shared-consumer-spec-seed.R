# Regression — shared `ppVar(shared = "<key>")` consumer must honor the
# `state$spec_seed` written by `apply_spec_at_boot` for the canonical
# `shared_<key>` id at boot.
#
# Before the fix, `ptr_bind_shared_consumer_uis()` seeded its picker's
# `selected` from the live `input[[ns(rep_node$id)]]` only and never
# read `state$spec_seed[[rep_node$id]]` — so a
# `spec = list(shared_<key> = ...)` entry wrote into `state$spec_seed`
# (via `apply_spec_at_boot`) but never reached the rendered picker. The
# widget booted blank.
#
# The closed-loop seed test in `test-spec-roundtrip-closed-loop.R::Test D`
# already covers that `state$spec_seed[["shared_grp"]]` is populated; the
# missing check is the renderUI side, which only fires under a real host
# (`ptr_make_app_server` → `ptr_bind_local_shared_consumers`). That host
# is exercised here via the e2e `ptr_app()` fixture.

test_that("regression: shared consumer picker honors spec_seed at boot (shared_<key>)", {
  app <- boot_vignette_app("shared-consumer-spec")
  app$wait_for_idle(timeout = 15 * 1000)

  # Companion shared-value widget (ppNum, shared = "lw") — this already
  # worked pre-fix because `ptr_setup_value_uis()`'s shared loop read
  # `state$spec_seed`. Acts as the "other half works" control.
  expect_equal(app$get_value(input = "shared_lw"), 2,
               label = "shared_lw honored at boot (2 from spec)")

  # The bug: shared consumer (ppVar, shared = "grp"). Must boot at "cyl".
  expect_picker_populated(app, "shared_grp", "cyl")
  expect_equal(app$get_value(input = "shared_grp"), "cyl",
               label = "shared_grp honored at boot (\"cyl\" from spec)")
})
