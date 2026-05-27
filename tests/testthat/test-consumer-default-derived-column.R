# Regression: ppVar(<derived-col>) consumer must default-seed even when the
# derived column is produced by an upstream placeholder (e.g. ppExpr inside
# `dplyr::mutate()`).
#
# Bug (reported 2026-05-27): boot the super-1 kitchen-sink fixture and click
# Draw without touching any inputs. The y picker (`aes(y = ppVar(adj))`)
# stayed empty -> final-mode code emitted `aes(x = mpg, color = cyl)` with
# no y -> `stat_smooth() requires the following missing aesthetics: y`.
#
# Root cause: the first `entry_reactive()` fire for the y picker happened
# before the upstream ppExpr widgets had echoed their initial values to the
# snapshot; the `mutate(adj = ...)` pruned and `adj` was absent from cols.
# `invoke_build_ui` injected `node$default = "adj"`, but
# `ptr_builtin_var_build_ui`'s `intersect(selected, cols)` dropped it.
# `has_rendered` flipped TRUE anyway (gated only on `length(cols) > 0L`),
# locking subsequent fires into the `seed %||% current %||% character(0)`
# branch with `current = character(0)` -- the framework default no longer
# re-injected once the snapshot stabilized.
#
# Fix: `ptr_setup_consumer_uis` flips `has_rendered` only when the fire
# could persist the chosen selection (selected_arg non-NULL, or the
# effective default landed in cols). See R/paintr-server.R around the
# consumer renderUI block.
#
# This test guards the user-reported repro path exactly: boot, click Draw,
# no inputs touched. The strengthened super-1 suite assertion in
# test-super-pressure.R covers the same gate after the existing
# set-sentinels-then-Draw scenario.

test_that("super-1: click Draw with no input changes -> y default-seeds to derived column", {
  app <- boot_super_app("super-1-kitchen-sink")
  app$wait_for_idle(timeout = 25 * 1000)

  app$click("ptr_update_plot")
  app$wait_for_idle(timeout = 25 * 1000)

  # Picker actually selects "adj" -- not just offers it. The old
  # `expect_picker_populated` is a presence proxy on the options list and
  # missed this bug entirely.
  expect_picker_selected(app, "ggplot_1_2_ppVar_NA", "adj")

  # Plot does not error. We need the document-scoped check here because the
  # missing-aesthetic error routes into the sibling `#ptr_error` host, not
  # `#ptr_plot` (which carries a stale-but-successful `<img>`). The narrow
  # `expect_no_plot_error` would have missed the bug.
  expect_no_plot_error(app)
  expect_no_inline_error_anywhere(app)

  # Final-mode code includes `aes(... y = adj ...)`. Without this, an
  # empty-y final-mode render would still pass every other propagation
  # regex in the super-1 suite (x / color / facet / mutate-RHS / title).
  expect_sentinel_in_code(app, "ptr_code", "adj",
    "aes\\([^)]*y\\s*=\\s*([^,)]*)", "final")
})
