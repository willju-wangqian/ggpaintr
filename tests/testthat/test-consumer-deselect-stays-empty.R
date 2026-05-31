# End-to-end regression tests for the consumer-picker deselect bug
# (reported against the super-1 kitchen-sink fixture in dev/scripts).
#
# Bug: user deselects a `ppVar(...)` picker, clicks Update Plot, the
# picker silently snaps back to its formula default (or first-occurrence
# default for shared pickers). Caused by `invoke_build_ui()`'s
# `length(extra$selected) == 0L` branch conflating "input not bound on
# first render" with "user explicitly emptied the widget" — see
# invoke_build_ui()'s seed branch (R/paintr-build-ui.R:842) and the bind
# paths in ptr_setup_value_uis()/ptr_setup_source_uis() (R/paintr-server.R).
#
# Unit-layer regression (no Shiny session): see
# tests/testthat/test-invoke-build-ui-selected-contract.R (U3).
#
# Fixture: tests/testthat/fixtures/vignette-apps/consumer-deselect-stays-empty/
# (one ptr_app with non-shared ppVar(mpg) for x and shared ppVar(cyl,
# shared = "grp") for both color and facet — collapses to one widget).

empty_picker <- function(v) {
  is.null(v) || (is.character(v) && length(v) == 0L)
}

test_that("non-shared consumer: deselect + Update Plot stays empty (does not snap to ppVar default)", {
  app <- boot_vignette_app("consumer-deselect-stays-empty")
  x_id <- "ggplot_1_1_ppVar_NA"

  # Boot: positional default seeds the picker. This is the existing
  # ADR-0009 boot-seed contract — pin it here too so a regression in
  # the boot path shows up as part of this file's failure surface.
  expect_equal(app$get_value(input = x_id), "mpg",
               label = "boot seed from ppVar(mpg)")

  # Persistence baseline: a real pick survives Update Plot.
  set_input(app, x_id, "gear")
  draw(app, "ptr_update_plot")
  expect_equal(app$get_value(input = x_id), "gear",
               label = "user pick survives Update Plot")

  # The bug repro: deselect to character(0), click Update Plot. Before
  # the fix this snapped back to "mpg"; after the fix it stays empty.
  set_input(app, x_id, character(0))
  draw(app, "ptr_update_plot")
  v <- app$get_value(input = x_id)
  expect_true(empty_picker(v),
              label = paste0("non-shared picker stays empty after deselect (got: ",
                             deparse(v), ")"))
})

test_that("shared consumer: deselect + Update Plot stays empty (does not snap to first-occurrence default)", {
  app <- boot_vignette_app("consumer-deselect-stays-empty")
  shared_id <- "shared_grp"

  # Boot: shared widget seeds from the first occurrence's default ("cyl").
  expect_equal(app$get_value(input = shared_id), "cyl",
               label = "shared widget boot-seeds from first occurrence")

  # Persistence.
  set_input(app, shared_id, "gear")
  draw(app, "ptr_update_plot")
  expect_equal(app$get_value(input = shared_id), "gear",
               label = "shared pick survives Update Plot")

  # The bug repro on the shared path. ptr_bind_shared_consumer_uis()
  # had the same `seed %||% current %||% character(0)` shape as the
  # non-shared binder; the fix replicated the NULL-vs-character(0)
  # distinction there too.
  set_input(app, shared_id, character(0))
  draw(app, "ptr_update_plot")
  v <- app$get_value(input = shared_id)
  expect_true(empty_picker(v),
              label = paste0("shared picker stays empty after deselect (got: ",
                             deparse(v), ")"))
})
