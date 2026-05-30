# Regression e2e (2026-05-30): a `spec=` seed naming a DERIVED column (from
# `mutate(adj = ppExpr(...))`) must still be selected at boot even when the spec
# ALSO disables a pipeline stage (`ggplot_2_stage_enabled = FALSE`). The stage
# toggle, applied in `onFlushed`, fires a second consumer redraw in the window
# between the picker emitting the seeded `adj` and the browser committing it
# back; with the latch already flipped that redraw read an empty committed value
# and wiped the selection. The earlier `seed_landed` fix (test-spec-seed-derived-
# column-browser.R) was necessary but not sufficient here -- this exercises the
# stage-toggle race that the first fixture did not. See the consumer column-list
# resolution in R/paintr-server.R (the upstream-producer snapshot fallback).

app_dir <- function() {
  test_path("fixtures", "vignette-apps", "spec-seed-derived-column-stage-off")
}

test_that("a derived-column spec seed survives a boot-time stage toggle", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir(), name = "spec-seed-derived-column-stage-off", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  # `adj` is a derived column, present in the y picker's CHOICES once the
  # mutate evaluates...
  expect_true(
    grepl("adj", app$get_html("#ggplot_1_2_ppVar_NA"), fixed = TRUE),
    label = "derived column 'adj' is in the y picker's choices"
  )
  # ...AND it must be the SELECTED value despite the boot-time stage toggle
  # (the bug: choices had it, selection was wiped by the stage-toggle redraw).
  expect_equal(
    app$get_value(input = "ggplot_1_2_ppVar_NA"), "adj",
    label = "the derived-column spec seed stays selected through the stage toggle"
  )
  # Control: the real-column x seed is unaffected.
  expect_equal(
    app$get_value(input = "ggplot_1_1_ppVar_NA"), "mpg",
    label = "the real-column seed selects as before"
  )
})

test_that("Option I preserves the deselect contract for a derived-column picker", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir(), name = "spec-seed-derived-column-stage-off-deselect", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)
  expect_equal(app$get_value(input = "ggplot_1_2_ppVar_NA"), "adj",
               label = "boot seed lands")

  # The Option I producer-snapshot fallback must NOT re-inject the seed once the
  # user empties the derived-column picker: deselect + Update Plot stays empty.
  app$set_inputs(ggplot_1_2_ppVar_NA = character(0), wait_ = FALSE)
  app$set_inputs(ptr_update_plot = "click", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)
  v <- app$get_value(input = "ggplot_1_2_ppVar_NA")
  expect_true(
    is.null(v) || length(v) == 0L,
    label = paste0("derived-column picker stays empty after deselect (got: ",
                   deparse(v), ")")
  )
})
