# Regression e2e (2026-05-29 #2): a `spec=` seed naming a DERIVED column -- one
# produced by an upstream `mutate(adj = ppExpr(...))`, so it is absent from the
# consumer picker's `cols` on the first render and only appears once the ppExpr
# echoes -- must still seed the picker. `consumer_seed_decision()` used to flip
# the `has_rendered` latch the moment any seed was present (`!is.null(selected)`),
# even though the downstream `intersect(selected, cols)` drops a seed whose
# column is not yet in `cols`. So the latch flipped on the empty first render and
# the seed was gone when `adj` finally appeared -- the y picker booted EMPTY
# while `adj` sat selectable in its choices. The fix added a `seed_landed` gate
# (R/paintr-server.R), symmetric with the existing `default_landed`. Unit seam:
# test-spec-seed-boot-only.R "Seam 1d".

app_dir <- function() {
  test_path("fixtures", "vignette-apps", "spec-seed-derived-column")
}

test_that("a spec seed for a derived (mutate/ppExpr) column selects it at boot", {
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
    app_dir(), name = "spec-seed-derived-column", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  # `adj` is a derived column -- it must be present in the y picker's CHOICES
  # (the mutate evaluated once the seeded ppExpr echoed)...
  expect_true(
    grepl("adj", app$get_html("#ggplot_1_2_ppVar_NA"), fixed = TRUE),
    label = "derived column 'adj' is in the y picker's choices"
  )

  # ...AND it must be the SELECTED value (the bug: choices had it, selection
  # was empty).
  expect_equal(
    app$get_value(input = "ggplot_1_2_ppVar_NA"), "adj",
    label = "the derived-column spec seed is selected at boot"
  )

  # Control: the x picker over a real (non-derived) mtcars column was never
  # affected -- confirms the asymmetry was specific to the derived column.
  expect_equal(
    app$get_value(input = "ggplot_1_1_ppVar_NA"), "mpg",
    label = "the real-column seed selects as before"
  )
})
