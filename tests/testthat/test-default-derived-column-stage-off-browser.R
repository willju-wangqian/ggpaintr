# Regression e2e (2026-05-30): a DERIVED-column FORMULA DEFAULT -- `ppVar(adj)`
# over `mutate(adj = ppExpr(...))` with NO spec seed for the picker or its
# upstream ppExpr -- must still seed at boot when the spec disables a pipeline
# stage. Same stage-toggle race as the spec-seed case, but the boot value now
# comes from the producer's formula default (its bareword expression), not a
# spec seed. Pins the default arm of the Option I producer-snapshot fallback in
# R/paintr-server.R (alongside test-spec-seed-derived-column-stage-off-browser.R
# for the spec arm).

app_dir <- function() {
  test_path("fixtures", "vignette-apps", "default-derived-column-stage-off")
}

test_that("a derived-column formula default survives a boot-time stage toggle", {
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
    app_dir(), name = "default-derived-column-stage-off", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  expect_true(
    grepl("adj", app$get_html("#ggplot_1_2_ppVar_NA"), fixed = TRUE),
    label = "derived column 'adj' is in the y picker's choices"
  )
  expect_equal(
    app$get_value(input = "ggplot_1_2_ppVar_NA"), "adj",
    label = "the derived-column formula default stays selected through the stage toggle"
  )
  expect_equal(
    app$get_value(input = "ggplot_1_1_ppVar_NA"), "mpg",
    label = "the real-column default selects as before"
  )
})
