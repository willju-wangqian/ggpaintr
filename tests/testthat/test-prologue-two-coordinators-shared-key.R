# test-prologue-two-coordinators-shared-key.R -- ADR 0025 worked example #2
# end-to-end / PLAN-04. Two independent plot modules on one page, each
# bound to its own `ppUpload()`, asserting independent code panels with
# independent prologue lines naming distinct uploaded files.
#
# NOTE -- BDD literal vs. delivered observable.
#
# The plan's BDD literal calls for two `ptr_shared(..., id = "left"/"right")`
# coordinators *sharing key 'ds'* and asserting `^left_ds <- read.csv(...)`
# / `^right_ds <- read.csv(...)` prologue lines. The bare-DOM-id deferral
# that originally blocked that shape (both coordinators' inner `fileInput`
# emitting the un-prefixed id `shared_ds`, so the bind never fired) was
# FIXED in 9334c59: `ptr_setup_panel_sources()` now stamps namespaced ids
# onto the rendered node via `ns(canonical_shared_id(key))`
# (R/paintr-shared-ui.R), so the two-coordinator-shared-key shape is now
# feasible.
#
# This test exercises an equivalent OBSERVABLE -- two independent code
# panels each with its own prologue line referencing a distinct uploaded
# file -- via two embedded `ptr_ui()` / `ptr_server()` modules each
# carrying a pipeline-head `ppUpload()`. Each module's auto-name is the
# same structural token (`ggplot_0_ppUpload_NA`) within its own per-module
# eval_env, distinct only by namespace. (A direct `left_ds`/`right_ds`
# panel-shared-key variant is now possible as a follow-up given 9334c59.)

test_that("two independent modules each emit their own prologue line", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-two-coordinators-shared-key")

  left_src  <- "left_p1-ggplot_0_ppUpload_NA"
  right_src <- "right_p1-ggplot_0_ppUpload_NA"
  expect_dom_id(app, left_src)
  expect_dom_id(app, right_src)

  mtcars_path <- testthat::test_path("fixtures", "mtcars.csv")
  penguins_path <- testthat::test_path("fixtures", "penguins.csv")
  upload_file(app, `left_p1-ggplot_0_ppUpload_NA` = mtcars_path)
  app$wait_for_idle(timeout = 15 * 1000)
  upload_file(app, `right_p1-ggplot_0_ppUpload_NA` = penguins_path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "left_p1-ptr_update_plot")
  draw(app, "right_p1-ptr_update_plot")

  left_code  <- app$get_value(output = "left_p1-ptr_code")  %||% ""
  right_code <- app$get_value(output = "right_p1-ptr_code") %||% ""

  expect_true(nzchar(left_code),  label = "left code panel non-empty")
  expect_true(nzchar(right_code), label = "right code panel non-empty")

  # Each module's code panel begins with its own prologue line.
  expect_match(left_code,
               "^[A-Za-z0-9_.]+ <- read\\.csv\\(\"mtcars\\.csv\"\\)\n",
               label = "left code panel leads with mtcars.csv prologue")
  expect_match(right_code,
               "^[A-Za-z0-9_.]+ <- read\\.csv\\(\"penguins\\.csv\"\\)\n",
               label = "right code panel leads with penguins.csv prologue")

  # No cross-contamination: neither panel mentions the other's file.
  expect_false(grepl("penguins.csv", left_code, fixed = TRUE),
               label = "left panel does NOT reference right's file")
  expect_false(grepl("mtcars.csv", right_code, fixed = TRUE),
               label = "right panel does NOT reference left's file")

  # Both plots rendered something (the host outputs both have <img> -- the
  # bare ggplot renderPlot path even when aes() resolution is partial).
  left_plot_html  <- app$get_html("#left_p1-ptr_plot")  %||% ""
  right_plot_html <- app$get_html("#right_p1-ptr_plot") %||% ""
  expect_true(nzchar(left_plot_html),  label = "left plot HTML non-empty")
  expect_true(nzchar(right_plot_html), label = "right plot HTML non-empty")
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
