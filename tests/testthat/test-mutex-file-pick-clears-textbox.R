# test-mutex-file-pick-clears-textbox.R -- shinytest2 driver for ADR 0025 §2
# (PLAN-03 / Q3-B). The flip side of the mutex: picking a file
# auto-clears the sibling shortcut textbox, so a pre-typed name cannot
# coexist with an active upload.

test_that("picking a file auto-clears the shortcut textbox", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("mutex-file-pick-clears-textbox")

  src_id      <- "ggplot_0_ppUpload_NA"
  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"
  expect_dom_id(app, src_id)
  expect_dom_id(app, shortcut_id)

  # Step 1: type a name into the shortcut textbox.
  set_input(app, shortcut_id, "mtcars")
  app$wait_for_idle(timeout = 15 * 1000)
  expect_equal(
    app$get_value(input = shortcut_id), "mtcars",
    label = "shortcut textbox starts at the typed value"
  )

  # Step 2: pick a file. `upload_file` runs the same path as the browser
  # picker, which fires the file-pick observer.
  csv_path <- testthat::test_path(
    "fixtures", "vignette-apps", "mutex-file-pick-clears-textbox",
    "penguins.csv"
  )
  upload_file(app, ggplot_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 15 * 1000)

  # Then: the shortcut textbox has been wiped to the empty string
  # (updateTextInput(..., value = "")).
  expect_equal(
    app$get_value(input = shortcut_id), "",
    label = "shortcut textbox cleared to empty string after file pick"
  )
})
