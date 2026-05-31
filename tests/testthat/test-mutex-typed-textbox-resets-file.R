# test-mutex-typed-textbox-resets-file.R -- shinytest2 driver for ADR 0025 §2
# (PLAN-03 / Q3-B). Covers the worked example #5 negative observable:
# typing into the shortcut textbox auto-clears the sibling fileInput so the
# concurrently-active file+typed-text state is structurally unreachable.
#
# Gate pattern + boot scaffolding follow `.claude/rules/testing.md` "Browser
# e2e (shinytest2) -- hard-won gotchas" (app-dir + pkgload::load_all in the
# fixture; never get_values(); set placeholder inputs with wait_=FALSE).

test_that("typing into shortcut textbox auto-clears the sibling fileInput", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("mutex-typed-textbox-resets-file")

  src_id      <- "ggplot_0_ppUpload_NA"
  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"
  expect_dom_id(app, src_id)
  expect_dom_id(app, shortcut_id)

  # Step 1: upload a file so the file pill appears.
  csv_path <- testthat::test_path(
    "fixtures", "vignette-apps", "mutex-typed-textbox-resets-file",
    "penguins.csv"
  )
  upload_file(app, ggplot_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 15 * 1000)

  # Step 2: type into the shortcut textbox. wait_=FALSE per project rule
  # (placeholder writes do not by themselves trigger an output update).
  set_input(app, shortcut_id, "mtcars")
  app$wait_for_idle(timeout = 15 * 1000)

  # Then 1: the fileInput's value has been reset to NULL. We query Shiny's
  # input registry directly via app$get_js() rather than app$get_value(input=),
  # which internally calls get_values() and 500s on custom-renderer apps in
  # a pre-draw silent.error state (project memory shinytest2-appdir-pkgload).
  # The BDD Then semantics ("file input is reset") are preserved.
  fi_val <- app$get_js(sprintf("Shiny.shinyapp.$inputValues['%s']", src_id))
  expect_true(
    is.null(fi_val) || identical(fi_val, "") || isTRUE(is.na(fi_val)),
    label = "fileInput reset to NULL after typing in shortcut textbox"
  )

  # Then 2: the file-name pill text is no longer rendered for the source.
  src_html <- app$get_html(paste0("#", src_id)) %||% ""
  expect_false(
    grepl("penguins.csv", src_html, fixed = TRUE),
    label = "rendered HTML for source widget no longer shows the file-name pill"
  )
})
