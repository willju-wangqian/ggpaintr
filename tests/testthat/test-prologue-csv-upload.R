# test-prologue-csv-upload.R -- shinytest2 driver for ADR 0025 §4 / PLAN-04.
# Worked observable: after uploading a csv to a `ppUpload() |> ggplot(...)`
# app and clicking Update, the code panel leads with the prologue line
# `<node$auto_name> <- read.csv("mtcars.csv")\n` followed by the substituted
# formula. Asserts the literal prologue prefix + the auto-name token on the
# subsequent line, per the BDD scenario.
#
# Gating + boot follow `.claude/rules/testing.md` "Browser e2e (shinytest2)"
# (skip_on_cran / skip_if_not_installed; app-dir + pkgload::load_all in the
# fixture; never get_values(); wait_=FALSE for placeholder writes).

test_that("csv upload emits read.csv prologue with auto-name", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-csv-upload")

  src_id <- "ggplot_0_ppUpload_NA"
  expect_dom_id(app, src_id)

  csv_path <- testthat::test_path("fixtures", "mtcars.csv")
  app$upload_file(ggplot_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  code_text <- app$get_value(output = "ptr_code")
  expect_true(is.character(code_text) && length(code_text) == 1L && nzchar(code_text),
              label = "ptr_code panel returns a single non-empty string")

  # ADR 0025 §4: leading prologue line of the form
  # `<syntactic R name> <- read.csv("mtcars.csv")\n`
  expect_match(code_text,
               "^[A-Za-z0-9_.]+ <- read\\.csv\\(\"mtcars\\.csv\"\\)\n",
               label = "code panel begins with the prologue line")

  # The auto-name LHS for a pipeline-head `ppUpload()` (no shared key) ends
  # with the structural `_ppUpload_NA` suffix per paintr-ids.R.
  prologue_line <- sub("\n.*$", "", code_text)
  expect_match(prologue_line,
               "_ppUpload_NA <- read\\.csv\\(\"mtcars\\.csv\"\\)$",
               label = "prologue LHS equals node$auto_name (ends with _ppUpload_NA)")

  # The substituted formula on the second line references the same auto-name
  # symbol. We don't pin the exact deparse shape (the ggplot deparse uses
  # `data = <name>`, not the pipe form), only that the symbol appears.
  body_text <- sub("^[^\n]*\n", "", code_text)
  expect_match(body_text, src_id,
               label = "substituted formula body references the auto-name symbol")
})
