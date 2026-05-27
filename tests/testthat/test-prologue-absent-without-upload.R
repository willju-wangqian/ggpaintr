# test-prologue-absent-without-upload.R -- shinytest2 driver for ADR 0025 §4
# / PLAN-04. Worked observable: with NO ppUpload in the formula the code
# panel emits no prologue line; the rendered text is identical to today's
# behaviour (substituted formula alone).

test_that("no prologue when no upload is active", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-absent-without-upload")
  draw(app, "ptr_update_plot")

  code_text <- app$get_value(output = "ptr_code") %||% ""
  expect_true(nzchar(code_text),
              label = "code panel text is non-empty post-Update")

  # No reader-fn pattern of the form `<sym> <- read.*(`, `readRDS(` or
  # `readxl::read_excel(` anywhere in the panel.
  expect_false(grepl(" <- read\\.csv\\(",  code_text), label = "no read.csv prologue")
  expect_false(grepl(" <- read\\.delim\\(", code_text), label = "no read.delim prologue")
  expect_false(grepl(" <- readRDS\\(",      code_text), label = "no readRDS prologue")
  expect_false(grepl(" <- readxl::read_excel\\(", code_text),
               label = "no readxl::read_excel prologue")

  # The body still contains the substituted formula's data symbol (`mtcars`).
  expect_match(code_text, "mtcars",
               label = "substituted formula references the in-scope `mtcars` data symbol")
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
