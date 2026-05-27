# test-prologue-reader-fn-mirror.R -- shinytest2 driver for ADR 0025 §4 /
# PLAN-04. Verifies the code-panel prologue's reader-fn name mirrors the
# `ptr_read_uploaded_data()` dispatch table at name-only resolution:
#   csv  -> read.csv
#   tsv  -> read.delim
#   xlsx -> readxl::read_excel
#   rds  -> readRDS
# The single source of truth is `reader_fn_name_for_ext()` in paintr-upload.R
# (refactored from `ptr_resolve_upload_info()`'s switch in PLAN-04). Each
# sub-test boots a fresh AppDriver, uploads the matching fixture, and asserts
# the leading prologue regex.
#
# Gating + boot per `.claude/rules/testing.md` "Browser e2e (shinytest2)".

prologue_first_line <- function(app) {
  txt <- app$get_value(output = "ptr_code") %||% ""
  sub("\n.*$", "", txt)
}

test_that("csv upload emits read.csv prologue", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-reader-fn-mirror")
  path <- testthat::test_path("fixtures", "mtcars.csv")
  app$upload_file(ggplot_0_ppUpload_NA = path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  expect_match(prologue_first_line(app),
               "^[A-Za-z0-9_.]+ <- read\\.csv\\(\"mtcars\\.csv\"\\)$",
               label = "csv -> read.csv prologue")
})

test_that("tsv upload emits read.delim prologue", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-reader-fn-mirror")
  path <- testthat::test_path("fixtures", "mtcars.tsv")
  app$upload_file(ggplot_0_ppUpload_NA = path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  expect_match(prologue_first_line(app),
               "^[A-Za-z0-9_.]+ <- read\\.delim\\(\"mtcars\\.tsv\"\\)$",
               label = "tsv -> read.delim prologue")
})

test_that("xlsx upload emits readxl::read_excel prologue", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  skip_if_not_installed("readxl")

  app <- boot_vignette_app("prologue-reader-fn-mirror")
  path <- testthat::test_path("fixtures", "mtcars.xlsx")
  app$upload_file(ggplot_0_ppUpload_NA = path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  expect_match(prologue_first_line(app),
               "^[A-Za-z0-9_.]+ <- readxl::read_excel\\(\"[^\"]+\\.xlsx\"\\)$",
               label = "xlsx -> readxl::read_excel prologue")
})

test_that("rds upload emits readRDS prologue", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-reader-fn-mirror")
  path <- testthat::test_path("fixtures", "mtcars.rds")
  app$upload_file(ggplot_0_ppUpload_NA = path)
  app$wait_for_idle(timeout = 15 * 1000)
  draw(app, "ptr_update_plot")

  expect_match(prologue_first_line(app),
               "^[A-Za-z0-9_.]+ <- readRDS\\(\"[^\"]+\\.rds\"\\)$",
               label = "rds -> readRDS prologue")
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
