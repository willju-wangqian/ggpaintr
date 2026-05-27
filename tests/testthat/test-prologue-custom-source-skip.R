# test-prologue-custom-source-skip.R -- shinytest2 driver for ADR 0025 §4 /
# PLAN-04. Worked observable: a custom source registered with
# `shortcut = TRUE` but no `prologue_emit_fn` hook (i.e. all custom sources
# today, since the hook is explicitly deferred per ADR scope cuts) emits
# NO prologue line. Only the substituted formula appears in the panel.

test_that("custom source (shortcut = TRUE) without prologue hook emits no prologue", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("prologue-custom-source-skip")
  draw(app, "ptr_update_plot")

  code_text <- app$get_value(output = "ptr_code") %||% ""
  expect_true(nzchar(code_text),
              label = "code panel text non-empty (formula deparse present)")

  # No reader-fn prologue pattern (built-in or otherwise) for the custom
  # source. The ppDataset textbox is seeded with "mtcars" and binds
  # `mtcars` into eval_env, but the code panel does NOT prepend a
  # `mtcars <- read.csv(...)` style line.
  expect_false(grepl(" <- read\\.csv\\(",       code_text), label = "no read.csv prologue")
  expect_false(grepl(" <- read\\.delim\\(",     code_text), label = "no read.delim prologue")
  expect_false(grepl(" <- readRDS\\(",          code_text), label = "no readRDS prologue")
  expect_false(grepl(" <- readxl::read_excel\\(", code_text),
               label = "no readxl::read_excel prologue")

  # The substituted formula body still references the bound `mtcars` symbol.
  expect_match(code_text, "mtcars",
               label = "substituted formula references the bound `mtcars` symbol")
})

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
