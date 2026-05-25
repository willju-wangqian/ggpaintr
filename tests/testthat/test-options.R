restore_options <- function(envir = parent.frame()) {
  withr::local_options(
    list(
      ggpaintr.verbose = NULL
    ),
    .local_envir = envir
  )
}

# ---------------------------------------------------------------------------
# ptr_options() — getter
# ---------------------------------------------------------------------------

test_that("ptr_options() with no args returns current settings as named list", {
  restore_options()
  out <- ptr_options()
  expect_type(out, "list")
  expect_setequal(names(out), c("verbose"))
  expect_false(out$verbose)
})

test_that("ptr_options() reflects values set via base options()", {
  withr::local_options(
    ggpaintr.verbose = FALSE
  )
  out <- ptr_options()
  expect_false(out$verbose)
})

# ---------------------------------------------------------------------------
# ptr_options() — setter
# ---------------------------------------------------------------------------

test_that("ptr_options(name = value) sets the underlying option", {
  restore_options()
  ptr_options(verbose = FALSE)
  expect_false(getOption("ggpaintr.verbose"))
  expect_false(ptr_options()$verbose)
})

test_that("ptr_options() setting returns previous values invisibly", {
  restore_options()
  prev <- withVisible(ptr_options(verbose = TRUE))
  expect_false(prev$visible)
  expect_identical(prev$value$verbose, FALSE)
})

test_that("ptr_options() round-trip restores prior state", {
  restore_options()
  ptr_options(verbose = TRUE)
  old <- ptr_options(
    verbose = FALSE
  )
  expect_false(getOption("ggpaintr.verbose"))
  do.call(ptr_options, old)
  expect_true(getOption("ggpaintr.verbose"))
})

# ---------------------------------------------------------------------------
# ptr_options() — validation
# ---------------------------------------------------------------------------

test_that("ptr_options() errors on unknown setting name", {
  restore_options()
  expect_error(ptr_options(verbose = TRUE, nonsense = FALSE), "nonsense")
})

test_that("ptr_options() errors on non-logical value", {
  restore_options()
  expect_error(ptr_options(verbose = "off"), "logical")
  expect_error(ptr_options(verbose = NA), "NA|logical")
  expect_error(ptr_options(verbose = c(TRUE, FALSE)), "single")
})

# ---------------------------------------------------------------------------
# ADR 0020 / Plan 04: the deprecated `checkbox_default_all_other_layer`
# option was removed straight out — `ptr_options()` must reject the name.
# ---------------------------------------------------------------------------

test_that("ptr_options() errors on the removed checkbox_default_all_other_layer option", {
  restore_options()
  expect_error(
    ptr_options(checkbox_default_all_other_layer = FALSE),
    "checkbox_default_all_other_layer"
  )
  expect_false(
    "checkbox_default_all_other_layer" %in% names(ptr_options())
  )
})
