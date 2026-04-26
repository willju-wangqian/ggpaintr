restore_options <- function(envir = parent.frame()) {
  withr::local_options(
    list(
      ggpaintr.verbose = NULL,
      ggpaintr.checkbox_default_all_other_layer = NULL
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
  expect_setequal(names(out), c("verbose", "checkbox_default_all_other_layer"))
  expect_false(out$verbose)
  expect_true(out$checkbox_default_all_other_layer)
})

test_that("ptr_options() reflects values set via base options()", {
  withr::local_options(
    ggpaintr.verbose = FALSE,
    ggpaintr.checkbox_default_all_other_layer = FALSE
  )
  out <- ptr_options()
  expect_false(out$verbose)
  expect_false(out$checkbox_default_all_other_layer)
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
  ptr_options(verbose = TRUE, checkbox_default_all_other_layer = TRUE)
  old <- ptr_options(
    verbose = FALSE,
    checkbox_default_all_other_layer = FALSE
  )
  expect_false(getOption("ggpaintr.verbose"))
  do.call(ptr_options, old)
  expect_true(getOption("ggpaintr.verbose"))
  expect_true(getOption("ggpaintr.checkbox_default_all_other_layer"))
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
# checkbox_default_all_other_layer threads into resolver
# ---------------------------------------------------------------------------

test_that("checkbox_default_all_other_layer = FALSE makes unspecified layers start unchecked", {
  withr::local_options(ggpaintr.checkbox_default_all_other_layer = FALSE)
  expr_list <- list(ggplot = NULL, geom_point = NULL, geom_smooth = NULL)
  result <- ptr_resolve_checkbox_defaults(NULL, expr_list)
  expect_identical(result, c(geom_point = FALSE, geom_smooth = FALSE))
})

test_that("per-call checkbox_defaults overrides the global fallback", {
  withr::local_options(ggpaintr.checkbox_default_all_other_layer = FALSE)
  expr_list <- list(ggplot = NULL, geom_point = NULL, geom_smooth = NULL)
  result <- ptr_resolve_checkbox_defaults(
    list(geom_point = TRUE),
    expr_list
  )
  expect_identical(result, c(geom_point = TRUE, geom_smooth = FALSE))
})

test_that("short vector inside duplicate group pads with the global fallback", {
  withr::local_options(ggpaintr.checkbox_default_all_other_layer = FALSE)
  expr_list <- list(
    ggplot = NULL, geom_point = NULL, "geom_point-2" = NULL, "geom_point-3" = NULL
  )
  result <- ptr_resolve_checkbox_defaults(
    list(geom_point = c(TRUE)),
    expr_list
  )
  expect_identical(
    result,
    c(geom_point = TRUE, "geom_point-2" = FALSE, "geom_point-3" = FALSE)
  )
})

test_that("default behavior (option unset) is still all-TRUE for backward compat", {
  restore_options()
  expr_list <- list(ggplot = NULL, geom_point = NULL, geom_smooth = NULL)
  result <- ptr_resolve_checkbox_defaults(NULL, expr_list)
  expect_identical(result, c(geom_point = TRUE, geom_smooth = TRUE))
})
