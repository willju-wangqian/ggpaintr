# ADR 0020 / PLAN-01 SC1 — naked-R semantics of ppLayerOff / ppVerbOff.
#
# Outside `ptr_app()` the wrappers behave per their function body so naked-
# ggplot / naked-dplyr scripts still render. `ppLayerOff(layer_expr, TRUE)`
# is NULL; `ppLayerOff(layer_expr, FALSE)` evaluates the layer. `ppVerbOff(
# .data, verb_expr, TRUE)` returns `.data` unchanged; `ppVerbOff(.data,
# verb_expr, FALSE)` routes `.data` through the verb call.

test_that("ppLayerOff(geom_point(), TRUE) returns NULL", {
  result <- ppLayerOff(ggplot2::geom_point(), TRUE)
  expect_null(result)
})

test_that("ppLayerOff(geom_point(), FALSE) returns the layer", {
  result <- ppLayerOff(ggplot2::geom_point(), FALSE)
  expect_s3_class(result, "Layer")
  # And the layer composes onto a ggplot without error.
  p <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = wt)) + result
  expect_s3_class(p, "ggplot")
})

test_that("ppLayerOff default for hide is TRUE", {
  # No explicit hide argument; the function-body default must be TRUE so
  # the formula's "off by default" semantics are the dominant reading.
  expect_null(ppLayerOff(ggplot2::geom_point()))
})

test_that("ppVerbOff(mtcars, mutate(...), TRUE) returns mtcars unchanged", {
  skip_if_not_installed("dplyr")
  result <- ppVerbOff(mtcars, dplyr::mutate(mpg = mpg + 100), TRUE)
  expect_identical(result, mtcars)
})

test_that("ppVerbOff(mtcars, mutate(...), FALSE) routes .data through the verb", {
  skip_if_not_installed("dplyr")
  result <- ppVerbOff(mtcars, dplyr::mutate(mpg = mpg + 100), FALSE)
  expect_equal(result$mpg[[1L]], mtcars$mpg[[1L]] + 100)
  expect_equal(nrow(result), nrow(mtcars))
})

test_that("ppVerbOff default for hide is TRUE", {
  skip_if_not_installed("dplyr")
  result <- ppVerbOff(mtcars, dplyr::mutate(mpg = mpg + 100))
  expect_identical(result, mtcars)
})

test_that("ppLayerOff rejects non-logical hide at naked-R call time", {
  expect_error(
    ppLayerOff(ggplot2::geom_point(), hide = "yes"),
    "hide"
  )
})

test_that("ppVerbOff rejects non-logical hide at naked-R call time", {
  skip_if_not_installed("dplyr")
  expect_error(
    ppVerbOff(mtcars, dplyr::mutate(mpg = mpg + 1), hide = NA),
    "hide"
  )
})
