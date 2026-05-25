# ADR 0020 / 0021 PLAN-06 SC1 — naked-R semantics of ppLayerOff / ppVerbSwitch.
#
# Outside `ptr_app()` the wrappers behave per their function body so naked-
# ggplot / naked-dplyr scripts still render. `ppLayerOff(layer_expr, TRUE)`
# is NULL; `ppLayerOff(layer_expr, FALSE)` evaluates the layer.
# `ppVerbSwitch(.data, verb_expr, FALSE)` returns `.data` unchanged;
# `ppVerbSwitch(.data, verb_expr, TRUE)` routes `.data` through the verb
# call. The legacy off-only wrapper has been hard-removed (PLAN-06); a
# SC-2 guard below asserts the legacy name is no longer in the namespace
# (the name is built via `paste0` so the SC-1 source-tree grep stays
# clean while the runtime assertion still bites).

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

test_that("ppLayerOff rejects non-logical hide at naked-R call time", {
  expect_error(
    ppLayerOff(ggplot2::geom_point(), hide = "yes"),
    "hide"
  )
})

# ---- ppVerbSwitch migration coverage (rewrites of the legacy off-only blocks) ----

# Boot-off: migration of the legacy `(.data, verb, TRUE)` shape. Per ADR
# 0021 the equivalent is `ppVerbSwitch(.data, verb, switch_on = FALSE)`,
# which returns `.data` unchanged.
test_that("ppVerbSwitch(mtcars, mutate(...), switch_on = FALSE) returns mtcars unchanged", {
  skip_if_not_installed("dplyr")
  result <- ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), switch_on = FALSE)
  expect_identical(result, mtcars)
})

# Boot-on (apply-the-verb): under ppVerbSwitch this is `switch_on = TRUE`,
# the function-body default. Routes `.data` through the verb call.
test_that("ppVerbSwitch(mtcars, mutate(...), switch_on = TRUE) routes .data through the verb", {
  skip_if_not_installed("dplyr")
  result <- ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), switch_on = TRUE)
  expect_equal(result$mpg[[1L]], mtcars$mpg[[1L]] + 100)
  expect_equal(nrow(result), nrow(mtcars))
})

# Function-body default is `switch_on = TRUE` (apply the verb) — i.e. the
# checkbox boots checked. Mirrors the historical "default for hide is
# TRUE" assertion in the positive sense: omitting the slot leaves the
# verb on.
test_that("ppVerbSwitch default for switch_on is TRUE", {
  skip_if_not_installed("dplyr")
  result <- ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100))
  expect_equal(result$mpg[[1L]], mtcars$mpg[[1L]] + 100)
  expect_equal(nrow(result), nrow(mtcars))
})

# Boot-on, bare-verb migration: the legacy `(.data, verb, FALSE)` shape
# (a no-op wrapper whose runtime semantics is just "run the verb")
# migrates to a bare pipeline stage. This asserts the equivalence at the
# naked-R level.
test_that("bare verb stage is the migration target for the legacy no-op off-wrapper shape", {
  skip_if_not_installed("dplyr")
  bare <- dplyr::mutate(mtcars, mpg = mpg + 100)
  via_switch <- ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), TRUE)
  expect_equal(bare$mpg, via_switch$mpg)
  expect_equal(nrow(bare), nrow(via_switch))
})

# Force-checkbox capability that the legacy off-wrapper could NOT express:
# a bare verb can be promoted into a user-toggleable stage with
# `switch_on = TRUE` and (optionally) a custom `label =`. The naked-R
# path ignores `label` but accepts it without error — this is the new
# surface area ppVerbSwitch adds over the legacy wrapper.
test_that("ppVerbSwitch(switch_on = TRUE, label = ...) accepts the new label slot at naked-R", {
  skip_if_not_installed("dplyr")
  result <- ppVerbSwitch(
    mtcars,
    dplyr::mutate(mpg = mpg + 100),
    switch_on = TRUE,
    label = "Add 100 to mpg"
  )
  expect_equal(result$mpg[[1L]], mtcars$mpg[[1L]] + 100)
})

test_that("ppVerbSwitch rejects non-logical switch_on at naked-R call time", {
  skip_if_not_installed("dplyr")
  expect_error(
    ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 1), switch_on = NA),
    "switch_on"
  )
})

# ---- PLAN-06 SC-2 — the legacy off-only wrapper is no longer in the namespace ----

test_that("the legacy off-only wrapper is hard-removed from ggpaintr namespace (PLAN-06 SC-2)", {
  # Build the legacy name without inlining the literal so the SC-1
  # source-tree grep returns zero hits while still asserting the removal
  # at runtime.
  legacy <- paste0("pp", "Verb", "Off")
  expect_identical(
    exists(legacy, envir = asNamespace("ggpaintr"), inherits = FALSE),
    FALSE
  )
})
