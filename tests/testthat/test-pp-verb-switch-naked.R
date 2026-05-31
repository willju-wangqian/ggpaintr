# ADR 0021 / PLAN-02 — naked-R semantics of ppVerbSwitch.
#
# Outside `ptr_app()` the wrapper behaves per its function body so naked-
# dplyr scripts still render. `ppVerbSwitch(.data, verb_expr, TRUE)`
# routes `.data` through the verb call; `ppVerbSwitch(.data, verb_expr,
# FALSE)` returns `.data` unchanged. The `label` argument is metadata-
# only outside `ptr_app()`.

test_that("SC-1: ppVerbSwitch(mtcars, filter(mpg > 20), TRUE) has 14 rows", {
  skip_if_not_installed("dplyr")
  result <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), switch_on = TRUE)
  expect_equal(nrow(result), 14L)
})

test_that("SC-2: ppVerbSwitch(mtcars, filter(mpg > 20), FALSE) returns mtcars unchanged", {
  skip_if_not_installed("dplyr")
  expect_identical(
    ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), FALSE),
    mtcars
  )
})

test_that("SC-3: ppVerbSwitch(mtcars, mutate(...), TRUE) applies the mutation", {
  skip_if_not_installed("dplyr")
  result <- ppVerbSwitch(mtcars, dplyr::mutate(mpg = mpg + 100), TRUE)
  expect_equal(result$mpg[[1L]], mtcars$mpg[[1L]] + 100)
})

test_that("SC-4: switch_on validation rejects NA / length-2 logical / character", {
  skip_if_not_installed("dplyr")
  expect_error(
    ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), switch_on = NA),
    "`switch_on` must be a length-1 non-NA logical.",
    fixed = TRUE
  )
  expect_error(
    ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), switch_on = c(TRUE, FALSE)),
    "`switch_on` must be a length-1 non-NA logical.",
    fixed = TRUE
  )
  expect_error(
    ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), switch_on = "yes"),
    "`switch_on` must be a length-1 non-NA logical.",
    fixed = TRUE
  )
})

test_that("SC-5: non-call verb_expr is rejected with the ppVerbSwitch message", {
  expect_error(
    ppVerbSwitch(mtcars, mtcars, FALSE),
    "`ppVerbSwitch(verb_expr = )` must be a verb call, e.g. `mutate(x = 1)`.",
    fixed = TRUE
  )
})

test_that("SC-6: label is metadata-only outside ptr_app", {
  skip_if_not_installed("dplyr")
  with_label <- ppVerbSwitch(
    mtcars, dplyr::filter(mpg > 20), TRUE, label = "X"
  )
  without_label <- ppVerbSwitch(mtcars, dplyr::filter(mpg > 20), TRUE)
  expect_identical(with_label, without_label)
})
