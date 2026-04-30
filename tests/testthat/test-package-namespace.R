# Tests for package-namespace (`pkg::fn`) function heads in formulas.
# Regression for: rlang::as_string(x[[1]]) failing when x[[1]] is a `::` call.

test_that("ptr_parse_formula handles `pkg::fn()` top-level layers", {
  formula <- paste(
    "ggplot(data = penguins,",
    "       aes(x = var, y = var, color = var)) +",
    "  ggbeeswarm::geom_quasirandom()"
  )

  obj <- ptr_parse_formula(formula)

  expect_s3_class(obj, "ptr_obj")
  expect_named(obj$expr_list, c("ggplot", "geom_quasirandom"))
})

test_that("ptr_parse_formula handles `:::` (triple colon) function heads", {
  formula <- "ggplot(data, aes(x = var)) + ggplot2:::ggplot()"

  obj <- ptr_parse_formula(formula)

  expect_s3_class(obj, "ptr_obj")
  expect_true("ggplot" %in% names(obj$expr_list) ||
                "ggplot-2" %in% names(obj$expr_list))
})

test_that("placeholders inside `pkg::fn(...)` are still detected", {
  formula <- "ggplot(data) + ggbeeswarm::geom_quasirandom(aes(x = var))"

  obj <- ptr_parse_formula(formula)

  expect_true("geom_quasirandom" %in% names(obj$expr_list))
  layer_keywords <- obj$keywords_list$geom_quasirandom
  expect_true(length(layer_keywords) >= 1L)
})

test_that("two different `pkg::fn` layers with the same bare name get unique ids", {
  formula <- paste(
    "ggplot(data) +",
    "ggbeeswarm::geom_quasirandom() +",
    "ggplot2::geom_quasirandom()"
  )

  obj <- ptr_parse_formula(formula)

  layer_names <- names(obj$expr_list)
  expect_equal(length(layer_names), length(unique(layer_names)))
  expect_true(any(grepl("^geom_quasirandom", layer_names)))
})

test_that("get_fun_names returns the bare name for `::` and `:::` heads", {
  expect_equal(get_fun_names(quote(ggbeeswarm::geom_quasirandom())),
               "geom_quasirandom")
  expect_equal(get_fun_names(quote(pkg:::internal())),
               "internal")
  expect_equal(get_fun_names(quote(geom_point())), "geom_point")
  expect_equal(get_fun_names(quote(my_var)), "my_var")
})
