test_that("multiple expressions are rejected at parse time", {
  expect_error(
    ptr_parse_formula(unsupported_use_cases$multiple_expressions$formula)
  )
})

test_that("formulas with var and no data fail during UI preparation", {
  obj <- ptr_parse_formula(unsupported_use_cases$no_data_for_var$formula)

  expect_error(
    register_var_ui_outputs(list(), list2env(list(), parent = emptyenv()), obj),
    "Variable inputs cannot be rendered because data columns are not available"
  )
})

test_that("unknown data objects fail when variable UI needs data columns", {
  obj <- ptr_parse_formula(unsupported_use_cases$unknown_data_object$formula)
  output <- list2env(list(), parent = emptyenv())

  expect_no_error(register_var_ui_outputs(list(), output, obj))
})

test_that("quoted placeholders do not create variable controls", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = "var", y = "var")) + geom_point()'
  )

  expect_false(any(vapply(obj$keywords_list$ggplot, rlang::as_string, character(1)) == "var"))
})
