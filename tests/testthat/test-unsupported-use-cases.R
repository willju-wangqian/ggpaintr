test_that("multiple expressions are rejected at parse time", {
  expect_error(
    ptr_parse_formula(unsupported_use_cases$multiple_expressions$formula)
  )
})

test_that("formulas with var and no data silently skip during UI preparation", {
  obj <- ptr_parse_formula(unsupported_use_cases$no_data_for_var$formula)

  expect_no_error(
    register_var_ui_outputs(list(), list2env(list(), parent = emptyenv()), obj)
  )
})

test_that("unknown data objects fail when variable UI needs data columns", {
  obj <- ptr_parse_formula(unsupported_use_cases$unknown_data_object$formula)
  output <- list2env(list(), parent = emptyenv())

  expect_no_error(register_var_ui_outputs(list(), output, obj))
})
