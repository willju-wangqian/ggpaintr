test_that("documented supported use cases parse, complete, and build", {
  for (case_name in names(supported_use_cases)) {
    case <- supported_use_cases[[case_name]]
    obj <- ggpaintr_formula(case$formula)
    result <- ggpaintr_complete_expr(obj, case$input, envir = case$envir)
    plot_obj <- ggpaintr_get_plot(result$complete_expr_list, envir = result$eval_env)

    expect_s3_class(plot_obj, "ggplot")
    expect_false(grepl("_NULL_PLACEHOLDER", result$code_text), info = case_name)
  }
})
