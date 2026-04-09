test_that("ggpaintr_formula parses formulas into a paintr object", {
  formula <- paste(
    "ggplot(data = mtcars, aes(x = var, y = var)) +",
    "geom_point() +",
    "geom_point() +",
    "labs(title = text)"
  )

  obj <- ggpaintr_formula(formula)

  expect_s3_class(obj, "ggpaintr_obj")
  expect_identical(obj$formula_text, formula)
  expect_named(
    obj$expr_list,
    c("ggplot", "geom_point-1", "geom_point-2", "labs")
  )
  expect_true("ggplot+2" %in% names(obj$keywords_list$ggplot))
  expect_true("ggplot+3+2" %in% names(obj$keywords_list$ggplot))
  expect_true("ggplot+3+3" %in% names(obj$keywords_list$ggplot))
  expect_identical(
    unname(vapply(obj$keywords_list$ggplot, rlang::as_string, character(1))),
    c("mtcars", "var", "var")
  )
})

test_that("ggpaintr_formula detects every placeholder type", {
  formula <- paste(
    "ggplot(data = upload, aes(x = var, y = var)) +",
    "geom_point(size = num) +",
    "labs(title = text) +",
    "facet_wrap(expr)"
  )

  obj <- ggpaintr_formula(formula)

  keyword_strings <- unlist(lapply(
    obj$keywords_list,
    function(keyword_list) vapply(keyword_list, rlang::as_string, character(1))
  ), use.names = FALSE)

  expect_true(all(c("upload", "var", "text", "num", "expr") %in% keyword_strings))
})

test_that("quoted placeholders are not treated as placeholders", {
  obj <- ggpaintr_formula(
    'ggplot(data = mtcars, aes(x = "var", y = "var")) + geom_point()'
  )

  expect_false(any(vapply(obj$keywords_list$ggplot, rlang::as_string, character(1)) == "var"))
})
