test_that("ptr_parse_formula parses formulas into a paintr object", {
  formula <- paste(
    "ggplot(data = mtcars, aes(x = var, y = var)) +",
    "geom_point() +",
    "geom_point() +",
    "labs(title = text)"
  )

  obj <- ptr_parse_formula(formula)

  expect_s3_class(obj, "ptr_obj")
  expect_identical(obj$formula_text, formula)
  expect_named(
    obj$expr_list,
    c("ggplot", "geom_point", "geom_point-2", "labs")
  )
  # 'data = mtcars' is NOT collected (mtcars is not a placeholder keyword)
  expect_false("ggplot_2" %in% names(obj$keywords_list$ggplot))
  expect_true("ggplot_3_2" %in% names(obj$keywords_list$ggplot))
  expect_true("ggplot_3_3" %in% names(obj$keywords_list$ggplot))
  expect_identical(
    unname(vapply(obj$keywords_list$ggplot, rlang::as_string, character(1))),
    c("var", "var")
  )
})

test_that("ptr_parse_formula detects every placeholder type", {
  formula <- paste(
    "ggplot(data = upload, aes(x = var, y = var)) +",
    "geom_point(size = num) +",
    "labs(title = text) +",
    "facet_wrap(expr)"
  )

  obj <- ptr_parse_formula(formula)

  keyword_strings <- unlist(lapply(
    obj$keywords_list,
    function(keyword_list) vapply(keyword_list, rlang::as_string, character(1))
  ), use.names = FALSE)

  expect_true(all(c("upload", "var", "text", "num", "expr") %in% keyword_strings))
})

test_that("quoted placeholders are not treated as placeholders", {
  obj <- ptr_parse_formula(
    'ggplot(data = mtcars, aes(x = "var", y = "var")) + geom_point()'
  )

  expect_false(any(vapply(obj$keywords_list$ggplot, rlang::as_string, character(1)) == "var"))
})

test_that("ptr_parse_formula accepts a bare-symbol placeholder as a trailing layer", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + expr"
  )

  expect_true("expr" %in% names(obj$expr_list))
  expect_identical(obj$expr_list$expr, rlang::sym("expr"))

  meta <- obj$placeholder_map$expr$expr
  expect_identical(meta$keyword, "expr")
  expect_identical(meta$layer_name, "expr")
  expect_identical(meta$index_path, integer(0))
  expect_null(meta$param)
})

test_that("bare-symbol expr layer round-trips through ptr_complete_expr", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + expr"
  )

  base_input <- list()
  base_input[["geom_point_checkbox"]] <- TRUE
  base_input[["expr_checkbox"]] <- TRUE

  # Empty input: layer silently dropped from code pane
  empty_input <- base_input
  empty_input[["expr"]] <- ""
  res_empty <- ptr_complete_expr(obj, empty_input)
  expect_false(grepl("_NULL_PLACEHOLDER", res_empty$code_text, fixed = TRUE))
  expect_false("expr" %in% names(res_empty$complete_expr_list))

  # User-provided expression: spliced verbatim
  filled_input <- base_input
  filled_input[["expr"]] <- "theme_minimal(base_size = 14)"
  res_filled <- ptr_complete_expr(obj, filled_input)
  expect_true(grepl("theme_minimal", res_filled$code_text, fixed = TRUE))
  expect_identical(
    res_filled$complete_expr_list$expr,
    rlang::expr(theme_minimal(base_size = 14))
  )

  # Checkbox off: trailing layer dropped
  off_input <- filled_input
  off_input[["expr_checkbox"]] <- FALSE
  res_off <- ptr_complete_expr(obj, off_input)
  expect_false("expr" %in% names(res_off$complete_expr_list))
})

test_that("bare-symbol expr layer survives when resolved to a zero-arg call", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = wt, y = mpg)) + geom_point() + expr"
  )
  input <- list(
    "geom_point_checkbox" = TRUE,
    "expr_checkbox"       = TRUE,
    "expr"                = "theme_minimal()"
  )
  res <- ptr_complete_expr(obj, input)
  expect_identical(
    res$complete_expr_list$expr,
    rlang::expr(theme_minimal())
  )
  expect_true(grepl("theme_minimal()", res$code_text, fixed = TRUE))
})

test_that("ptr_parse_formula accepts a single-layer formula with no `+`", {
  obj <- ptr_parse_formula("ggplot(mtcars, aes(x = wt, y = mpg))")
  expect_s3_class(obj, "ptr_obj")
  expect_named(obj$expr_list, "ggplot")
  expect_null(obj$ggplot_pipe_op)
})

test_that("ptr_parse_formula accepts native pipe formulas", {
  obj <- ptr_parse_formula(
    "mtcars |> ggplot(aes(x = wt, y = mpg)) + geom_point()"
  )
  expect_named(obj$expr_list, c("ggplot", "geom_point"))
  expect_identical(obj$ggplot_pipe_op, "|>")
  expect_identical(obj$expr_list$ggplot[[2]], rlang::sym("mtcars"))
})

test_that("ptr_parse_formula accepts native pipe formulas with a single layer", {
  obj <- ptr_parse_formula("mtcars |> ggplot(aes(x = wt, y = mpg))")
  expect_named(obj$expr_list, "ggplot")
  expect_identical(obj$ggplot_pipe_op, "|>")
})

test_that("ptr_parse_formula rewrites %>% into nested calls", {
  obj <- ptr_parse_formula(
    "mtcars %>% ggplot(aes(x = wt, y = mpg)) + geom_point()"
  )
  expect_named(obj$expr_list, c("ggplot", "geom_point"))
  expect_identical(obj$ggplot_pipe_op, "%>%")
  expect_identical(obj$expr_list$ggplot[[2]], rlang::sym("mtcars"))
})

test_that("code panel reproduces native pipe surface form", {
  obj <- ptr_parse_formula(
    "mtcars |> ggplot(aes(x = wt, y = mpg)) + geom_point()"
  )
  res <- ptr_complete_expr(obj, list(geom_point_checkbox = TRUE))
  expect_true(grepl("mtcars |> ggplot(", res$code_text, fixed = TRUE))
  expect_false(grepl("ggplot(mtcars,", res$code_text, fixed = TRUE))
})

test_that("code panel reproduces magrittr pipe surface form", {
  obj <- ptr_parse_formula(
    "mtcars %>% ggplot(aes(x = wt, y = mpg)) + geom_point()"
  )
  res <- ptr_complete_expr(obj, list(geom_point_checkbox = TRUE))
  expect_true(grepl("mtcars %>% ggplot(", res$code_text, fixed = TRUE))
})

test_that("ptr_parse_formula records the full pipe chain feeding ggplot", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = wt, y = mpg))"
  )
  expect_identical(obj$ggplot_pipe_chain_ops, c("|>", "|>"))
  expect_identical(obj$ggplot_pipe_op, "|>")

  obj2 <- ptr_parse_formula(
    "mtcars %>% head(num) %>% ggplot(aes(x = wt, y = mpg))"
  )
  expect_identical(obj2$ggplot_pipe_chain_ops, c("%>%", "%>%"))

  obj3 <- ptr_parse_formula(
    "mtcars %>% head(num) |> ggplot(aes(x = wt, y = mpg))"
  )
  expect_identical(obj3$ggplot_pipe_chain_ops, c("%>%", "|>"))

  obj4 <- ptr_parse_formula("ggplot(mtcars, aes(x = wt, y = mpg))")
  expect_identical(obj4$ggplot_pipe_chain_ops, character())
})

test_that("code panel preserves a chained native-pipe pipeline", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = wt, y = mpg))"
  )
  res <- ptr_complete_expr(obj, list("ggplot_2_3" = 5L))
  expect_true(grepl("mtcars |> head(", res$code_text, fixed = TRUE))
  expect_true(grepl(") |> ggplot(", res$code_text, fixed = TRUE))
  expect_false(grepl("head(mtcars,", res$code_text, fixed = TRUE))
  expect_false(grepl("ggplot(head", res$code_text, fixed = TRUE))
})

test_that("code panel preserves a chained magrittr pipeline", {
  obj <- ptr_parse_formula(
    "mtcars %>% head(num) %>% ggplot(aes(x = wt, y = mpg))"
  )
  res <- ptr_complete_expr(obj, list("ggplot_2_3" = 5L))
  expect_true(grepl("mtcars %>% head(", res$code_text, fixed = TRUE))
  expect_true(grepl(") %>% ggplot(", res$code_text, fixed = TRUE))
})

test_that("code panel preserves a mixed pipe chain", {
  obj <- ptr_parse_formula(
    "mtcars %>% head(num) |> ggplot(aes(x = wt, y = mpg))"
  )
  res <- ptr_complete_expr(obj, list("ggplot_2_3" = 5L))
  expect_true(grepl("mtcars %>% head(", res$code_text, fixed = TRUE))
  expect_true(grepl(") |> ggplot(", res$code_text, fixed = TRUE))
})

test_that("chained pipe drops middle-link arg when its placeholder is empty", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> ggplot(aes(x = wt, y = mpg))"
  )
  res <- ptr_complete_expr(obj, list())
  expect_true(grepl("mtcars |> head() |> ggplot(", res$code_text, fixed = TRUE))
})

test_that("chained pipe rendering does not re-introduce comments from source", {
  obj <- ptr_parse_formula(
    "mtcars |> head(num) |> # trim rows\n ggplot(aes(x = wt, y = mpg))"
  )
  res <- ptr_complete_expr(obj, list("ggplot_2_3" = 5L))
  expect_false(grepl("trim rows", res$code_text, fixed = TRUE))
  expect_true(grepl("mtcars |> head(", res$code_text, fixed = TRUE))
})
