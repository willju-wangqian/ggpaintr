# Internal headless runtime (ptr_run_formula / ptr_exec_headless). These run
# the formula -> plot pipeline without a Shiny session; devtools::test() loads
# internals so they're callable directly.

.headless_env <- function() list2env(list(mtcars = mtcars), parent = globalenv())

test_that("ptr_run_formula renders a static point plot", {
  res <- ptr_run_formula(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
    envir = .headless_env()
  )
  expect_true(isTRUE(res$ok))
  expect_equal(res$stage, "complete")
  expect_s3_class(res$plot, "ggplot")
  expect_match(res$code_text, "geom_point")
})

test_that("ptr_run_formula handles bar + facet formulas", {
  bar <- ptr_run_formula(
    "ggplot(mtcars) + geom_bar(aes(x = factor(cyl)))",
    envir = .headless_env()
  )
  expect_true(isTRUE(bar$ok))
  expect_s3_class(bar$plot, "ggplot")

  facet <- ptr_run_formula(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + facet_wrap(~ cyl)",
    envir = .headless_env()
  )
  expect_true(isTRUE(facet$ok))
  expect_match(facet$code_text, "facet_wrap")
})

test_that("ptr_run_formula resolves a `var` chain from supplied inputs", {
  fml <- "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  res <- ptr_run_formula(
    fml,
    inputs = list(ggplot_1_1_var_NA = "mpg", ggplot_1_2_var_NA = "hp"),
    envir = .headless_env()
  )
  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
  expect_match(res$code_text, "x = mpg")
  expect_match(res$code_text, "y = hp")
})

test_that("ptr_run_formula completes a `labs(title = text())` formula with defaults", {
  res <- ptr_run_formula(
    "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + labs(title = text())",
    envir = .headless_env()
  )
  # text() with no supplied value is pruned -> the labs() call drops out.
  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
})

test_that("ptr_run_formula reports a failed plot render (missing required aes)", {
  res <- ptr_run_formula(
    "ggplot(mtcars) + geom_point()",
    envir = .headless_env()
  )
  expect_false(isTRUE(res$ok))
  expect_equal(res$stage, "plot")
  expect_true(nzchar(res$error))
})

test_that("ptr_run_formula catches a disallowed `expr` value at the complete stage", {
  fml <- "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + ggtitle(label = expr())"
  res <- ptr_run_formula(
    fml,
    inputs = list(ggtitle_1_expr_NA = 'system("echo hi")'),
    envir = .headless_env()
  )
  expect_false(isTRUE(res$ok))
  expect_equal(res$stage, "complete")
  expect_match(res$error, "not allowed")

  # ...and an allowed value flows through to the rendered code.
  ok <- ptr_run_formula(
    fml,
    inputs = list(ggtitle_1_expr_NA = '"My title"'),
    envir = .headless_env()
  )
  expect_true(isTRUE(ok$ok))
  expect_match(ok$code_text, 'ggtitle\\(label = "My title"\\)')
})

test_that("ptr_default_snapshot defaults checkboxes/stage toggles on, others NULL", {
  tree <- ptr_translate("ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()")
  spec <- ptr_runtime_input_spec(tree)
  snap <- ptr_default_snapshot(spec, tree)
  expect_true(all(spec$input_id %in% names(snap)))
  cb <- spec$input_id[spec$role == "layer_checkbox"]
  expect_true(all(vapply(snap[cb], isTRUE, logical(1))))
  vars <- spec$input_id[spec$role == "placeholder"]
  expect_true(all(vapply(snap[vars], is.null, logical(1))))
})

test_that("ptr_exec_headless honours stage_enabled and extras", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  res <- ptr_exec_headless(
    tree, snapshot = list(), eval_env = .headless_env(),
    extras = list(ggplot2::theme_minimal())
  )
  expect_true(isTRUE(res$ok))
  expect_s3_class(res$plot, "ggplot")
})
