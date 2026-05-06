# P11 — eval. Typed tree -> ggplot via rlang::call2 fold for pipelines;
# safety re-check per layer; no-layers errors out distinctly.

test_that("P11.1 plain ggplot evaluates to a ggplot object", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point()")
  s <- ptr_substitute(r)
  p <- ptr_prune(s)
  pl <- ptr_eval(p)
  expect_s3_class(pl, "ggplot")
})

test_that("P11.2 pipeline folds via desugar (head(mtcars, 2))", {
  r <- ptr_translate("mtcars |> head(2) |> ggplot(aes(x = mpg))")
  s <- ptr_substitute(r)
  p <- ptr_prune(s)
  pl <- ptr_eval(p)
  expect_s3_class(pl, "ggplot")
  # The plot's data should be the head(mtcars, 2) result.
  expect_equal(nrow(pl$data), 2L)
})

test_that("P11.3 empty plot list errors as no-layers", {
  empty <- ptr_root(layers = list(), expr = NULL)
  expect_error(ptr_eval(empty), "No layers")
})

test_that("P11.4 optional layers unchecked produce base ggplot only", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point() + geom_smooth()")
  point_id <- Filter(function(l) l$name == "geom_point", r$layers)[[1]]$active_input_id
  smooth_id <- Filter(function(l) l$name == "geom_smooth", r$layers)[[1]]$active_input_id
  s <- ptr_substitute(r, input_snapshot = setNames(list(FALSE, FALSE), c(point_id, smooth_id)))
  p <- ptr_prune(s)
  pl <- ptr_eval(p)
  expect_s3_class(pl, "ggplot")
  expect_equal(length(pl$layers), 0L)  # only the base ggplot, no geoms
})

test_that("P11 pkg::fn head evaluates", {
  r <- ptr_translate("ggplot2::ggplot(mtcars, aes(x = mpg)) + ggplot2::geom_point()")
  s <- ptr_substitute(r)
  p <- ptr_prune(s)
  pl <- ptr_eval(p)
  expect_s3_class(pl, "ggplot")
})

test_that("P11 expr placeholder substitutes a real layer that evaluates", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point() + expr")
  expr_id <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "expr")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = setNames(list("theme_minimal()"), expr_id))
  p <- ptr_prune(s)
  pl <- ptr_eval(p)
  expect_s3_class(pl, "ggplot")
})

test_that("P11 magrittr pipe evaluates correctly via desugar", {
  r <- ptr_translate("mtcars %>% head(3) %>% ggplot(aes(x = mpg))")
  s <- ptr_substitute(r)
  p <- ptr_prune(s)
  pl <- ptr_eval(p)
  expect_s3_class(pl, "ggplot")
  expect_equal(nrow(pl$data), 3L)
})
