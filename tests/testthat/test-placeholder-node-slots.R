# PLAN-02 — Placeholder node slots (default, named_args)
#
# Verifies that the three placeholder node constructors expose the additive
# `default` and `named_args` slots with the documented defaults and
# pass-through behaviour, without changing any pre-existing slot or class
# membership.

test_that("ptr_ph_value default slot defaults to NULL", {
  node <- ptr_ph_value(keyword = "ppNum", expr = quote(ppNum()))
  expect_null(node$default)
})

test_that("ptr_ph_value default slot stores its argument verbatim", {
  node <- ptr_ph_value(keyword = "ppNum", expr = quote(ppNum(5)), default = 5)
  expect_equal(node$default, 5)

  node_chr <- ptr_ph_value(
    keyword = "ppVar", expr = quote(ppVar("mpg")), default = "mpg"
  )
  expect_equal(node_chr$default, "mpg")
})

test_that("ptr_ph_value named_args slot defaults to empty list", {
  node <- ptr_ph_value(keyword = "ppNum", expr = quote(ppNum()))
  expect_true(identical(node$named_args, list()))
})

test_that("ptr_ph_value named_args slot stores a named list", {
  node <- ptr_ph_value(
    keyword = "ppRange",
    expr = quote(ppRange(c(0, 1), step = 0.5)),
    named_args = list(step = 0.5)
  )
  expect_equal(node$named_args$step, 0.5)
})

test_that("ptr_ph_value preserves existing fields and S3 class", {
  node <- ptr_ph_value(
    id = "x1", keyword = "ppNum", param = "x",
    expr = quote(ppNum()), shared = "g1"
  )
  expect_true(is_ptr_ph_value(node))
  expect_true(is_ptr_placeholder(node))
  expect_equal(node$id, "x1")
  expect_equal(node$keyword, "ppNum")
  expect_equal(node$param, "x")
  expect_equal(node$expr, quote(ppNum()))
  expect_equal(node$shared, "g1")
})

test_that("ptr_ph_data_consumer carries new slots without losing upstream", {
  upstream <- ptr_ph_data_source(
    keyword = "ppUpload", expr = quote(ppUpload())
  )
  node <- ptr_ph_data_consumer(
    keyword = "ppVar", expr = quote(ppVar()), upstream = upstream
  )
  expect_true(is_ptr_ph_data_consumer(node))
  expect_identical(node$upstream, upstream)
  expect_null(node$default)
  expect_true(identical(node$named_args, list()))

  node2 <- ptr_ph_data_consumer(
    keyword = "ppVar", expr = quote(ppVar("mpg")),
    upstream = upstream, default = "mpg",
    named_args = list(multiple = TRUE)
  )
  expect_equal(node2$default, "mpg")
  expect_equal(node2$named_args$multiple, TRUE)
  expect_identical(node2$upstream, upstream)
})

test_that("ptr_ph_data_source carries new slots without losing companion_id", {
  node <- ptr_ph_data_source(
    keyword = "ppUpload", expr = quote(ppUpload()),
    companion_id = "x_name"
  )
  expect_true(is_ptr_ph_data_source(node))
  expect_equal(node$companion_id, "x_name")
  expect_null(node$default)
  expect_true(identical(node$named_args, list()))
})
