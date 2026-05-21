# P2 — classify-data. Annotates `ptr_ph_data_consumer` with `upstream`.

test_that("P2.1 ppVar inside aes of ggplot(df) sees df as upstream", {
  r <- ptr_translate("ggplot(mtcars, aes(x = ppVar)) + geom_point()")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 1L)
  up <- consumers[[1]]$upstream
  expect_s3_class(up, "ptr_literal")
  expect_identical(up$expr, quote(mtcars))
})

test_that("P2.2 ppVar inside aes with named data = arg sees the named arg", {
  r <- ptr_translate("ggplot(data = mtcars, aes(x = ppVar)) + geom_point()")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 1L)
  up <- consumers[[1]]$upstream
  expect_s3_class(up, "ptr_literal")
  expect_identical(up$expr, quote(mtcars))
})

test_that("P2.3 ppVar in geom inherits from ggplot", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_line(aes(y = ppVar))")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 1L)
  up <- consumers[[1]]$upstream
  expect_s3_class(up, "ptr_literal")
  expect_identical(up$expr, quote(mtcars))
})

test_that("P2.4 geom with explicit data= shadows inheritance", {
  r <- ptr_translate("ggplot(mtcars) + geom_line(data = iris, aes(y = ppVar))")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 1L)
  up <- consumers[[1]]$upstream
  expect_s3_class(up, "ptr_literal")
  expect_identical(up$expr, quote(iris))
})

test_that("P2.5 pipeline stage k>1 sees prior stages as synthetic upstream", {
  r <- ptr_translate("mtcars |> filter(year >= ppNum) |> ggplot(aes(x = ppVar))")
  num_node <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  var_node <- find_nodes(r, is_ptr_ph_data_consumer)[[1]]
  expect_s3_class(var_node$upstream, "ptr_pipeline")
  expect_equal(length(var_node$upstream$stages), 2L)
})

test_that("P2.6 pipeline stage 1 inherits enclosing ctx_data", {
  r <- ptr_translate("ggplot(data = mtcars |> filter(ppNum > 0))")
  num_node <- find_nodes(r, function(x) is_ptr_ph_value(x) && x$keyword == "ppNum")[[1]]
  expect_true(is_ptr_ph_value(num_node))
})

test_that("P2.7 source has no upstream; consumers see source as upstream", {
  r <- ptr_translate(
    'ggplot(data = ppUpload(shared = "ds")) + geom_point(aes(x = ppVar, y = ppVar))'
  )
  source_nodes <- find_nodes(r, is_ptr_ph_data_source)
  expect_equal(length(source_nodes), 1L)
  expect_null(source_nodes[[1]]$upstream)
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 2L)
  for (c in consumers) {
    expect_true(is_ptr_ph_data_source(c$upstream))
    expect_equal(c$upstream$keyword, "ppUpload")
  }
})

test_that("P2.8 two var nodes in the same pipeline share an upstream pointer", {
  r <- ptr_translate("mtcars |> filter(ppNum > 0) |> ggplot(aes(x = ppVar, y = ppVar))")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 2L)
  expect_identical(consumers[[1]]$upstream, consumers[[2]]$upstream)
})

test_that("P2.9 chained var pipeline assigns position-correct upstream", {
  r <- ptr_translate("mtcars |> select(ppVar) |> select(ppVar) |> ggplot(aes(x = ppVar))")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 3L)
  # The aes-level var has the deepest pipeline upstream.
  ups_lengths <- vapply(consumers, function(c) {
    if (is_ptr_pipeline(c$upstream)) length(c$upstream$stages) else 0L
  }, integer(1))
  expect_true(any(ups_lengths == 0L))   # innermost select(var) sees plain mtcars
  expect_true(any(ups_lengths >= 2L))   # outermost var sees mtcars + 2 selects
})

test_that("P2.11 bare-symbol non-keyword data arg becomes ptr_literal upstream", {
  r <- ptr_translate("ggplot(my_local_df, aes(x = ppVar))")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_s3_class(consumers[[1]]$upstream, "ptr_literal")
  expect_identical(consumers[[1]]$upstream$expr, quote(my_local_df))
})

test_that("P2.12 layer with no data placeholder has no upstream metadata", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point()")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 0L)
})
