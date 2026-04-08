test_that("ggpaintr_normalize_column_names leaves syntactic names unchanged", {
  normalized <- ggpaintr_normalize_column_names(iris)

  expect_s3_class(normalized, "data.frame")
  expect_identical(names(normalized), names(iris))
})

test_that("ggpaintr_normalize_column_names normalizes invalid names deterministically", {
  messy <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6)
  names(messy) <- c("first column", "first-column", "1start", "if", "", "a.b")

  normalized <- ggpaintr_normalize_column_names(messy)

  expect_identical(
    names(normalized),
    c("first_column", "first_column_1", "X1start", "if_", "col", "a.b")
  )
})

test_that("ggpaintr_normalize_column_names preserves data.frame subclasses", {
  subclassed <- structure(
    data.frame(value = 1:2, check.names = FALSE),
    class = c("custom_tbl", "data.frame"),
    row.names = c(NA_integer_, -2L)
  )
  names(subclassed) <- "first column"

  normalized <- ggpaintr_normalize_column_names(subclassed)

  expect_s3_class(normalized, "custom_tbl")
  expect_identical(names(normalized), "first_column")
})

test_that("ggpaintr_normalize_column_names coerces tabular inputs with as.data.frame", {
  matrix_input <- matrix(
    1:4,
    ncol = 2,
    dimnames = list(NULL, c("first column", "if"))
  )

  normalized <- ggpaintr_normalize_column_names(matrix_input)

  expect_s3_class(normalized, "data.frame")
  expect_false(inherits(normalized, "matrix"))
  expect_identical(names(normalized), c("first_column", "if_"))
})

test_that("ggpaintr_normalize_column_names errors on non-tabular inputs", {
  expect_error(
    ggpaintr_normalize_column_names(function(x) x),
    "Data is not usable as tabular data for ggpaintr"
  )
})
