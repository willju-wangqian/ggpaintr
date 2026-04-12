test_that("ptr_normalize_column_names leaves syntactic names unchanged", {
  normalized <- ptr_normalize_column_names(iris)

  expect_s3_class(normalized, "data.frame")
  expect_identical(names(normalized), names(iris))
})

test_that("ptr_normalize_column_names normalizes invalid names deterministically", {
  messy <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5, f = 6)
  names(messy) <- c("first column", "first-column", "1start", "if", "", "a.b")

  normalized <- ptr_normalize_column_names(messy)

  expect_identical(
    names(normalized),
    c("first_column", "first_column_1", "X1start", "if_", "col", "a.b")
  )
})

test_that("ptr_normalize_column_names preserves data.frame subclasses", {
  subclassed <- structure(
    data.frame(value = 1:2, check.names = FALSE),
    class = c("custom_tbl", "data.frame"),
    row.names = c(NA_integer_, -2L)
  )
  names(subclassed) <- "first column"

  normalized <- ptr_normalize_column_names(subclassed)

  expect_s3_class(normalized, "custom_tbl")
  expect_identical(names(normalized), "first_column")
})

test_that("ptr_normalize_column_names coerces tabular inputs with as.data.frame", {
  matrix_input <- matrix(
    1:4,
    ncol = 2,
    dimnames = list(NULL, c("first column", "if"))
  )

  normalized <- ptr_normalize_column_names(matrix_input)

  expect_s3_class(normalized, "data.frame")
  expect_false(inherits(normalized, "matrix"))
  expect_identical(names(normalized), c("first_column", "if_"))
})

test_that("ptr_normalize_column_names errors on non-tabular inputs", {
  expect_error(
    ptr_normalize_column_names(function(x) x),
    "Data is not usable as tabular data for ggpaintr"
  )
})

# --- F6: collision after reserved-word suffix --------------------------------

test_that("F6: happy path c('a','b','c') round-trips unchanged", {
  expect_identical(ptr_normalize_column_name_vector(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("F6: c('if','if_') disambiguates without silently renaming the pre-existing 'if_'", {
  result <- ptr_normalize_column_name_vector(c("if", "if_"))
  # 'if' must become 'if_', original 'if_' must be deduped to 'if_1'
  expect_equal(result, c("if_", "if_1"))
  # Both names must be present and distinct
  expect_length(unique(result), 2L)
})

test_that("F6: three 'if' columns all get unique reserved-word-safe names", {
  result <- ptr_normalize_column_name_vector(c("if", "if", "if"))
  expect_length(result, 3L)
  expect_length(unique(result), 3L)
  # All must be safe (none equals a reserved word)
  expect_false(any(result %in% ptr_reserved_words()))
})

test_that("F6: mix of reserved-word and normal columns preserves non-reserved names", {
  result <- ptr_normalize_column_name_vector(c("if", "x", "for", "y"))
  expect_equal(result[2], "x")
  expect_equal(result[4], "y")
  expect_false(any(result %in% ptr_reserved_words()))
  expect_length(unique(result), 4L)
})
