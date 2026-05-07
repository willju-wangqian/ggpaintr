# Tests for previously uncovered code paths

# ---------------------------------------------------------------------------
# 1. ptr_resolve_placeholder_expr — function-return abort
# ---------------------------------------------------------------------------


# ---------------------------------------------------------------------------
# 2. ptr_resolve_upload_info — strict = TRUE aborts when no file uploaded
# ---------------------------------------------------------------------------

test_that("ptr_resolve_upload_info aborts when strict and no file", {
  input <- list()  # nothing uploaded

  expect_error(
    ptr_resolve_upload_info(input, "upload_id", strict = TRUE),
    "Upload required"
  )
})

test_that("ptr_resolve_upload_info returns NULL when not strict and no file", {
  result <- ptr_resolve_upload_info(list(), "upload_id", strict = FALSE)
  expect_null(result)
})

# ---------------------------------------------------------------------------
# 3. ptr_resolve_layer_data — three code paths
# ---------------------------------------------------------------------------






# ---------------------------------------------------------------------------
# 4. ptr_upload_default_name — edge cases
# ---------------------------------------------------------------------------

test_that("ptr_upload_default_name converts hyphens to underscores", {
  expect_equal(ptr_upload_default_name("my-data.csv"), "my_data")
})

test_that("ptr_upload_default_name strips leading/trailing separators from unusual stems", {
  # "...weird...file.rds" → stem "...weird...file"
  # gsub non-alnum/_ runs → "_weird_file"
  # strip leading/trailing _ → "weird_file"
  # make.names → "weird_file"
  expect_equal(ptr_upload_default_name("...weird...file.rds"), "weird_file")
})

test_that("ptr_upload_default_name derives 'csv' from a dot-only filename like '.csv'", {
  # tools::file_path_sans_ext(".csv") returns ".csv" (not stripped),
  # then non-alnum runs are replaced → "_csv", leading _ stripped → "csv"
  expect_equal(ptr_upload_default_name(".csv"), "csv")
})

test_that("ptr_upload_default_name falls back to 'uploaded_data' for a truly empty stem", {
  # A filename whose sanitized stem is empty (all separators, no alnum).
  # E.g. "----.csv": stem "----" → all replaced → "____" → stripped → "" → "uploaded_data"
  expect_equal(ptr_upload_default_name("----.csv"), "uploaded_data")
})

test_that("ptr_upload_default_name handles names with spaces", {
  # "my file.csv" → stem "my file" → "my_file"
  expect_equal(ptr_upload_default_name("my file.csv"), "my_file")
})

test_that("ptr_upload_default_name makes syntactically valid R names", {
  result <- ptr_upload_default_name("123data.csv")
  expect_true(make.names(result) == result)
})
