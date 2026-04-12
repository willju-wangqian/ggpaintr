# Tests for previously uncovered code paths

# ---------------------------------------------------------------------------
# 1. ptr_resolve_placeholder_expr — function-return abort
# ---------------------------------------------------------------------------

test_that("ptr_resolve_placeholder_expr aborts when resolve_expr returns a function", {
  bad_placeholder <- ptr_define_placeholder(
    keyword = "bad",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, "x"),
    resolve_expr = function(value, meta, context) identity  # returns a function
  )

  registry <- ptr_merge_placeholders(list(bad = bad_placeholder))
  obj <- ptr_parse_formula(
    "ggplot(mtcars, aes(x = bad)) + geom_point()",
    placeholders = registry
  )

  # Identify the bad placeholder's id from the parsed object
  bad_metas <- ptr_flatten_placeholder_map(obj, keyword = "bad")
  expect_length(bad_metas, 1L)
  bad_id <- bad_metas[[1]]$id

  input <- stats::setNames(
    list("mpg", TRUE),
    c(bad_id, "geom_point+checkbox")
  )

  expect_error(
    ptr_complete_expr(obj, input),
    "returned a function instead of an expression"
  )
})

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

test_that("ptr_resolve_layer_data returns has_data=FALSE when no data param in layer", {
  # geom_point() with no data= argument
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  eval_env <- new.env(parent = baseenv())

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "geom_point",
    input      = list(),
    context    = context,
    eval_env   = eval_env
  )

  expect_false(result$has_data)
  expect_null(result$data)
})

test_that("ptr_resolve_layer_data returns has_data=TRUE, data=NULL for unresolved upload placeholder", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var)) + geom_point()"
  )
  # No upload provided in input → placeholder resolves to missing
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  eval_env <- new.env(parent = baseenv())

  # ggplot is the layer that has data= upload
  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input      = list(),        # upload id absent → missing expr
    context    = context,
    eval_env   = eval_env
  )

  expect_true(result$has_data)
  expect_null(result$data)
})

test_that("ptr_resolve_layer_data evaluates a literal symbol from eval_env", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  # mtcars is available in the base environment; use it as parent
  eval_env <- new.env(parent = .GlobalEnv)

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input      = list(),
    context    = context,
    eval_env   = eval_env
  )

  expect_true(result$has_data)
  expect_s3_class(result$data, "data.frame")
  expect_identical(result$data, mtcars)
})

test_that("ptr_resolve_layer_data detects positional first-arg data", {
  obj <- ptr_parse_formula(
    "ggplot(mtcars, aes(x = var, y = var)) + geom_point()"
  )
  context <- ptr_define_placeholder_context(obj, ui_text = NULL)
  eval_env <- new.env(parent = .GlobalEnv)

  result <- ptr_resolve_layer_data(
    obj,
    layer_name = "ggplot",
    input      = list(),
    context    = context,
    eval_env   = eval_env
  )

  expect_true(result$has_data)
  expect_s3_class(result$data, "data.frame")
  expect_identical(result$data, mtcars)
})

test_that("ptr_param_matches_data detects named and positional data params", {
  expect_true(ptr_param_matches_data("data"))
  expect_true(ptr_param_matches_data(NULL, index_path = 2))
  expect_true(ptr_param_matches_data("", index_path = 2))
  expect_false(ptr_param_matches_data(NULL))
  expect_false(ptr_param_matches_data(NULL, index_path = 3))
  expect_false(ptr_param_matches_data("color"))
})

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
