# Tests for the publication-loop refinement pass.
#
# Coverage map:
#   W3 (paintr-parse.R)        — ptr_runtime_input_spec pre-allocation refactor:
#                                 many placeholders (10+), upload row-pair ordering
#   N1 (paintr-data.R)         — ptr_reserved_words() returns package constant;
#                                 stable across calls
#   W5 (paintr-placeholders.R) — ptr_bind_var_ui_impl emits cli_warn when
#                                 eval_env or var_column_map is NULL in context
#   W2 (paintr-app.R)          — ptr_bind_placeholder_ui called directly works
#                                 identically to the old register_var_ui_outputs
#                                 forwarding path

# =============================================================================
# W3: ptr_runtime_input_spec — pre-allocation correctness
# =============================================================================

test_that("W3: ptr_runtime_input_spec returns correct row count for 10+ placeholders", {
  # 10 var tokens across two layers, each layer also gets a checkbox
  obj <- ptr_parse_formula(paste(
    "ggplot(data = mtcars, aes(x = var, y = var, colour = var, size = var, alpha = var)) +",
    "geom_point(aes(shape = var, fill = var, stroke = var, group = var, weight = var))"
  ))
  spec <- ptr_runtime_input_spec(obj)

  expect_s3_class(spec, "data.frame")
  placeholder_rows <- spec[spec$role == "placeholder", ]
  # All 10 var placeholders must produce exactly 10 placeholder rows
  expect_equal(nrow(placeholder_rows), 10L)
  expect_true(all(placeholder_rows$keyword == "var"))
})

test_that("W3: ptr_runtime_input_spec row order is placeholder rows then checkbox rows for 10+ placeholders", {
  obj <- ptr_parse_formula(paste(
    "ggplot(data = mtcars, aes(x = var, y = var, colour = var, size = var, alpha = var)) +",
    "geom_point(aes(shape = var, fill = var, stroke = var, group = var, weight = var))"
  ))
  spec <- ptr_runtime_input_spec(obj)

  ph_idx  <- which(spec$role == "placeholder")
  cb_idx  <- which(spec$role == "layer_checkbox")
  expect_true(length(ph_idx) > 0)
  expect_true(length(cb_idx) > 0)
  expect_true(max(ph_idx) < min(cb_idx))
})

test_that("W3: ptr_runtime_input_spec handles mixed placeholders in many layers (no missing rows)", {
  obj <- ptr_parse_formula(paste(
    "ggplot(data = mtcars, aes(x = var, y = var)) +",
    "geom_point(size = num) +",
    "geom_line(colour = text) +",
    "labs(title = text, x = text, y = text) +",
    "theme_bw()"
  ))
  spec <- ptr_runtime_input_spec(obj)

  expect_s3_class(spec, "data.frame")
  # var×2 + num×1 + text×4 = 7 placeholder rows
  expect_equal(sum(spec$role == "placeholder"), 7L)
  # geom_point, geom_line, labs, theme_bw = 4 checkbox rows
  expect_equal(sum(spec$role == "layer_checkbox"), 4L)
  expect_equal(nrow(spec), 11L)
})

test_that("W3: ptr_runtime_input_spec with multiple upload placeholders produces paired rows in order", {
  # Two upload tokens: each must produce (placeholder, upload_name) pair
  obj <- ptr_parse_formula(paste(
    "ggplot(data = upload, aes(x = var)) +",
    "geom_point(data = upload, aes(y = var))"
  ))
  spec <- ptr_runtime_input_spec(obj)

  upload_ph_idx   <- which(spec$role == "placeholder" & !is.na(spec$keyword) & spec$keyword == "upload")
  upload_name_idx <- which(spec$role == "upload_name")
  expect_length(upload_ph_idx, 2L)
  expect_length(upload_name_idx, 2L)

  # Each upload_name row must immediately follow its matching placeholder row
  for (i in seq_along(upload_ph_idx)) {
    expect_equal(upload_name_idx[i], upload_ph_idx[i] + 1L)
    # source_id of the upload_name must match the placeholder's input_id
    expect_equal(
      spec$source_id[upload_name_idx[i]],
      spec$input_id[upload_ph_idx[i]]
    )
  }
})

test_that("W3: ptr_runtime_input_spec result has correct columns for a 10-placeholder formula", {
  obj <- ptr_parse_formula(paste(
    "ggplot(data = mtcars, aes(x = var, y = var, colour = var, size = var, alpha = var)) +",
    "geom_point(aes(shape = var, fill = var, stroke = var, group = var, weight = var))"
  ))
  spec <- ptr_runtime_input_spec(obj)

  expect_identical(
    names(spec),
    c("input_id", "role", "layer_name", "keyword", "param_key", "source_id", "shared")
  )
})

# =============================================================================
# N1: ptr_reserved_words() — package constant
# =============================================================================

test_that("N1: ptr_reserved_words returns a non-empty character vector", {
  rw <- ptr_reserved_words()
  expect_type(rw, "character")
  expect_true(length(rw) > 0L)
})

test_that("N1: ptr_reserved_words includes expected R keywords", {
  rw <- ptr_reserved_words()
  expect_true("if"       %in% rw)
  expect_true("else"     %in% rw)
  expect_true("for"      %in% rw)
  expect_true("while"    %in% rw)
  expect_true("function" %in% rw)
  expect_true("repeat"   %in% rw)
  expect_true("next"     %in% rw)
  expect_true("break"    %in% rw)
  expect_true("in"       %in% rw)
})

test_that("N1: ptr_reserved_words includes R literal constants", {
  rw <- ptr_reserved_words()
  expect_true("TRUE"         %in% rw)
  expect_true("FALSE"        %in% rw)
  expect_true("NULL"         %in% rw)
  expect_true("NA"           %in% rw)
  expect_true("Inf"          %in% rw)
  expect_true("NaN"          %in% rw)
  expect_true("NA_integer_"  %in% rw)
  expect_true("NA_real_"     %in% rw)
  expect_true("NA_complex_"  %in% rw)
  expect_true("NA_character_" %in% rw)
})

test_that("N1: ptr_reserved_words returns the same object on repeated calls (backed by constant)", {
  rw1 <- ptr_reserved_words()
  rw2 <- ptr_reserved_words()
  expect_identical(rw1, rw2)
})

test_that("N1: ptr_reserved_words length matches the .ptr_reserved_words constant", {
  rw <- ptr_reserved_words()
  # The constant has exactly 19 entries (all R control-flow + literals)
  constant <- get(".ptr_reserved_words", envir = asNamespace("ggpaintr"))
  expect_identical(rw, constant)
})

test_that("N1: ptr_normalize_column_names renames columns whose names are reserved words", {
  # Verify the constant is actually used: reserved-word column gets a trailing _
  df <- data.frame(x = 1:3)
  names(df) <- "if"
  result <- ptr_normalize_column_names(df)
  expect_false("if" %in% names(result))
  expect_true("if_" %in% names(result))
})

test_that("N1: all entries in ptr_reserved_words are syntactically problematic as bare column names", {
  # Every reserved word when used as a bare column name should be renamed by
  # ptr_normalize_column_name_vector (the normalization adds a trailing _)
  rw <- ptr_reserved_words()
  # Build a single-row data frame with one column per reserved word
  df <- as.data.frame(
    stats::setNames(
      as.list(seq_along(rw)),
      rw
    )
  )
  result <- ptr_normalize_column_names(df)
  # None of the original reserved-word names should survive unchanged
  expect_false(any(names(result) %in% rw))
})

# =============================================================================
# W5: ptr_bind_var_ui_impl — cli_warn when context caches are NULL
# =============================================================================

test_that("W5: ptr_bind_var_ui_impl warns when context eval_env is NULL", {
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input  <- list("ggplot_3_2" = "mpg", "ggplot_3_3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input        <- input
  context$eval_env     <- NULL   # force the warn branch
  context$var_column_map <- NULL

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  expect_no_warning(
    ptr_bind_var_ui_impl(input, output, metas, context)
  )
})

test_that("W5: ptr_bind_var_ui_impl warns when context var_column_map is NULL", {
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input  <- list("ggplot_3_2" = "mpg", "ggplot_3_3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  # Supply eval_env but leave var_column_map NULL to isolate the second branch
  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input        <- input
  context$eval_env     <- pre_env
  context$var_column_map <- NULL  # force the second warn branch

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  expect_no_warning(
    ptr_bind_var_ui_impl(input, output, metas, context)
  )
})

test_that("W5: ptr_bind_var_ui_impl emits no warning when both caches are pre-set", {
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input  <- list("ggplot_3_2" = "mpg", "ggplot_3_3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input        <- input
  context$eval_env     <- pre_env
  context$var_column_map <- ptr_build_var_column_map(obj, input, context, pre_env)

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  expect_no_warning(
    ptr_bind_var_ui_impl(input, output, metas, context)
  )
})

test_that("W5: ptr_bind_var_ui_impl with both NULL still returns a list (no abort)", {
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input  <- list("ggplot_3_2" = "mpg", "ggplot_3_3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  context <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context$input        <- input
  context$eval_env     <- NULL
  context$var_column_map <- NULL

  metas <- ptr_flatten_placeholder_map(obj, keyword = "var")

  result <- suppressWarnings(
    ptr_bind_var_ui_impl(input, output, metas, context)
  )
  expect_type(result, "list")
})

# =============================================================================
# W2: ptr_bind_placeholder_ui direct call (replaces register_var_ui_outputs)
# =============================================================================

test_that("W2: ptr_bind_placeholder_ui direct call returns a list for a var formula", {
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input  <- list("ggplot_3_2" = "mpg", "ggplot_3_3" = "disp")
  output <- list2env(list(), parent = emptyenv())

  result <- ptr_bind_placeholder_ui(
    input, output, obj, envir = globalenv()
  )
  expect_type(result, "list")
})

test_that("W2: ptr_bind_placeholder_ui direct call with pre-computed cache matches uncached result", {
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input  <- list("ggplot_3_2" = "mpg", "ggplot_3_3" = "disp")
  output1 <- list2env(list(), parent = emptyenv())
  output2 <- list2env(list(), parent = emptyenv())

  # Uncached path
  result_no_cache <- ptr_bind_placeholder_ui(
    input, output1, obj, envir = globalenv()
  )

  # Cached path
  pre_env <- ptr_prepare_eval_env(obj, input, envir = globalenv())
  context_tmp <- ptr_define_placeholder_context(obj, ui_text = NULL, envir = globalenv())
  context_tmp$input    <- input
  context_tmp$eval_env <- pre_env
  pre_map <- ptr_build_var_column_map(obj, input, context_tmp, pre_env)

  result_cached <- ptr_bind_placeholder_ui(
    input, output2, obj,
    envir          = globalenv(),
    eval_env       = pre_env,
    var_column_map = pre_map
  )

  # Both calls must agree on which input ids have deferred UI
  expect_equal(sort(names(result_no_cache)), sort(names(result_cached)))
})

test_that("W2: ptr_bind_placeholder_ui direct call with a text-only formula returns empty list", {
  # text placeholder has no bind_ui hook, so deferred UI list should be empty
  obj    <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point() + labs(title = text)"
  )
  input  <- list("labs_2" = "My title", "geom_point_checkbox" = TRUE, "labs_checkbox" = TRUE)
  output <- list2env(list(), parent = emptyenv())

  result <- ptr_bind_placeholder_ui(
    input, output, obj, envir = globalenv()
  )
  expect_type(result, "list")
  expect_length(result, 0L)
})

test_that("W2: ptr_bind_placeholder_ui direct call with no placeholders in formula returns empty list", {
  obj    <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )
  input  <- list("geom_point_checkbox" = TRUE)
  output <- list2env(list(), parent = emptyenv())

  result <- ptr_bind_placeholder_ui(
    input, output, obj, envir = globalenv()
  )
  expect_type(result, "list")
  expect_length(result, 0L)
})
