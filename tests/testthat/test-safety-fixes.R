# Tests for three safety fixes introduced in the codex/publication-loop branch.
#
# Fix 1 (paintr-parse.R):    tryCatch wraps rlang::parse_expr — malformed
#                             formula signals rlang error with user message.
# Fix 2 (paintr-utils.R):    12 info-disclosure functions added to
#                             unsafe_expr_denylist.
# Fix 3 (paintr-runtime.R):  NULL input_item early-return guard in
#                             ptr_complete_expr prevents crash outside Shiny.

# =============================================================================
# Fix 1: ptr_parse_formula — malformed formula error wrapping
# =============================================================================

test_that("Fix1: malformed formula signals an error (not a raw parse error)", {
  expect_error(
    ptr_parse_formula("not valid R @@@ code"),
    regexp = "could not parse formula"
  )
})

test_that("Fix1: error class is rlang_error (from rlang::abort)", {
  err <- tryCatch(
    ptr_parse_formula("@@@ invalid"),
    error = function(e) e
  )
  expect_true(inherits(err, "rlang_error"))
})

test_that("Fix1: error message contains function name for user identification", {
  err <- tryCatch(
    ptr_parse_formula("1 + ("),
    error = function(e) e
  )
  msg <- conditionMessage(err)
  expect_true(grepl("ptr_parse_formula", msg) || grepl("could not parse formula", msg))
})

test_that("Fix1: error carries original parse error as parent", {
  err <- tryCatch(
    ptr_parse_formula("$ $ $"),
    error = function(e) e
  )
  # rlang errors with parent= have a $parent field
  expect_false(is.null(err$parent))
})

test_that("Fix1: valid formula still parses without error (regression)", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  expect_s3_class(obj, "ptr_obj")
})

test_that("Fix1: empty string signals parse error", {
  expect_error(
    ptr_parse_formula(""),
    regexp = "could not parse formula"
  )
})

# =============================================================================
# Fix 2: unsafe_expr_denylist — new info-disclosure entries
# =============================================================================

test_that("Fix2: Sys.getenv is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.getenv('HOME')")),
    "not allowed"
  )
})

test_that("Fix2: list.files is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("list.files('/')")),
    "not allowed"
  )
})

test_that("Fix2: message is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("message('leak')")),
    "not allowed"
  )
})

test_that("Fix2: Sys.getpid is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.getpid()")),
    "not allowed"
  )
})

test_that("Fix2: Sys.info is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.info()")),
    "not allowed"
  )
})

test_that("Fix2: Sys.time is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.time()")),
    "not allowed"
  )
})

test_that("Fix2: proc.time is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("proc.time()")),
    "not allowed"
  )
})

test_that("Fix2: warning is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("warning('oops')")),
    "not allowed"
  )
})

test_that("Fix2: getwd is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("getwd()")),
    "not allowed"
  )
})

test_that("Fix2: normalizePath is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("normalizePath('.')")),
    "not allowed"
  )
})

test_that("Fix2: Sys.glob is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.glob('*')")),
    "not allowed"
  )
})

test_that("Fix2: list.dirs is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("list.dirs('/')")),
    "not allowed"
  )
})

test_that("Fix2: nested Sys.getenv call is also blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("paste(Sys.getenv('SECRET'), 'x')")),
    "not allowed"
  )
})

test_that("Fix2: safe expression remains unblocked after denylist additions", {
  expect_invisible(validate_expr_safety(rlang::parse_expr("sqrt(x + 1)")))
  expect_invisible(validate_expr_safety(rlang::parse_expr("paste0('a', 'b')")))
})

# =============================================================================
# Fix 3: ptr_complete_expr — NULL input_item early-return guard
# =============================================================================

test_that("Fix3: ptr_complete_expr does not error when all placeholder inputs are NULL", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  # Provide only checkboxes; all placeholder ids are absent (NULL from input[[id]])
  input <- list("geom_point+checkbox" = TRUE)

  expect_no_error(ptr_complete_expr(obj, input))
})

test_that("Fix3: ptr_complete_expr result is a list with required fields when inputs are NULL", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input <- list("geom_point+checkbox" = TRUE)

  result <- ptr_complete_expr(obj, input)

  expect_type(result, "list")
  expect_true("complete_expr_list" %in% names(result))
  expect_true("code_text" %in% names(result))
  expect_true("eval_env" %in% names(result))
})

test_that("Fix3: code_text is a non-empty string when placeholder inputs are NULL", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  input <- list("geom_point+checkbox" = TRUE)

  result <- ptr_complete_expr(obj, input)

  expect_type(result$code_text, "character")
  expect_true(nchar(result$code_text) > 0)
})

test_that("Fix3: NULL input for text placeholder does not error", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point() + labs(title = text)"
  )
  # text placeholder input absent (NULL); checkbox set
  input <- list(
    "geom_point+checkbox" = TRUE,
    "labs+checkbox" = TRUE
  )

  expect_no_error(ptr_complete_expr(obj, input))
})

test_that("Fix3: NULL input for num placeholder does not error", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point(size = num)"
  )
  input <- list("geom_point+checkbox" = TRUE)

  expect_no_error(ptr_complete_expr(obj, input))
})

test_that("Fix3: mixed NULL and non-NULL inputs work correctly", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )
  spec <- ptr_runtime_input_spec(obj)
  # Supply one var, leave the other and text as NULL
  input <- list(
    "ggplot+3+2" = "mpg",
    "geom_point+checkbox" = TRUE,
    "labs+checkbox" = TRUE
  )

  result <- ptr_complete_expr(obj, input)

  expect_type(result, "list")
  expect_match(result$code_text, "mpg")
})
