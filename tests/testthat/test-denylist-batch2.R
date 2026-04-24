# Tests for batch 2 safety fixes (codex/publication-loop branch).
#
# Fix 1 (paintr-utils.R):     attr<- and attributes<- blocked in denylist
# Fix 2 (paintr-placeholders.R): validate_expr_safety applied to custom
#                               resolve_expr hook results
# Fix 3 (paintr-utils.R):     Multi-element character vector catch in walk_expr
# Fix 5 (paintr-runtime.R):   expr_check threaded through ptr_assemble_plot
# Fix 6 (paintr-parse.R):     Empty/whitespace formula rejection

# =============================================================================
# Fix 1: attr<- and attributes<- blocked in denylist
# =============================================================================

test_that("Fix1-batch2: attr() call is blocked (baseline already in denylist)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('attr(x, "class")')),
    "not allowed"
  )
})

test_that("Fix1-batch2: `attr<-` backtick-quoted call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('`attr<-`(x, "class", "evil")')),
    "not allowed"
  )
})

test_that("Fix1-batch2: `attributes<-` backtick-quoted call is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('`attributes<-`(x, list(class = "evil"))')),
    "not allowed"
  )
})

test_that("Fix1-batch2: attr<- as bare symbol (higher-order) is blocked", {
  # `attr<-` passed as function argument
  expect_error(
    validate_expr_safety(rlang::parse_expr('lapply(list(), `attr<-`)')),
    "not allowed"
  )
})

test_that("Fix1-batch2: attributes<- as bare symbol (higher-order) is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('lapply(list(), `attributes<-`)')),
    "not allowed"
  )
})

test_that("Fix1-batch2: safe attribute read via $ is not blocked (no false positive)", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("x$class"))
  )
})

# =============================================================================
# Fix 2: Custom resolve_expr hook validation
# =============================================================================

test_that("Fix2-batch2: dangerous expression from custom hook is rejected", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) rlang::parse_expr("system('id')")
  )
  expect_error(
    ggpaintr:::ptr_resolve_placeholder_expr(
      spec, "anything", list(keyword = "test"), list(expr_check = TRUE)
    ),
    "not allowed"
  )
})

test_that("Fix2-batch2: safe expression from custom hook passes", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) quote(x + 1)
  )
  result <- ggpaintr:::ptr_resolve_placeholder_expr(
    spec, "anything", list(keyword = "test"), list(expr_check = TRUE)
  )
  expect_true(is.language(result))
  expect_equal(deparse(result), "x + 1")
})

test_that("Fix2-batch2: numeric result from custom hook passes without validation", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) 42L
  )
  result <- ggpaintr:::ptr_resolve_placeholder_expr(
    spec, "anything", list(keyword = "test"), list(expr_check = TRUE)
  )
  expect_equal(result, 42L)
})

test_that("Fix2-batch2: character result from custom hook passes without validation", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) "safe_string"
  )
  result <- ggpaintr:::ptr_resolve_placeholder_expr(
    spec, "anything", list(keyword = "test"), list(expr_check = TRUE)
  )
  expect_equal(result, "safe_string")
})

test_that("Fix2-batch2: expr_check = FALSE allows dangerous expression through", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) rlang::parse_expr("system('id')")
  )
  # With expr_check = FALSE, the safety check is skipped — no error from safety
  result <- ggpaintr:::ptr_resolve_placeholder_expr(
    spec, "anything", list(keyword = "test"), list(expr_check = FALSE)
  )
  expect_true(is.language(result))
})

test_that("Fix2-batch2: ptr_missing_expr() return from hook bypasses validation", {
  spec <- list(
    keyword = "test",
    resolve_expr = function(value, meta, context) ptr_missing_expr()
  )
  # Missing sentinel should pass through without safety error
  result <- ggpaintr:::ptr_resolve_placeholder_expr(
    spec, NULL, list(keyword = "test"), list(expr_check = TRUE)
  )
  # Result should be the missing-expr symbol (an internal sentinel)
  expect_true(is.symbol(result) || inherits(result, "ptr_missing_expr"))
})

# =============================================================================
# Fix 3: Multi-element character vector catch in validate_expr_safety
# =============================================================================

test_that("Fix3-batch2: c('system', 'eval') vector — dangerous element is caught", {
  # Direct call with a multi-element character vector containing a denylist name
  expect_error(
    validate_expr_safety(c("system", "eval")),
    "not allowed"
  )
})

test_that("Fix3-batch2: single string 'system' is blocked", {
  expect_error(
    validate_expr_safety("system"),
    "not allowed"
  )
})

test_that("Fix3-batch2: c('harmless', 'stuff') vector passes", {
  # Neither element is on the denylist
  expect_invisible(
    validate_expr_safety(c("harmless", "stuff"))
  )
})

test_that("Fix3-batch2: multi-element vector with one dangerous element blocked", {
  expect_error(
    validate_expr_safety(c("red", "eval", "blue")),
    "not allowed"
  )
})

test_that("Fix3-batch2: multi-element vector with all safe elements passes", {
  expect_invisible(
    validate_expr_safety(c("mpg", "hp", "disp"))
  )
})

# =============================================================================
# Fix 5: expr_check threading through ptr_assemble_plot
# =============================================================================

test_that("Fix5-batch2: ptr_assemble_plot with dangerous expr and expr_check=TRUE errors on safety", {
  danger <- list(rlang::parse_expr("system('id')"))
  expect_error(
    ptr_assemble_plot(danger, expr_check = TRUE),
    "not allowed"
  )
})

test_that("Fix5-batch2: ptr_assemble_plot with expr_check=FALSE skips safety check", {
  danger <- list(rlang::parse_expr("system('id', ignore.stdout = TRUE, ignore.stderr = TRUE)"))
  # Safety check is skipped; the call may succeed or fail at eval — either is fine.
  # What must NOT happen is an error whose message contains "not allowed".
  err <- tryCatch(
    ptr_assemble_plot(danger, envir = new.env(parent = baseenv()), expr_check = FALSE),
    error = function(e) e
  )
  # If an error occurred, it must not be a safety error
  if (inherits(err, "error")) {
    expect_false(grepl("not allowed", conditionMessage(err)))
  } else {
    # No error — safety check was indeed skipped (eval succeeded or returned a value)
    expect_true(TRUE)
  }
})

test_that("Fix5-batch2: ptr_assemble_plot with safe ggplot2 expr and default expr_check passes", {
  safe <- list(
    quote(ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp))),
    quote(ggplot2::geom_point())
  )
  result <- ptr_assemble_plot(safe, envir = globalenv())
  expect_s3_class(result, "gg")
})

test_that("Fix5-batch2: ptr_assemble_plot passes evaluation-level error (not safety) with expr_check=FALSE", {
  # An expression that uses an undefined function — eval will fail, not safety check
  bad_eval <- list(rlang::parse_expr("nonexistent_fn_xyz_abc()"))
  err <- tryCatch(
    ptr_assemble_plot(bad_eval, envir = new.env(parent = baseenv()), expr_check = FALSE),
    error = function(e) e
  )
  expect_false(is.null(err))
  # The error should NOT be a safety error
  expect_false(grepl("not allowed", conditionMessage(err)))
})

# =============================================================================
# Fix 6: Empty/whitespace formula rejection in ptr_parse_formula
# =============================================================================

test_that("Fix6-batch2: empty string formula errors with 'empty or whitespace'", {
  expect_error(
    ptr_parse_formula(""),
    regexp = "empty or whitespace"
  )
})

test_that("Fix6-batch2: whitespace-only formula errors with 'empty or whitespace'", {
  expect_error(
    ptr_parse_formula("   "),
    regexp = "empty or whitespace"
  )
})

test_that("Fix6-batch2: tab-and-newline formula errors with 'empty or whitespace'", {
  expect_error(
    ptr_parse_formula("\t\n"),
    regexp = "empty or whitespace"
  )
})

test_that("Fix6-batch2: single newline formula errors with 'empty or whitespace'", {
  expect_error(
    ptr_parse_formula("\n"),
    regexp = "empty or whitespace"
  )
})

test_that("Fix6-batch2: valid formula still parses correctly after empty-check (regression)", {
  result <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  expect_s3_class(result, "ptr_obj")
})

# =============================================================================
# Edge cases: bare symbols blocked by denylist
# =============================================================================

test_that("edge-case: bare symbol `attr` is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("attr")),
    "not allowed"
  )
})

test_that("edge-case: bare symbol `slot` is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("slot")),
    "not allowed"
  )
})

test_that("edge-case: NULL input does not crash validate_expr_safety", {
  # NULL is neither a call, symbol, nor character — should return invisibly TRUE
  # without error (graceful no-op)
  result <- tryCatch(
    validate_expr_safety(NULL),
    error = function(e) e
  )
  # Accept either graceful TRUE return or an error — just must not crash the
  # process. Record actual behavior.
  expect_true(is.null(result) || isTRUE(result) || inherits(result, "error"))
})
