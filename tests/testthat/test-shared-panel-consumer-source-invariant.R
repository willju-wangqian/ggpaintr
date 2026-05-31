# PLAN-03 / ADR 0023 §6 -- parse-time invariant: a panel-owned consumer key
# requires a panel-owned source upstream. The check fires inside
# `ptr_shared()` after `shared_partition()` and only inspects `panel_keys`;
# single-formula / inline-section / formula-local-consumer cases are not
# affected.

# --- negative: ADR worked example R1 ----------------------------------------

test_that("R1 -- different bare-data sources behind one shared consumer abort", {
  formulas <- c(
    "ggplot(mtcars, aes(x = ppVar(shared = 'col'))) + geom_point()",
    "ggplot(iris,   aes(x = ppVar(shared = 'col'))) + geom_point()"
  )
  err <- expect_error(
    ptr_shared(formulas),
    class = "ptr_panel_consumer_source_mismatch"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "col",     fixed = TRUE)
  expect_match(msg, "mtcars",  fixed = TRUE)
  expect_match(msg, "iris",    fixed = TRUE)
  expect_match(msg, "formula 1", fixed = TRUE)
  expect_match(msg, "formula 2", fixed = TRUE)
  # Structured condition fields: the abort carries the consumer key and the
  # offending source descriptors verbatim, so embedders can `tryCatch()` and
  # render their own UI.
  expect_equal(err$key, "col")
  expect_true("mtcars" %in% err$sources)
  expect_true("iris"   %in% err$sources)
  expect_setequal(err$formulas, formulas)
})

# --- negative: ADR worked example R2 ----------------------------------------

test_that("R2 -- two bare ppUpload() with shared consumer falls to R1", {
  formulas <- c(
    "ggplot(ppUpload(), aes(x = ppVar(shared = 'col'))) + geom_point()",
    "ggplot(ppUpload(), aes(x = ppVar(shared = 'col'))) + geom_col()"
  )
  err <- expect_error(
    ptr_shared(formulas),
    class = "ptr_panel_consumer_source_mismatch"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "col", fixed = TRUE)
  # Each bare ppUpload() widget is independent (no `shared`), so the message
  # must call that out (formula-local upstream sources).
  expect_match(msg, "formula-local", fixed = TRUE)
  expect_equal(err$key, "col")
})

# --- positive: panel-owned consumer with panel-owned source -----------------

test_that("panel-owned consumer with panel-owned source passes", {
  formulas <- c(
    "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'))) + geom_point()",
    "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'))) + geom_col()"
  )
  obj <- ptr_shared(formulas)
  expect_s3_class(obj, "ptr_shared_spec")
  expect_true("ds"  %in% obj$panel_keys)
  expect_true("col" %in% obj$panel_keys)
})

# --- positive: mixed scope (panel src + formula-local consumers) ------------

test_that("mixed scope (panel src, formula-local consumers) passes; panel_keys excludes single-formula keys", {
  formulas <- c(
    "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'colA'), y = mpg)) + geom_point()",
    "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'colB'), y = hp)) + geom_col()"
  )
  obj <- ptr_shared(formulas)
  expect_s3_class(obj, "ptr_shared_spec")
  expect_equal(sort(obj$panel_keys), "ds")
  expect_false("colA" %in% obj$panel_keys)
  expect_false("colB" %in% obj$panel_keys)
})

# --- positive: panel-owned consumer with identical bare-data on both sides --

test_that("panel-owned consumer with identical bare-data upstream passes", {
  formulas <- c(
    "ggplot(mtcars, aes(x = ppVar(shared = 'col'))) + geom_point()",
    "ggplot(mtcars, aes(x = ppVar(shared = 'col'))) + geom_col()"
  )
  obj <- ptr_shared(formulas)
  expect_s3_class(obj, "ptr_shared_spec")
  expect_true("col" %in% obj$panel_keys)
})

# --- negative-adjacent: single-formula spec is unaffected -------------------

test_that("single-formula spec is unaffected by the panel-consumer/source check", {
  formulas <- c(
    "ggplot(mtcars, aes(x = ppVar(shared = 'col'))) + geom_point()"
  )
  # Single-formula ptr_shared() is the existing
  # "shared spec needs >=1 cross-formula key OR a single formula with
  # only formula-local shared keys" -- here `col` lives in exactly one
  # formula, so panel_keys is empty and our new check skips entirely.
  obj <- ptr_shared(formulas)
  expect_s3_class(obj, "ptr_shared_spec")
  expect_equal(obj$panel_keys, character(0))
})

# --- structured-field shape check ------------------------------------------

test_that("abort carries structured fields (class + key + sources + formulas)", {
  formulas <- c(
    "ggplot(mtcars, aes(x = ppVar(shared = 'col'))) + geom_point()",
    "ggplot(iris,   aes(x = ppVar(shared = 'col'))) + geom_point()"
  )
  err <- tryCatch(
    ptr_shared(formulas),
    ptr_panel_consumer_source_mismatch = function(e) e
  )
  expect_s3_class(err, "ptr_panel_consumer_source_mismatch")
  expect_equal(err$key, "col")
  expect_equal(length(err$formulas), 2L)
  expect_equal(length(err$sources), 2L)
})
