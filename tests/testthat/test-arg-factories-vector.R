# Tests for PLAN-02 (ADR 0027 D3): vector= validator factories + ptr_arg_symbol.
# Derived from the plan's Interface Contract + BDD ONLY (implementation-blind:
# the vector=/ptr_arg_symbol behavior was NOT read from source). Most are RED until
# R/paintr-default-args.R gains vector=/length=/ptr_arg_symbol. The two cases tagged
# [characterization] are GREEN-NOW regression guards: PLAN-02 must PRESERVE the
# existing scalar form and the ptr_arg_numeric_vector alias — they must stay green.

# ---- RED-first: new contract surface ---------------------------------------

test_that("ptr_arg_numeric(vector = TRUE) returns the vector [PLAN-02 IC worked example]", {
  expect_identical(ptr_arg_numeric(vector = TRUE)(quote(c(1, 2))), c(1, 2))
})

test_that("ptr_arg_numeric(vector = TRUE, length = 2L) enforces length [PLAN-02 BDD]", {
  expect_error(ptr_arg_numeric(vector = TRUE, length = 2L)(quote(c(1, 2, 3))), "length 2")
})

test_that("ptr_arg_symbol() parses a bare name to a string [PLAN-02 IC worked example]", {
  expect_identical(ptr_arg_symbol()(quote(mpg)), "mpg")
})

test_that("ptr_arg_symbol(vector = TRUE) parses bare names to a character vector [PLAN-02 BDD]", {
  expect_identical(ptr_arg_symbol(vector = TRUE)(quote(c(mpg, hp))), c("mpg", "hp"))
})

test_that("ptr_arg_symbol() rejects a non-symbol literal [PLAN-02 BDD: negative space]", {
  expect_error(ptr_arg_symbol()(quote(1 + 1)), "column name")
})

test_that("ptr_arg_string(vector = TRUE) returns the string vector [PLAN-02 IC]", {
  expect_identical(ptr_arg_string(vector = TRUE)(quote(c("a", "b"))), c("a", "b"))
})

test_that("ptr_arg_symbol_or_string(vector = TRUE) mixes symbols and strings [PLAN-02 IC worked example]", {
  expect_identical(ptr_arg_symbol_or_string(vector = TRUE)(quote(c(mpg, "hp"))), c("mpg", "hp"))
})

# ---- characterization (GREEN-NOW regression guards) ------------------------

test_that("ptr_arg_numeric() scalar default preserved [PLAN-02 IC; characterization]", {
  expect_identical(ptr_arg_numeric()(quote(5)), 5)
})

test_that("ptr_arg_numeric_vector() alias preserved this round [PLAN-02 IC; characterization]", {
  expect_identical(ptr_arg_numeric_vector()(quote(c(1, 2))), c(1, 2))
})
