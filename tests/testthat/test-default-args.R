# Unit tests for default-validator helpers and the constant-fold registry
# (PLAN-01 of ADR-0009).

# ---- helpers ---------------------------------------------------------------

local_clean_constant_fold <- function(.env = parent.frame()) {
  # Save and restore the constant-fold registry so tests that register or
  # clear extra entries can't leak across tests.
  saved <- ls(envir = ptr_constant_fold_env, all.names = TRUE)
  saved_vals <- mget(saved, envir = ptr_constant_fold_env)
  withr::defer({
    rm(list = ls(envir = ptr_constant_fold_env, all.names = TRUE),
       envir = ptr_constant_fold_env)
    for (nm in names(saved_vals)) {
      assign(nm, saved_vals[[nm]], envir = ptr_constant_fold_env)
    }
  }, envir = .env)
}

# ---- ptr_arg_symbol_or_string ------------------------------------------

test_that("ptr_arg_symbol_or_string accepts a bareword symbol", {
  v <- ptr_arg_symbol_or_string()
  expect_equal(v(quote(mpg)), "mpg")
})

test_that("ptr_arg_symbol_or_string accepts a string literal verbatim", {
  v <- ptr_arg_symbol_or_string()
  expect_equal(v("miles per gallon"), "miles per gallon")
})

test_that("ptr_arg_symbol_or_string accepts the empty string (A3)", {
  v <- ptr_arg_symbol_or_string()
  expect_equal(v(""), "")
})

test_that("ptr_arg_symbol_or_string preserves backticked names (A4)", {
  v <- ptr_arg_symbol_or_string()
  expr <- rlang::parse_expr("`miles per gallon`")
  expect_equal(v(expr), "miles per gallon")
})

test_that("ptr_arg_symbol_or_string rejects numbers and calls", {
  v <- ptr_arg_symbol_or_string()
  expect_error(v(5), class = "rlang_error")
  expect_error(v(quote(c(1, 2))), class = "rlang_error")
})

# ---- ptr_arg_string ----------------------------------------------------

test_that("ptr_arg_string accepts a string literal", {
  v <- ptr_arg_string()
  expect_equal(v("hi"), "hi")
})

test_that("ptr_arg_string accepts the empty string (A3)", {
  v <- ptr_arg_string()
  expect_equal(v(""), "")
})

test_that("ptr_arg_string rejects a symbol or number", {
  v <- ptr_arg_string()
  expect_error(v(quote(mpg)), class = "rlang_error")
  expect_error(v(5), class = "rlang_error")
})

# ---- ptr_arg_numeric ---------------------------------------------------

test_that("ptr_arg_numeric accepts numeric literals (incl. negative, sci)", {
  v <- ptr_arg_numeric()
  expect_equal(v(5), 5)
  expect_equal(v(-5), -5)
  expect_equal(v(2.5e3), 2500)
})

test_that("ptr_arg_numeric accepts unary minus on a literal AST", {
  v <- ptr_arg_numeric()
  expect_equal(v(quote(-5)), -5)
})

test_that("ptr_arg_numeric accepts `2 * pi` via baseenv parent", {
  v <- ptr_arg_numeric()
  expect_equal(v(quote(2 * pi)), 2 * pi)
})

test_that("ptr_arg_numeric rejects symbols not in allowlist", {
  v <- ptr_arg_numeric()
  expect_error(
    v(quote(5L * x)),
    regexp = "x.*not in constant-fold allowlist",
    class = "rlang_error"
  )
})

test_that("ptr_arg_numeric rejects unregistered functions", {
  v <- ptr_arg_numeric()
  expect_error(
    v(quote(log10(100))),
    regexp = "log10.*not in constant-fold allowlist",
    class = "rlang_error"
  )
})

test_that("ptr_arg_numeric rejects multi-element vectors", {
  v <- ptr_arg_numeric()
  expect_error(v(quote(c(1, 2))), class = "rlang_error")
})

# ---- ptr_arg_numeric_vector --------------------------------------------

test_that("ptr_arg_numeric_vector accepts c(0, 1) and c(-1, 1) at length=2L", {
  v <- ptr_arg_numeric_vector(length = 2L)
  expect_equal(v(quote(c(0, 1))), c(0, 1))
  expect_equal(v(quote(c(-1, 1))), c(-1, 1))
})

test_that("ptr_arg_numeric_vector accepts seq() via constant-fold", {
  v <- ptr_arg_numeric_vector()
  expect_equal(v(quote(seq(0, 1, by = 0.1))), seq(0, 1, by = 0.1))
})

test_that("ptr_arg_numeric_vector enforces explicit length when given", {
  v2 <- ptr_arg_numeric_vector(length = 2L)
  expect_error(v2(quote(c(0))), regexp = "length 2", class = "rlang_error")
})

test_that("ptr_arg_numeric_vector rejects system() in any position", {
  v <- ptr_arg_numeric_vector()
  expect_error(
    v(quote(c(0, system("rm")))),
    regexp = "system.*not in constant-fold allowlist",
    class = "rlang_error"
  )
})

test_that("ptr_arg_numeric_vector validates the `length` argument", {
  expect_error(ptr_arg_numeric_vector(length = "two"), class = "rlang_error")
  expect_error(ptr_arg_numeric_vector(length = -1L), class = "rlang_error")
})

# ---- ptr_arg_expression ------------------------------------------------

test_that("ptr_arg_expression returns its input unchanged", {
  v <- ptr_arg_expression()
  e <- quote(x + 1)
  expect_identical(v(e), e)
})

test_that("ptr_arg_expression does not warn on bare expressions", {
  v <- ptr_arg_expression()
  expect_no_warning(v(quote(x + 1)))
})

test_that("ptr_arg_expression warns on quote() wrapper (A5.b)", {
  v <- ptr_arg_expression()
  e <- quote(quote(x + 1))
  expect_warning(out <- v(e), regexp = "already captures")
  expect_identical(out, e)
})

test_that("ptr_arg_expression warns on rlang::expr() wrapper (A5.b)", {
  v <- ptr_arg_expression()
  e <- quote(rlang::expr(x + 1))
  expect_warning(out <- v(e), regexp = "already captures")
  expect_identical(out, e)
})

test_that("ptr_arg_expression warns on bquote() and rlang::quo() wrappers", {
  v <- ptr_arg_expression()
  expect_warning(v(quote(bquote(x + 1))), regexp = "already captures")
  expect_warning(v(quote(rlang::quo(x + 1))), regexp = "already captures")
})

test_that("ptr_arg_expression stores system() verbatim (never evals)", {
  v <- ptr_arg_expression()
  e <- quote(system("rm -rf /"))
  expect_identical(v(e), e)
})

# ---- constant-fold registry lifecycle --------------------------------------

test_that("ptr_constant_fold_keywords lists built-ins after package load", {
  kw <- ptr_constant_fold_keywords()
  for (nm in c("+", "-", "*", "/", "^", "%%", "%/%",
               ":", "c", "seq", "seq.int", "seq_len", "seq_along")) {
    expect_true(nm %in% kw, info = nm)
  }
})

test_that("ptr_register_constant_fold lets authors extend the allowlist", {
  local_clean_constant_fold()
  ptr_register_constant_fold("log10", log10)
  expect_equal(ptr_arg_numeric()(quote(log10(100))), 2)
})

test_that("ptr_clear_constant_fold(NULL) re-seeds built-ins", {
  local_clean_constant_fold()
  ptr_register_constant_fold("log10", log10)
  ptr_clear_constant_fold()
  kw <- ptr_constant_fold_keywords()
  expect_false("log10" %in% kw)
  expect_true("+" %in% kw)
  expect_true("c" %in% kw)
  expect_true("seq" %in% kw)
})

test_that("ptr_clear_constant_fold(name) drops only that name", {
  local_clean_constant_fold()
  ptr_register_constant_fold("log10", log10)
  expect_true("log10" %in% ptr_constant_fold_keywords())
  ptr_clear_constant_fold("log10")
  expect_false("log10" %in% ptr_constant_fold_keywords())
  expect_true("c" %in% ptr_constant_fold_keywords())
})

test_that("ptr_register_constant_fold validates name", {
  expect_error(ptr_register_constant_fold("", identity), class = "rlang_error")
  expect_error(ptr_register_constant_fold(c("a", "b"), identity),
               class = "rlang_error")
})

# ---- defence-in-depth ------------------------------------------------------

test_that("validators never call eval() on rejecting input (system not run)", {
  # Sanity: validate_constant_ast aborts before eval_bare runs. We can't
  # observe a "system did not run" directly, but we can confirm the AST
  # walker aborts on the system() shape for every numeric validator.
  expect_error(ptr_arg_numeric()(quote(system("rm"))),
               regexp = "system.*not in constant-fold allowlist")
  expect_error(ptr_arg_numeric_vector()(quote(system("rm"))),
               regexp = "system.*not in constant-fold allowlist")
})
