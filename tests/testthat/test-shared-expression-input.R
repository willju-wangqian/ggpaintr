# Contract tests for PLAN ptr-shared-expression-input
# (dev/plans/ptr-shared-expression-input/index.html).
#
# Derived ONLY from the plan's frozen Interface Contract + BDD scenarios,
# at the public ptr_shared() boundary. No impl of normalize_shared_formulas()
# is read or assumed; expectations come from the contract and from the
# partition semantics (key referenced in >=2 formulas -> panel key).
#
# Written red-first: these must FAIL on the pre-change code (which rejects any
# non-string element with the cryptic vapply() assertion message), except the
# all-string regression guard (SC6), which is green-by-design before AND after.

# --- shared fixtures: byte-equivalent string vs quoted-expression formulas ---
# Cross-formula shared key "col" (referenced in both) -> a panel key.
FS1 <- 'ggplot(mtcars, aes(x = ppVar(shared = "col"), y = mpg)) + geom_point()'
FS2 <- 'ggplot(mtcars, aes(x = ppVar(shared = "col"), y = hp)) + geom_line()'

FE1 <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(shared = "col"), y = mpg)) + geom_point()
)
FE2 <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(shared = "col"), y = hp)) + geom_line()
)

# ---------------------------------------------------------------------------
# Scenario "all-expression list equals all-string spec"  (SC1; contract worked
# example #1; Interface-Contract output spec: spec fields equal the all-string
# build). Hand-derived oracle: key in 2 formulas -> panel_keys == "col",
# formula_count == 2L.
test_that("EXPR-ACCEPT: all-quoted-expression formulas build the same spec as strings", {
  expr_spec <- ptr_shared(formulas = list(FE1, FE2))
  str_spec  <- ptr_shared(formulas = list(FS1, FS2))

  expect_s3_class(expr_spec, "ptr_shared_spec")

  # equality on the four contract-named fields
  expect_equal(expr_spec$panel_keys,            str_spec$panel_keys)
  expect_equal(expr_spec$local_keys_by_formula, str_spec$local_keys_by_formula)
  expect_equal(expr_spec$formula_keys,          str_spec$formula_keys)
  expect_equal(expr_spec$formula_count,         str_spec$formula_count)

  # independent semantic oracle (partition rule), not derived from the string spec
  expect_equal(expr_spec$panel_keys,    "col")
  expect_equal(expr_spec$formula_count, 2L)
})

# ---------------------------------------------------------------------------
# Scenario "spec stores normalized strings"  (SC2; contract: obj$formulas holds
# character strings, not raw language objects).
test_that("SPEC-STRINGS: obj$formulas are character strings after expression input", {
  expr_spec <- ptr_shared(formulas = list(FE1, FE2))

  expect_true(is.list(expr_spec$formulas) || is.character(expr_spec$formulas))
  expect_length(expr_spec$formulas, 2L)
  expect_true(all(vapply(expr_spec$formulas, rlang::is_string, logical(1))))
})

# ---------------------------------------------------------------------------
# Scenario "mixed string-and-expression list"  (SC3; contract: mixed lists
# allowed, equal to all-string spec).
test_that("MIXED: a list mixing a string and a quoted expression builds the all-string spec", {
  mixed_spec <- ptr_shared(formulas = list(FS1, FE2))
  str_spec   <- ptr_shared(formulas = list(FS1, FS2))

  expect_equal(mixed_spec$panel_keys,            str_spec$panel_keys)
  expect_equal(mixed_spec$local_keys_by_formula, str_spec$local_keys_by_formula)
  expect_equal(mixed_spec$formula_keys,          str_spec$formula_keys)
  expect_equal(mixed_spec$formula_count,         str_spec$formula_count)
})

# ---------------------------------------------------------------------------
# Scenario "built ggplot object is rejected with a guiding message"  (SC4;
# contract error mode: built ggplot/gg object -> abort naming the 1-based
# index, the class, and mentioning expr().)  Negative space.
test_that("REJECT-GGPLOT: a built ggplot object element is rejected with index + class + expr() guidance", {
  gobj <- ggplot2::ggplot(mtcars, ggplot2::aes(x = ppVar(mpg))) +
    ggplot2::geom_point()

  err <- expect_error(ptr_shared(formulas = list(gobj, FS2)))
  msg <- conditionMessage(err)
  expect_match(msg, "formulas\\[\\[1\\]\\]")   # 1-based index named
  expect_match(msg, "ggplot")                  # offending class named
  expect_match(msg, "expr\\(")                 # remediation: quote it
})

# ---------------------------------------------------------------------------
# Scenario "non-language non-string element rejected"  (SC5; contract error
# mode: numeric/other -> abort naming index + class.)  Negative space.
test_that("REJECT-NUMERIC: a numeric formula element is rejected naming index + class", {
  err <- expect_error(ptr_shared(formulas = list(42, FS2)))
  msg <- conditionMessage(err)
  expect_match(msg, "formulas\\[\\[1\\]\\]")
  expect_match(msg, "numeric")
})

# ---------------------------------------------------------------------------
# Boundary: Interface-Contract input domain says formulas must be a NON-EMPTY
# list/character vector. Empty -> abort mentioning "non-empty".
test_that("REJECT-EMPTY: an empty formulas list is rejected as non-empty", {
  err <- expect_error(ptr_shared(formulas = list()))
  expect_match(conditionMessage(err), "non-empty")
})

# ---------------------------------------------------------------------------
# Scenario "existing all-string call is unchanged"  (SC6).
# REGRESSION GUARD: green-by-design before AND after the change (the string
# path is byte-identical). It is intentionally NOT part of the red-first
# evidence; its teeth are against a hypothetical impl that mangles strings.
test_that("REGRESSION-STRING: an all-string c() call still builds the cross-formula panel spec", {
  spec <- ptr_shared(formulas = c(FS1, FS2))
  expect_s3_class(spec, "ptr_shared_spec")
  expect_equal(spec$panel_keys, "col")
  expect_equal(spec$formula_count, 2L)
})
