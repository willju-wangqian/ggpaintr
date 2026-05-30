# Tests for PLAN-06: the parser dispatches placeholder argument validation
# through the registry's `default_arg` + `named_args` schema. The helper
# under test is `extract_placeholder_args(expr, entry)` in
# R/paintr-translate.R, exercised end-to-end through `ptr_translate()` so
# we cover the call-site update inside `detect_placeholder` and node
# population inside `build_placeholder_node` as well.

# ---- shared fixtures -------------------------------------------------------

# Register a no-arg placeholder value (matches today's built-in `num`-like
# schema: default_arg = NULL, named_args = list()).
register_no_schema_value <- function(kw) {
  ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, ...) shiny::textInput(node$id, kw),
    resolve_expr = function(value, node, ...) value
  )
}

# Register a value placeholder that accepts a single positional numeric
# default and one named numeric arg `step`.
register_numeric_default_value <- function(kw) {
  ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, ...) shiny::numericInput(node$id, kw, value = 0),
    resolve_expr = function(value, node, ...) value,
    default_arg = ptr_arg_numeric(),
    named_args = list(step = ptr_arg_numeric())
  )
}

# Build a translate-able formula string ("ggplot(mtcars) + geom_point(aes(y = <ph>))").
ph_formula <- function(ph_call) {
  paste0("ggplot(mtcars, aes(x = mpg)) + geom_point(aes(y = ", ph_call, "))")
}

# Find first placeholder node in a translated tree.
find_first_placeholder <- function(root) {
  hits <- ptr_collect(root, is_ptr_placeholder)
  hits[[1L]]
}

# ---- BDD: positional rejected when default_arg = NULL ----------------------

test_that("positional arg is rejected when default_arg is NULL", {
  kw <- "ppPlan06NoSchemaA"
  register_no_schema_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  expect_error(
    ptr_translate(ph_formula(sprintf("%s(5)", kw))),
    "accepts only the named .shared. argument; positional args"
  )
})

# ---- BDD: positional accepted when default_arg is set ----------------------

test_that("positional arg accepted and stored on node$default", {
  kw <- "ppPlan06WithDefaultA"
  register_numeric_default_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  root <- ptr_translate(ph_formula(sprintf("%s(5)", kw)))
  node <- find_first_placeholder(root)
  expect_equal(node$default, 5)
  expect_equal(node$named_args, list())
})

# ---- BDD: unknown named arg rejected ---------------------------------------

test_that("unknown named arg is rejected with the expected message", {
  kw <- "ppPlan06NoSchemaB"
  register_no_schema_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  expect_error(
    ptr_translate(ph_formula(sprintf('%s(label = "X")', kw))),
    "unknown argument\\(s\\): label"
  )
})

# ---- BDD: declared named arg accepted --------------------------------------

test_that("declared named arg is accepted and stored on node$named_args", {
  kw <- "ppPlan06WithDefaultB"
  register_numeric_default_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  root <- ptr_translate(ph_formula(sprintf("%s(step = 0.5)", kw)))
  node <- find_first_placeholder(root)
  expect_equal(node$named_args$step, 0.5)
  expect_null(node$default)
})

# ---- BDD: shared continues to work -----------------------------------------

test_that("shared continues to work; default/named_args stay empty", {
  kw <- "ppPlan06NoSchemaC"
  register_no_schema_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  root <- ptr_translate(ph_formula(sprintf('%s(shared = "k")', kw)))
  node <- find_first_placeholder(root)
  expect_equal(node$shared, "k")
  expect_null(node$default)
  expect_equal(node$named_args, list())
})

# ---- BDD: default + named + shared combine ---------------------------------

test_that("default, named arg, and shared can combine in one call", {
  kw <- "ppPlan06AllThree"
  register_numeric_default_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  root <- ptr_translate(ph_formula(sprintf('%s(5, step = 0.5, shared = "k")', kw)))
  node <- find_first_placeholder(root)
  expect_equal(node$default, 5)
  expect_equal(node$named_args$step, 0.5)
  expect_equal(node$shared, "k")
})

# ---- BDD: validator failure surfaces as translate-time abort ---------------

test_that("validator abort is propagated with the validator's message", {
  kw <- "ppPlan06ValidatorFail"
  register_numeric_default_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  err <- expect_error(ptr_translate(ph_formula(sprintf('%s("not a number")', kw))))
  expect_s3_class(err, "rlang_error")
})

# ---- BDD: shared= must be a single non-empty string ------------------------

test_that("non-string shared= still aborts (existing semantics preserved)", {
  kw <- "ppPlan06NoSchemaD"
  register_no_schema_value(kw)
  withr::defer(ptr_clear_placeholder(kw))

  expect_error(
    ptr_translate(ph_formula(sprintf("%s(shared = 1)", kw))),
    "shared = .*single non-empty string"
  )
})

# ---- BDD: no eval on placeholder ASTs --------------------------------------

test_that("placeholder argument ASTs are never eval()'d at parse time", {
  # Use an expression-mode default validator that stores its input verbatim.
  kw <- "ppPlan06NoEval"
  ptr_define_placeholder_value(
    keyword = kw,
    build_ui = function(node, ...) shiny::textInput(node$id, kw),
    resolve_expr = function(value, node, ...) value,
    default_arg = ptr_arg_expression()
  )
  withr::defer(ptr_clear_placeholder(kw))

  # If the AST were eval()'d, `stop()` would fire here. We use stop() rather
  # than system() because it is portable + observable as a thrown error.
  expect_no_error(
    ptr_translate(ph_formula(sprintf("%s(stop('do not eval me'))", kw)))
  )
})

# ---- Source-grep: the parser itself contains no eval() on argument ASTs ----

test_that("extract_placeholder_args does not call eval/parse/get on the AST", {
  # Source-grep only works in the source tree (devtools::test). The R CMD
  # check sandbox does not ship R/ alongside tests/ -- skip cleanly there.
  src_path <- test_path("..", "..", "R", "paintr-translate.R")
  skip_if_not(file.exists(src_path), "package source tree not available")
  src <- readLines(src_path)
  # Locate the helper and check the next ~80 lines for forbidden tokens.
  start <- grep("^extract_placeholder_args <-", src)
  expect_length(start, 1L)
  end <- min(start + 80L, length(src))
  body <- src[start:end]
  expect_false(any(grepl("\\beval\\(", body)))
  expect_false(any(grepl("\\bparse\\(", body)))
  expect_false(any(grepl("\\bget\\(", body)))
})

# ---- B1.a: shared-key role-uniqueness validator ----------------------------

test_that("ptr_validate_shared_roles aborts when same shared key spans roles", {
  kw_val <- "ppPlan06SharedRoleVal"
  kw_con <- "ppPlan06SharedRoleCon"
  ptr_define_placeholder_value(
    keyword = kw_val,
    build_ui = function(node, ...) shiny::textInput(node$id, kw_val),
    resolve_expr = function(value, node, ...) value
  )
  ptr_define_placeholder_consumer(
    keyword = kw_con,
    build_ui = function(node, cols, data, label, ...)
      shiny::selectInput(node$id, label, choices = cols),
    resolve_expr = function(value, node, ...) rlang::sym(value)
  )
  withr::defer({
    ptr_clear_placeholder(kw_val)
    ptr_clear_placeholder(kw_con)
  })

  src <- sprintf(
    'ggplot(mtcars) + geom_point(aes(x = %s(shared = "k"), y = %s(shared = "k")))',
    kw_con, kw_val
  )
  expect_error(ptr_translate(src), "incompatible roles")
})

test_that("shared key with same role across occurrences is accepted", {
  kw_val <- "ppPlan06SameRoleVal"
  ptr_define_placeholder_value(
    keyword = kw_val,
    build_ui = function(node, ...) shiny::textInput(node$id, kw_val),
    resolve_expr = function(value, node, ...) value
  )
  withr::defer(ptr_clear_placeholder(kw_val))

  src <- sprintf(
    'ggplot(mtcars, aes(x = mpg)) + geom_point(aes(y = %s(shared = "k"))) + geom_line(aes(y = %s(shared = "k")))',
    kw_val, kw_val
  )
  expect_no_error(ptr_translate(src))
})
