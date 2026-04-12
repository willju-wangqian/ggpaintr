# Tests for fixes in ptr_merge_placeholders, ptr_resolve_placeholder_expr, and
# ptr_define_placeholder_context / ptr_bind_placeholder_ui.

# Helper: placeholder with a custom resolve_expr return value ---------------

make_resolve_expr_ph <- function(return_fn) {
  ptr_define_placeholder(
    keyword      = "rph",
    build_ui     = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) return_fn()
  )
}

# =============================================================================
# Fix 3: ptr_merge_placeholders composability — existing registry returned
#         unchanged; plain list of custom placeholders still works
# =============================================================================

test_that("Fix3: passing an existing registry to ptr_merge_placeholders returns it unchanged", {
  registry <- ptr_merge_placeholders(list(date = make_test_date_placeholder()))

  result <- ptr_merge_placeholders(registry)
  expect_identical(result, registry)
})

test_that("Fix3: passing a plain list of custom placeholders builds a registry", {
  registry <- ptr_merge_placeholders(list(date = make_test_date_placeholder()))

  expect_s3_class(registry, "ptr_define_placeholder_registry")
  expect_true("date" %in% names(registry))
  expect_true("var"  %in% names(registry))  # built-ins still present
})

test_that("Fix3: NULL input returns the default built-in registry", {
  registry <- ptr_merge_placeholders()

  expect_s3_class(registry, "ptr_define_placeholder_registry")
  expect_true(all(c("var", "text", "num", "expr", "upload") %in% names(registry)))
})

# =============================================================================
# Fix 4: type whitelist in ptr_resolve_placeholder_expr
# =============================================================================

# We exercise the whitelist through ptr_exec, which calls
# ptr_resolve_placeholder_expr internally.

make_typed_return_ph <- function(keyword, return_val_fn) {
  ptr_define_placeholder(
    keyword      = keyword,
    build_ui     = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) return_val_fn(value)
  )
}

# A minimal formula that uses a custom placeholder at a safe position.
typed_formula <- paste(
  "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +",
  "geom_point(alpha = tph)"
)

exec_typed <- function(registry, value) {
  obj <- ptr_parse_formula(typed_formula, placeholders = registry)
  ptr_complete_expr(
    obj,
    list(
      "geom_point+checkbox" = TRUE,
      "geom_point+2" = value
    )
  )
}

test_that("Fix4: numeric return value passes through the type whitelist", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) as.numeric(v))
  ))
  result <- exec_typed(reg, 0.5)
  expect_match(result$code_text, "alpha = 0.5")
})

test_that("Fix4: logical return value passes through the type whitelist", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) TRUE)
  ))
  result <- exec_typed(reg, "ignored")
  expect_match(result$code_text, "alpha = TRUE")
})

test_that("Fix4: integer return value passes through the type whitelist", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) 1L)
  ))
  result <- exec_typed(reg, "ignored")
  expect_match(result$code_text, "alpha = 1")
})

test_that("Fix4: NULL return value passes through the type whitelist without error", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) NULL)
  ))
  # NULL means missing-arg sentinel; should not abort
  expect_no_error(exec_typed(reg, "ignored"))
})

test_that("Fix4: environment return value triggers abort with type in message", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) new.env(parent = emptyenv()))
  ))
  obj <- ptr_parse_formula(typed_formula, placeholders = reg)
  expect_error(
    ptr_complete_expr(
      obj,
      list("geom_point+checkbox" = TRUE, "geom_point+2" = "x")
    ),
    "unsupported type"
  )
})

test_that("Fix4: raw return value triggers abort with type in message", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) as.raw(0x01))
  ))
  obj <- ptr_parse_formula(typed_formula, placeholders = reg)
  expect_error(
    ptr_complete_expr(
      obj,
      list("geom_point+checkbox" = TRUE, "geom_point+2" = "x")
    ),
    "unsupported type"
  )
})

test_that("Fix4: list return value triggers abort with type in message", {
  reg <- ptr_merge_placeholders(list(
    tph = make_typed_return_ph("tph", function(v) list(a = 1))
  ))
  obj <- ptr_parse_formula(typed_formula, placeholders = reg)
  expect_error(
    ptr_complete_expr(
      obj,
      list("geom_point+checkbox" = TRUE, "geom_point+2" = "x")
    ),
    "unsupported type"
  )
})

# =============================================================================
# Fix 5: ptr_define_placeholder_context includes eval_env and var_column_map
# =============================================================================

test_that("Fix5: ptr_define_placeholder_context includes eval_env when passed", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )
  e <- new.env(parent = emptyenv())
  ctx <- ptr_define_placeholder_context(obj, eval_env = e)

  expect_identical(ctx$eval_env, e)
})

test_that("Fix5: ptr_define_placeholder_context includes var_column_map when passed", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )
  vcm <- list(col_a = "col_a", col_b = "col_b")
  ctx <- ptr_define_placeholder_context(obj, var_column_map = vcm)

  expect_identical(ctx$var_column_map, vcm)
})

test_that("Fix5: ptr_define_placeholder_context defaults eval_env and var_column_map to NULL", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )
  ctx <- ptr_define_placeholder_context(obj)

  expect_null(ctx$eval_env)
  expect_null(ctx$var_column_map)
})

test_that("W1: context uses reference semantics — mutations persist across calls", {
  obj <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()"
  )
  ctx <- ptr_define_placeholder_context(obj)

  expect_true(is.environment(ctx))

  # Mutations to context persist (reference semantics, not copy-on-modify)
  mutate_context <- function(ctx) {
    ctx$input <- list(test = TRUE)
    ctx$eval_env <- new.env(parent = emptyenv())
  }
  mutate_context(ctx)

  expect_true(isTRUE(ctx$input$test))
  expect_true(is.environment(ctx$eval_env))
})

# =============================================================================
# N2 — ptr_builtin_placeholders() memoization
# =============================================================================

test_that("N2: ptr_builtin_placeholders() returns the same cached object on repeated calls", {
  first  <- ptr_builtin_placeholders()
  second <- ptr_builtin_placeholders()

  expect_identical(first, second)
})

test_that("N2: ptr_builtin_placeholders() cache contains all five built-in keywords", {
  reg <- ptr_builtin_placeholders()
  expect_true(all(c("var", "text", "num", "expr", "upload") %in% names(reg)))
})

# =============================================================================
# N4 — ...‑only formal triggers a warning in ptr_validate_placeholder
# =============================================================================

test_that("N4: hook with only ... formal emits a warning about positional mismatches", {
  expect_warning(
    ptr_define_placeholder(
      keyword      = "dotonly",
      build_ui     = function(...) shiny::textInput("id", "label"),
      resolve_expr = function(value, meta, context) rlang::sym(value)
    ),
    "positional argument mismatches"
  )
})

test_that("N4: hook with only ... formal warning mentions the hook name", {
  expect_warning(
    ptr_define_placeholder(
      keyword      = "dotonly2",
      build_ui     = function(...) shiny::textInput("id", "label"),
      resolve_expr = function(value, meta, context) rlang::sym(value)
    ),
    "build_ui"
  )
})

test_that("N4: hook with correct named formals produces no warning", {
  expect_no_warning(
    ptr_define_placeholder(
      keyword      = "namedph",
      build_ui     = function(id, copy, meta, context) shiny::textInput(id, copy$label),
      resolve_expr = function(value, meta, context) rlang::sym(value)
    )
  )
})

test_that("Fix5: ptr_bind_placeholder_ui works end-to-end (regression)", {
  obj    <- ptr_parse_formula(
    "ggplot(data = iris, aes(x = var, y = var)) + geom_point()"
  )
  output <- list2env(list(), parent = emptyenv())

  # ptr_bind_var_ui_impl emits expected operational warnings when eval_env and
  # var_column_map are not pre-cached; suppress them so the test focuses on
  # absence of errors.
  suppressWarnings(
    expect_no_error(
      ptr_bind_placeholder_ui(list(), output, obj, envir = parent.frame())
    )
  )
})
