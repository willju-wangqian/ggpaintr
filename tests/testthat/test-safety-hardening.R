# Tests for 5 safety-hardening changes introduced in codex/publication-loop.
#
# Change 1 (paintr-utils.R): walk_expr compound-head restructuring — compound
#   heads (call in x[[1]] position) are recursed into; extract_fn_names is
#   skipped for them so IIFEs are walked structurally rather than by name.
#
# Change 2 (paintr-utils.R): Denylist additions — getAnywhere, exists, find,
#   loadedNamespaces added to unsafe_expr_denylist.
#
# Change 3 (paintr-parse.R): formula_check default changed from FALSE to TRUE.
#
# Change 4 (paintr-utils.R): walk_expr now recurses into pairlist nodes so
#   denylisted symbols used as inline function default values are caught.
#
# Change 5 (paintr-utils.R): get_index_path no longer over-collects `data =`
#   arguments whose value is not a placeholder keyword.

# =============================================================================
# Change 1: walk_expr compound-head restructuring (IIFE / namespaced calls)
# =============================================================================

test_that("C1: IIFE wrapping denylisted inner call is blocked (denylist)", {
  # (function() system("x"))()  — compound head, inner body contains system()
  expect_error(
    validate_expr_safety(rlang::parse_expr('(function() system("x"))()')),
    "not allowed"
  )
})

test_that("C1: IIFE wrapping only safe calls passes denylist", {
  # (function() ggplot())()  — compound head, inner body is safe
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function() ggplot())()"))
  )
})

test_that("C1: base::system() is blocked — namespaced denylisted call", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('base::system("x")')),
    "not allowed"
  )
})

test_that("C1: ggplot2::ggplot() passes denylist — namespaced safe call", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("ggplot2::ggplot()"))
  )
})

test_that("C1: deeply nested IIFE with system() is blocked", {
  # Compound head inside compound head
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('(function() (function() system("rm"))())()')
    ),
    "not allowed"
  )
})

test_that("C1: IIFE with safe multi-arg body passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(x, y) x + y)(1, 2)"))
  )
})

# =============================================================================
# Change 2: New denylist entries (getAnywhere, exists, find, loadedNamespaces)
# =============================================================================

test_that("C2: getAnywhere() call is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('getAnywhere("system")')),
    "not allowed"
  )
})

test_that("C2: exists() call is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('exists("secret_var")')),
    "not allowed"
  )
})

test_that("C2: find() call is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('find("system")')),
    "not allowed"
  )
})

test_that("C2: loadedNamespaces() call is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("loadedNamespaces()")),
    "not allowed"
  )
})

test_that("C2: getAnywhere as bare symbol is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), getAnywhere)")),
    "not allowed"
  )
})

test_that("C2: exists as bare symbol is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(nms, exists)")),
    "not allowed"
  )
})

test_that("C2: find as bare symbol is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(nms, find)")),
    "not allowed"
  )
})

test_that("C2: loadedNamespaces as bare symbol is blocked by denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("(loadedNamespaces)")),
    "not allowed"
  )
})

test_that("C2: safe ggplot2 calls unaffected after denylist additions", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("ggplot(data = mtcars, aes(x = mpg))"))
  )
})

# =============================================================================
# Change 3: formula_check default is now TRUE
# =============================================================================

test_that("C3: ptr_parse_formula blocks system() with NO formula_check arg (default=TRUE)", {
  expect_error(
    ptr_parse_formula("system('x') + ggplot()"),
    "not allowed"
  )
})

test_that("C3: ptr_parse_formula with formula_check=FALSE does NOT abort on dangerous formula", {
  # formula_check=FALSE treats input as trusted developer code
  expect_no_error(
    ptr_parse_formula(
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      formula_check = FALSE
    )
  )
})

test_that("C3: ptr_parse_formula passes for a legitimate formula with default formula_check", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  expect_s3_class(obj, "ptr_obj")
})

test_that("C3: ptr_parse_formula blocks exists() in formula with default formula_check", {
  # Exercises that the new denylist entries also fire through the default path
  expect_error(
    ptr_parse_formula('exists("x") + ggplot()'),
    "not allowed"
  )
})

test_that("C3: ptr_parse_formula blocks getAnywhere() in formula with default formula_check", {
  expect_error(
    ptr_parse_formula('getAnywhere("system") + ggplot()'),
    "not allowed"
  )
})

# =============================================================================
# Change 4: walk_expr recurses into pairlist (inline function default values)
# =============================================================================

test_that("C4: denylisted symbol as default value is blocked (system default)", {
  # (function(x = system) x)()  — system is a denylist symbol in formal default
  expect_error(
    validate_expr_safety(rlang::parse_expr("(function(x = system) x)()")),
    "not allowed"
  )
})

test_that("C4: denylisted symbol as default value is blocked (eval default)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("(function(x = eval) x)()")),
    "not allowed"
  )
})

test_that("C4: denylisted call as default value is blocked", {
  # Default that *calls* a denylisted function
  expect_error(
    validate_expr_safety(rlang::parse_expr('(function(x = system("id")) x)()')),
    "not allowed"
  )
})

test_that("C4: safe numeric default passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(x = 1) x + 1)()"))
  )
})

test_that("C4: missing (empty symbol) default passes denylist", {
  # (function(x) x)() — x has no default; its formal entry is an empty symbol
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(x) x)()"))
  )
})

test_that("C4: NULL default passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(x = NULL) x)()"))
  )
})

test_that("C4: safe function symbol as default passes denylist (mean is not denylisted)", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(x = mean) x)()"))
  )
})

test_that("C4: multiple formals — one denylisted default triggers abort", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("(function(a = 1, b = system, c = 2) a)()")),
    "not allowed"
  )
})

test_that("C4: multiple safe formals all pass denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(a = 1, b = mean, c = NULL) a)()"))
  )
})

# =============================================================================
# Change 5: get_index_path no longer over-collects non-placeholder `data =`
# =============================================================================

test_that("C5: ggplot(data = mtcars, aes(x = var)) — only var path collected, not mtcars", {
  expr <- quote(ggplot(data = mtcars, aes(x = var)))
  paths <- get_index_path(expr)
  # Should find exactly 1 path (for var), not 2 (mtcars is not a placeholder keyword)
  expect_length(paths, 1L)
})

test_that("C5: geom_point(data = upload, aes(x = var)) — both upload and var are collected", {
  # upload IS a placeholder keyword in the default target
  expr <- quote(geom_point(data = upload, aes(x = var)))
  paths <- get_index_path(expr)
  expect_length(paths, 2L)
})

test_that("C5: geom_smooth(data = mydf) — mydf is not a keyword, 0 paths collected", {
  expr <- quote(geom_smooth(data = mydf))
  paths <- get_index_path(expr)
  expect_length(paths, 0L)
})

test_that("C5: ggplot(data = upload, aes(x = var, y = var)) — all 3 placeholder paths collected", {
  expr <- quote(ggplot(data = upload, aes(x = var, y = var)))
  paths <- get_index_path(expr)
  expect_length(paths, 3L)
})

test_that("C5: ggplot(data = mtcars, aes(x = var, y = var)) — exactly 2 paths (both var), not 3", {
  expr <- quote(ggplot(data = mtcars, aes(x = var, y = var)))
  paths <- get_index_path(expr)
  expect_length(paths, 2L)
})

test_that("C5: custom target — only listed keywords collected, non-keyword data= skipped", {
  expr <- quote(ggplot(data = iris, aes(x = num, y = text)))
  paths <- get_index_path(expr, target = c("num", "text"))
  expect_length(paths, 2L)
})

test_that("C5: custom target — data= value in target IS collected", {
  expr <- quote(ggplot(data = num, aes(x = text)))
  paths <- get_index_path(expr, target = c("num", "text"))
  expect_length(paths, 2L)
})
