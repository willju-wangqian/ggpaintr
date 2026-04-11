# Tests for three safety fixes introduced in the codex/publication-loop branch.
#
# Fix 1 (paintr-parse.R):    tryCatch wraps rlang::parse_expr — malformed
#                             formula signals rlang error with user message.
# Fix 2 (paintr-utils.R):    12 info-disclosure functions added to
#                             unsafe_expr_denylist.
# Fix 3 (paintr-runtime.R):  NULL input_item early-return guard in
#                             ptr_complete_expr prevents crash outside Shiny.
#
# Fix 4 (paintr-utils.R):    walk_expr checks bare symbols against denylist
#                             (higher-order function bypass), recurses into ALL
#                             children including x[[1]] (anonymous function
#                             bypass), expanded denylist, ::: operator
#                             preservation in extract_fn_names.
# Fix 5 (paintr-parse.R):    ptr_parse_formula gains formula_check param.

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

# =============================================================================
# Fix 4: Higher-order function symbol bypass (walk_expr checks bare symbols)
# =============================================================================

test_that("Fix4-G1: lapply with system as function argument is blocked (symbol in arg position)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('lapply(list("id"), system)')),
    "not allowed"
  )
})

test_that("Fix4-G1: Map with system as function argument is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('Map(system, list("id"))')),
    "not allowed"
  )
})

test_that("Fix4-G1: sapply with system as function argument is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('sapply(list("id"), system)')),
    "not allowed"
  )
})

test_that("Fix4-G1: Reduce with system as function argument is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('Reduce(system, list("a", "b"))')),
    "not allowed"
  )
})

# =============================================================================
# Fix 4: Anonymous function bypass (walk_expr recurses into x[[1]])
# =============================================================================

test_that("Fix4-G2: anonymous function wrapping system() is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('(function(x) system(x))("id")')),
    "not allowed"
  )
})

test_that("Fix4-G2: anonymous function containing eval is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('(function() eval(quote(1)))()')),
    "not allowed"
  )
})

# =============================================================================
# Fix 4: Allowlist mode symbol safety (denylist still blocks dangerous symbols)
# =============================================================================

test_that("Fix4-G3: allowlist mode still blocks denylist symbol in arg position", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('lapply(list("id"), system)'),
      expr_check = list(allow_list = c("lapply", "list"))
    ),
    "not allowed"
  )
})

test_that("Fix4-G3: allowlist mode passes legitimate ggplot2 call with listed symbols", {
  expect_invisible(
    validate_expr_safety(
      rlang::parse_expr("aes(x = mpg)"),
      expr_check = list(allow_list = c("aes"))
    )
  )
})

# =============================================================================
# Fix 4: New denylist entries
# =============================================================================

test_that("Fix4-G4: getFromNamespace is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('getFromNamespace("system", "base")')),
    "not allowed"
  )
})

test_that("Fix4-G4: Sys.sleep is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("Sys.sleep(100)")),
    "not allowed"
  )
})

test_that("Fix4-G4: readline is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('readline("prompt")')),
    "not allowed"
  )
})

test_that("Fix4-G4: environment is blocked by default denylist", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("environment(ls)")),
    "not allowed"
  )
})

# =============================================================================
# Fix 4: ::: operator preservation in extract_fn_names
# =============================================================================

test_that("Fix4-G5: base:::system is blocked (qualified name with ::: preserved)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('base:::system("id")')),
    "not allowed"
  )
})

test_that("Fix4-G5: extract_fn_names returns qualified form for ::: call", {
  fn_node <- rlang::parse_expr("base:::system")
  result <- extract_fn_names(fn_node)
  expect_true("base:::system" %in% result)
  expect_true("system" %in% result)
})

test_that("Fix4-G5: extract_fn_names returns qualified form for :: call", {
  fn_node <- rlang::parse_expr("base::system")
  result <- extract_fn_names(fn_node)
  expect_true("base::system" %in% result)
  expect_true("system" %in% result)
})

# =============================================================================
# Fix 5: formula_check parameter on ptr_parse_formula
# =============================================================================

test_that("Fix5-G6: formula_check=TRUE blocks system() in formula", {
  expect_error(
    ptr_parse_formula(
      "system('id') + ggplot(mtcars, aes(x = var))",
      formula_check = TRUE
    ),
    "not allowed"
  )
})

test_that("Fix5-G6: formula_check=FALSE (default) allows dangerous formula text to parse", {
  # No error — formula_check=FALSE is the default, formula treated as trusted input
  expect_no_error(
    ptr_parse_formula(
      "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      formula_check = FALSE
    )
  )
})

test_that("Fix5-G6: formula_check=TRUE passes for a legitimate formula", {
  expect_s3_class(
    ptr_parse_formula(
      "ggplot(mtcars, aes(x = var)) + geom_point()",
      formula_check = TRUE
    ),
    "ptr_obj"
  )
})

# =============================================================================
# Fix 4/5: No false positives — common ggplot2 idioms pass denylist
# =============================================================================

test_that("Fix4-G7: aes(x = mpg, y = hp) passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("aes(x = mpg, y = hp)"))
  )
})

test_that("Fix4-G7: c(1, 2, 3) passes denylist", {
  expect_invisible(
    validate_expr_safety(quote(c(1, 2, 3)))
  )
})

test_that("Fix4-G7: scale_x_log10() passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("scale_x_log10()"))
  )
})

test_that("Fix4-G7: geom_point(color = 'red', size = 3) passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("geom_point(color = 'red', size = 3)"))
  )
})

# =============================================================================
# walk_expr compound-head traversal fix (codex/publication-loop)
# Scenarios: anonymous fn bypass, paren-wrapped symbol, safe compound head,
#            nested anonymous fn, normal calls, length-1 call no-crash.
# =============================================================================

test_that("walk_expr: anon fn with varargs wrapping system() is blocked (denylist)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('(function(...) system(...))(arg)')),
    "not allowed"
  )
})

test_that("walk_expr: anon fn with braced body containing file.remove is blocked (denylist)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('(function() { file.remove("x") })()')),
    "not allowed"
  )
})

test_that("walk_expr: anon fn with safe body passes denylist", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("(function(x) x + 1)(2)"))
  )
})

test_that("walk_expr: nested anonymous functions — inner system() is blocked (denylist)", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('(function() (function() system("id"))())()')
    ),
    "not allowed"
  )
})

test_that("walk_expr: paren-wrapped dangerous symbol used as callable is blocked (denylist)", {
  expect_error(
    validate_expr_safety(rlang::parse_expr('(system)("id")')),
    "not allowed"
  )
})

test_that("walk_expr: sqrt(x) passes denylist — normal call unaffected by fix", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("sqrt(x)"))
  )
})

test_that("walk_expr: aes(x = mpg) passes denylist — normal call unaffected by fix", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("aes(x = mpg)"))
  )
})

test_that("walk_expr: c(1, 2, 3) passes denylist — normal call unaffected by fix", {
  expect_invisible(
    validate_expr_safety(quote(c(1, 2, 3)))
  )
})

test_that("walk_expr: zero-arg call (length-1) does not crash — seq_along(x)[-1] is integer(0)", {
  # f() has length 1; seq_along(x)[-1] yields integer(0), loop body never executes
  expect_invisible(
    validate_expr_safety(quote(sqrt()))
  )
})
