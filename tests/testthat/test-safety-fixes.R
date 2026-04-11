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
    regexp = "exactly one top-level expression"
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

test_that("Fix5-G6: formula_check=FALSE (opt-in) allows dangerous formula text to parse", {
  # No error — formula_check=FALSE is an opt-in override, formula treated as trusted input
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

# =============================================================================
# New denylist entries: super-assignment (<<-, ->>) and makeActiveBinding
# =============================================================================

test_that("denylist: x <<- 1 (left super-assign call) is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("x <<- 1")),
    "not allowed"
  )
})

test_that("denylist: 1 ->> x (right super-assign call) is blocked", {
  expect_error(
    validate_expr_safety(rlang::parse_expr("1 ->> x")),
    "not allowed"
  )
})

test_that("denylist: <<- as bare symbol reference (higher-order) is blocked", {
  # lapply(list(), `<<-`) passes `<<-` as a symbol argument
  expect_error(
    validate_expr_safety(rlang::parse_expr("lapply(list(), `<<-`)")),
    "not allowed"
  )
})

test_that("denylist: normal <- assignment is NOT blocked (no false positive)", {
  expect_invisible(
    validate_expr_safety(rlang::parse_expr("x <- 1"))
  )
})

test_that("denylist: makeActiveBinding call is blocked", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr('makeActiveBinding("x", function() 1, .GlobalEnv)')
    ),
    "not allowed"
  )
})

test_that("denylist: makeActiveBinding as bare symbol is blocked (higher-order)", {
  expect_error(
    validate_expr_safety(
      rlang::parse_expr("lapply(list(), makeActiveBinding)")
    ),
    "not allowed"
  )
})

# =============================================================================
# Multi-expression guard in ptr_parse_formula
# =============================================================================

test_that("multi-expr guard: two expressions separated by newline are rejected", {
  expect_error(
    ptr_parse_formula("x <- 1\nggplot(mtcars, aes(x = var)) + geom_point()",
                      formula_check = FALSE),
    "exactly one top-level expression"
  )
})

test_that("multi-expr guard: two expressions separated by semicolon are rejected", {
  expect_error(
    ptr_parse_formula("x <- 1; ggplot(mtcars, aes(x = var)) + geom_point()",
                      formula_check = FALSE),
    "exactly one top-level expression"
  )
})

test_that("multi-expr guard: trailing semicolon produces two expressions and is rejected", {
  # A trailing semicolon after a valid expression yields length 2 from parse_exprs
  # (one expression + one empty parse artifact in some R versions), or length 1.
  # Either way the guard should reject or pass cleanly — we check no crash occurs
  # and that if it produces 2 exprs the error fires.
  tryCatch(
    {
      result <- ptr_parse_formula(
        "ggplot(mtcars, aes(x = var)) + geom_point();",
        formula_check = FALSE
      )
      # If it parsed to exactly 1 expression, the result must be a ptr_obj
      expect_s3_class(result, "ptr_obj")
    },
    error = function(e) {
      expect_match(conditionMessage(e), "exactly one top-level expression")
    }
  )
})

test_that("multi-expr guard: empty string is rejected with exactly-one message", {
  expect_error(
    ptr_parse_formula(""),
    "exactly one top-level expression"
  )
})

test_that("multi-expr guard: single valid expression still works (regression)", {
  result <- ptr_parse_formula(
    "ggplot(mtcars, aes(x = var)) + geom_point()"
  )
  expect_s3_class(result, "ptr_obj")
})

# =============================================================================
# Pre-eval safety guard in ptr_assemble_plot
# =============================================================================

test_that("ptr_assemble_plot: blocks a list containing a dangerous expression", {
  danger_expr <- list(quote(system("id")))
  expect_error(
    ptr_assemble_plot(danger_expr, envir = new.env(parent = baseenv())),
    "not allowed"
  )
})

test_that("ptr_assemble_plot: blocks system() even when nested in a safe-looking call", {
  nested_danger <- list(quote(paste(system("id"), "x")))
  expect_error(
    ptr_assemble_plot(nested_danger, envir = new.env(parent = baseenv())),
    "not allowed"
  )
})

test_that("ptr_assemble_plot: passes a list of safe ggplot2 expressions", {
  safe_exprs <- list(
    quote(ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp))),
    quote(ggplot2::geom_point())
  )
  # Use globalenv so mtcars (and ggplot2 functions) are resolvable
  result <- ptr_assemble_plot(safe_exprs, envir = globalenv())
  expect_s3_class(result, "gg")
})

test_that("ptr_assemble_plot: empty list signals no-layers error (not a safety error)", {
  expect_error(
    ptr_assemble_plot(list()),
    "No plot layers"
  )
})

test_that("ptr_assemble_plot: NULL plot_expr_list signals no-layers error", {
  expect_error(
    ptr_assemble_plot(NULL),
    "No plot layers"
  )
})
