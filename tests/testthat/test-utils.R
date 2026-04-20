# Tests for internal utility functions in R/paintr-utils.R.
# Internal functions are sourced via helper-fixtures.R (no ::: needed).

# --- expr_type -----------------------------------------------------------

test_that("expr_type classifies syntactic literals as constant", {
  expect_equal(expr_type(1L),       "constant")
  expect_equal(expr_type(1.5),      "constant")
  expect_equal(expr_type(TRUE),     "constant")
  expect_equal(expr_type(FALSE),    "constant")
  expect_equal(expr_type("hello"),  "constant")
  expect_equal(expr_type(NULL),     "constant")  # NULL is a syntactic literal
})

test_that("expr_type classifies symbols", {
  expect_equal(expr_type(rlang::sym("x")),   "symbol")
  expect_equal(expr_type(rlang::sym("var")), "symbol")
})

test_that("expr_type classifies calls", {
  expect_equal(expr_type(quote(f(x))),       "call")
  expect_equal(expr_type(quote(a + b)),      "call")
  expect_equal(expr_type(quote(ggplot())),   "call")
})

test_that("expr_type classifies pairlists", {
  pl <- as.pairlist(list(a = 1, b = 2))
  expect_equal(expr_type(pl), "pairlist")
})

test_that("expr_type falls through to typeof for raw types", {
  # A plain list is not a literal/symbol/call/pairlist
  expect_equal(expr_type(list(1, 2)), typeof(list(1, 2)))
})

# --- handle_call_break_sum -----------------------------------------------

test_that("handle_call_break_sum splits a + call into two parts", {
  expr <- quote(a + b)
  result <- handle_call_break_sum(expr)
  expect_type(result, "list")
  expect_length(result, 2)
})

test_that("handle_call_break_sum returns the call unchanged when not +", {
  expr <- quote(geom_point())
  result <- handle_call_break_sum(expr)
  # not a + call: returns the call itself, not a list
  expect_true(is.call(result))
  expect_equal(rlang::as_string(result[[1]]), "geom_point")
})

# --- break_sum -----------------------------------------------------------

test_that("break_sum passes through symbols and constants unchanged", {
  expect_equal(break_sum(rlang::sym("x")), rlang::sym("x"))
  expect_equal(break_sum(42L), 42L)
  expect_equal(break_sum("hello"), "hello")
})

test_that("break_sum recursively splits nested + expressions", {
  # a + b + c parses as (a + b) + c
  expr <- quote(a + b + c)
  result <- break_sum(expr)
  # top level is a list (from the outer +)
  expect_type(result, "list")
})

test_that("break_sum handles a non-+ call without splitting", {
  expr <- quote(geom_point(size = 2))
  result <- break_sum(expr)
  expect_true(is.call(result))
})

test_that("break_sum splits a simple ggplot + layer expression", {
  expr <- quote(ggplot(data = mtcars) + geom_point())
  result <- break_sum(expr)
  expect_type(result, "list")
  expect_length(result, 2)
  # each element should be a call
  expect_true(is.call(result[[1]]))
  expect_true(is.call(result[[2]]))
})

# --- get_fun_names -------------------------------------------------------

test_that("get_fun_names returns the function name for a call", {
  expect_equal(get_fun_names(quote(ggplot())),    "ggplot")
  expect_equal(get_fun_names(quote(geom_point())), "geom_point")
  expect_equal(get_fun_names(quote(a + b)),        "+")
})

test_that("get_fun_names returns the symbol name for a symbol", {
  expect_equal(get_fun_names(rlang::sym("mpg")), "mpg")
})

test_that("get_fun_names returns deparsed string for constants and other types", {
  expect_equal(get_fun_names(42L),   "42L")
  expect_equal(get_fun_names("text"), "\"text\"")
  expect_equal(get_fun_names(TRUE),  "TRUE")
})

# --- expr_pluck ----------------------------------------------------------

test_that("expr_pluck retrieves element by index path", {
  expr <- quote(f(a, b, c))
  # index 1 is the function name, index 2 is first arg
  expect_equal(expr_pluck(expr, 2), rlang::sym("a"))
  expect_equal(expr_pluck(expr, 3), rlang::sym("b"))
})

test_that("expr_pluck returns NULL on out-of-bounds index", {
  expr <- quote(f(a))
  expect_null(expr_pluck(expr, 99))
})

test_that("expr_pluck returns NULL on invalid multi-index path", {
  expr <- quote(f(a, b))
  # index path [2, 5] — second arg has no children
  expect_null(expr_pluck(expr, c(2, 5)))
})

# --- expr_pluck<- --------------------------------------------------------

test_that("expr_pluck<- replaces an element at the given index path", {
  expr <- quote(f(a, b))
  expr_pluck(expr, 2) <- rlang::sym("z")
  expect_equal(expr[[2]], rlang::sym("z"))
})

test_that("expr_pluck<- errors on invalid index path", {
  e <- quote(f(a))
  expect_error(
    expr_pluck(e, c(99, 1)) <- rlang::sym("z"),
    "Failed to substitute expression at index path"
  )
})

# --- get_index_path ------------------------------------------------------

test_that("get_index_path finds a var placeholder in a simple call", {
  expr <- quote(aes(x = var, y = var))
  paths <- get_index_path(expr)
  expect_type(paths, "list")
  expect_length(paths, 2)
})

test_that("get_index_path finds multiple placeholder types", {
  expr <- quote(f(a = var, b = text, c = num, d = expr, e = upload))
  paths <- get_index_path(expr)
  expect_length(paths, 5)
})

test_that("get_index_path does not capture 'data' arg unless value is a placeholder keyword", {
  expr <- quote(geom_point(data = mydf, aes(x = a)))
  paths <- get_index_path(expr)
  # 'mydf' is not a placeholder keyword, so 'data = mydf' should NOT be captured
  data_path_found <- any(vapply(paths, function(p) {
    length(p) == 1 && p == 2
  }, logical(1)))
  expect_false(data_path_found)
})

test_that("get_index_path returns empty list when no placeholders", {
  expr <- quote(f(a = 1, b = "hello"))
  paths <- get_index_path(expr)
  expect_length(paths, 0)
})

test_that("get_index_path recurses into nested calls", {
  expr <- quote(ggplot(data = mtcars, aes(x = var, y = var)))
  paths <- get_index_path(expr)
  # 'data = mtcars' is NOT captured (mtcars is not a placeholder keyword).
  # Two var paths nested inside aes() are captured.  At least 2 paths total.
  expect_true(length(paths) >= 2)
  # At least two paths have depth > 1 (the nested var placeholders)
  deep_paths <- Filter(function(p) length(p) >= 2, paths)
  expect_true(length(deep_paths) >= 2)
})

# --- handle_duplicate_names ----------------------------------------------

test_that("handle_duplicate_names leaves unique names unchanged", {
  expect_equal(handle_duplicate_names(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("handle_duplicate_names keeps first occurrence, suffixes from 2nd", {
  result <- handle_duplicate_names(c("a", "b", "a"))
  expect_equal(result, c("a", "b", "a-2"))
})

test_that("handle_duplicate_names handles more than two duplicates", {
  result <- handle_duplicate_names(c("x", "x", "x"))
  expect_equal(result, c("x", "x-2", "x-3"))
})

test_that("handle_duplicate_names handles multiple different duplicated groups", {
  result <- handle_duplicate_names(c("a", "b", "a", "b"))
  expect_equal(result, c("a", "b", "a-2", "b-2"))
})

test_that("handle_duplicate_names handles a single-element vector", {
  expect_equal(handle_duplicate_names(c("a")), c("a"))
})

# --- encode_id -----------------------------------------------------------

test_that("encode_id joins func_name and index_path with +", {
  expect_equal(encode_id(c(2, 3), "ggplot"),    "ggplot_2_3")
  expect_equal(encode_id(c(2),    "geom_point"), "geom_point_2")
})

test_that("encode_id works with a length-3 path", {
  expect_equal(encode_id(c(3, 2, 1), "labs"), "labs_3_2_1")
})

test_that("encode_id works with an empty index path", {
  expect_equal(encode_id(numeric(0), "f"), "f")
})

# --- get_expr_param ------------------------------------------------------

test_that("get_expr_param returns the named argument at a length-1 path", {
  expr <- quote(f(alpha = 0.5, size = 2))
  # path = 2 → first argument → name "alpha"
  expect_equal(get_expr_param(expr, 2), "alpha")
  expect_equal(get_expr_param(expr, 3), "size")
})

test_that("get_expr_param returns NULL for unnamed arguments", {
  expr <- quote(f(x, y))
  result <- get_expr_param(expr, 2)
  # names(expr) is NULL for an unnamed call
  expect_null(result)
})

test_that("get_expr_param recurses for a length-2+ path", {
  # aes(x = var, y = var) — path c(2, 2) means: arg 2 of outer, then arg 2 of inner
  outer <- quote(ggplot(data = mtcars, aes(x = var, y = var)))
  # path c(3, 2): outer arg 3 = aes(...), inner arg 2 = x = var
  result <- get_expr_param(outer, c(3, 2))
  expect_equal(result, "x")
})

# --- expr_remove_null ----------------------------------------------------

test_that("expr_remove_null removes _NULL_PLACEHOLDER symbols from a call", {
  null_sym <- rlang::sym("_NULL_PLACEHOLDER")
  expr <- rlang::call2("f", rlang::sym("a"), null_sym, rlang::sym("b"))
  result <- expr_remove_null(expr)
  # The placeholder should be gone; remaining args are a and b
  expect_false(any(vapply(as.list(result[-1]), function(e) {
    is.symbol(e) && identical(e, null_sym)
  }, logical(1))))
})

test_that("expr_remove_null leaves expressions without the placeholder unchanged", {
  expr <- quote(f(a, b, c))
  result <- expr_remove_null(expr)
  expect_equal(result, expr)
})

test_that("expr_remove_null handles nested calls", {
  null_sym <- rlang::sym("_NULL_PLACEHOLDER")
  inner <- rlang::call2("g", null_sym, rlang::sym("x"))
  expr  <- rlang::call2("f", inner)
  result <- expr_remove_null(expr)
  inner_result <- result[[2]]
  expect_false(any(vapply(as.list(inner_result[-1]), function(e) {
    is.symbol(e) && identical(e, null_sym)
  }, logical(1))))
})

test_that("expr_remove_null accepts a custom target symbol", {
  custom_sym <- rlang::sym("REMOVE_ME")
  expr <- rlang::call2("f", custom_sym, rlang::sym("keep"))
  result <- expr_remove_null(expr, target = custom_sym)
  remaining <- as.list(result)[-1]
  expect_false(any(vapply(remaining, function(e) {
    is.symbol(e) && identical(e, custom_sym)
  }, logical(1))))
})

# --- expr_remove_emptycall2 ----------------------------------------------

test_that("expr_remove_emptycall2 removes an empty non-ggplot call", {
  expr <- quote(f(unknown_func()))
  result <- expr_remove_emptycall2(expr)
  # After removal f() has no args; the outer length-1 check fires too.
  expect_true(is.null(result) || (is.call(result) && length(result) < length(expr)))
})

test_that("expr_remove_emptycall2 preserves a gg-layer call by name", {
  expr <- quote(wrapper(geom_point()))
  result <- expr_remove_emptycall2(expr)
  # geom_point() is recognized as gg by name — should be kept
  args <- as.list(result)[-1]
  expect_true(length(args) >= 1)
  expect_equal(rlang::as_string(args[[1]][[1]]), "geom_point")
})

test_that("expr_remove_emptycall2 preserves other gg-layer types by name", {
  # scale_, coord_, facet_, theme_ should all be preserved
  expr <- quote(wrapper(scale_x_continuous(), coord_flip(), facet_wrap(), theme_minimal()))
  result <- expr_remove_emptycall2(expr)
  args <- as.list(result)[-1]
  expect_length(args, 4)
})

test_that("ptr_is_gg_layer_name recognizes known patterns", {
  expect_true(ptr_is_gg_layer_name("geom_point"))
  expect_true(ptr_is_gg_layer_name("stat_smooth"))
  expect_true(ptr_is_gg_layer_name("scale_x_continuous"))
  expect_true(ptr_is_gg_layer_name("theme_minimal"))
  expect_true(ptr_is_gg_layer_name("theme"))
  expect_true(ptr_is_gg_layer_name("labs"))
  expect_true(ptr_is_gg_layer_name("xlim"))
  expect_false(ptr_is_gg_layer_name("unknown_func"))
  expect_false(ptr_is_gg_layer_name("f"))
})

# W2: edge cases for startsWith-based implementation
test_that("ptr_is_gg_layer_name returns FALSE for empty string", {
  expect_false(ptr_is_gg_layer_name(""))
})

test_that("ptr_is_gg_layer_name returns FALSE for prefix without trailing underscore", {
  # "geom" alone is not a valid gg layer name; only "geom_*" is
  expect_false(ptr_is_gg_layer_name("geom"))
  expect_false(ptr_is_gg_layer_name("stat"))
  expect_false(ptr_is_gg_layer_name("scale"))
  expect_false(ptr_is_gg_layer_name("facet"))
  expect_false(ptr_is_gg_layer_name("coord"))
})

test_that("ptr_can_stand_alone returns FALSE for empty string", {
  expect_false(ptr_can_stand_alone(""))
})

test_that("ptr_can_stand_alone returns FALSE for prefix without trailing underscore", {
  expect_false(ptr_can_stand_alone("geom"))
  expect_false(ptr_can_stand_alone("stat"))
})

test_that("ptr_can_stand_alone returns FALSE for layers that share prefix but are not geom_/stat_", {
  # geomancy, statistic — not gg layer names
  expect_false(ptr_can_stand_alone("geomancy"))
  expect_false(ptr_can_stand_alone("statistics"))
})

# W3: expr_remove_emptycall2 must not emit messages
test_that("expr_remove_emptycall2 emits no messages when removing empty non-gg call", {
  expr <- quote(f(unknown_func()))
  expect_no_message(expr_remove_emptycall2(expr))
})

test_that("expr_remove_emptycall2 emits no messages when preserving gg-layer call", {
  expr <- quote(wrapper(geom_point()))
  expect_no_message(expr_remove_emptycall2(expr))
})

# --- check_remove_null ---------------------------------------------------

test_that("check_remove_null returns NULL for NULL input", {
  expect_null(check_remove_null(NULL))
})

test_that("check_remove_null drops NULL elements from a list", {
  x <- list(a = 1, b = NULL, c = 3)
  result <- check_remove_null(x)
  expect_equal(result, list(a = 1, c = 3))
})

test_that("check_remove_null returns NULL when all elements are NULL", {
  x <- list(NULL, NULL)
  expect_null(check_remove_null(x))
})

test_that("check_remove_null leaves a list with no NULLs unchanged", {
  x <- list(a = 1, b = 2, c = 3)
  expect_equal(check_remove_null(x), x)
})

# --- ptr_can_stand_alone --------------------------------------------------

test_that("ptr_can_stand_alone recognizes geom_ and stat_ prefixes", {
  expect_true(ptr_can_stand_alone("geom_point"))
  expect_true(ptr_can_stand_alone("geom_line"))
  expect_true(ptr_can_stand_alone("stat_smooth"))
})

test_that("ptr_can_stand_alone rejects non-standalone layers", {
  expect_false(ptr_can_stand_alone("labs"))
  expect_false(ptr_can_stand_alone("facet_wrap"))
  expect_false(ptr_can_stand_alone("theme"))
  expect_false(ptr_can_stand_alone("theme_minimal"))
  expect_false(ptr_can_stand_alone("scale_x_continuous"))
})

# --- ptr_remove_empty_nonstandalone_layers --------------------------------

test_that("ptr_remove_empty_nonstandalone_layers removes empty labs/facet/theme", {
  expr_list <- list(
    ggplot = quote(ggplot(data = iris, aes(x = Sepal.Length))),
    `geom_point_2` = quote(geom_point()),
    `labs_3` = quote(labs()),
    `facet_wrap_4` = quote(facet_wrap()),
    `theme_5` = quote(theme())
  )
  result <- suppressMessages(ptr_remove_empty_nonstandalone_layers(expr_list))
  expect_true("ggplot" %in% names(result))
  expect_true("geom_point_2" %in% names(result))
  expect_null(result[["labs_3"]])
  expect_null(result[["facet_wrap_4"]])
  expect_null(result[["theme_5"]])
})

test_that("ptr_remove_empty_nonstandalone_layers keeps layers with arguments", {
  expr_list <- list(
    ggplot = quote(ggplot(data = iris)),
    `labs_2` = quote(labs(title = "hello")),
    `theme_3` = quote(theme(legend.position = "top"))
  )
  result <- ptr_remove_empty_nonstandalone_layers(expr_list)
  expect_length(result, 3)
})

test_that("ptr_remove_empty_nonstandalone_layers keeps empty geom layers", {
  expr_list <- list(
    ggplot = quote(ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))),
    `geom_point_2` = quote(geom_point()),
    `stat_smooth_3` = quote(stat_smooth())
  )
  result <- ptr_remove_empty_nonstandalone_layers(expr_list)
  expect_length(result, 3)
})

test_that("check_remove_null handles a single non-NULL element", {
  x <- list(a = 42)
  expect_equal(check_remove_null(x), list(a = 42))
})

# --- depth guards --------------------------------------------------------

test_that("break_sum aborts when max_depth is exceeded", {
  # Build a deeply nested + chain: ((((a + b) + c) + d) + e) ...
  deep_expr <- rlang::sym("a")
  for (i in seq_len(10)) {
    deep_expr <- rlang::call2("+", deep_expr, rlang::sym(paste0("x", i)))
  }
  expect_error(
    break_sum(deep_expr, max_depth = 2L),
    "maximum depth"
  )
})

test_that("break_sum works normally within depth limit", {
  expr <- quote(a + b + c)
  result <- break_sum(expr)
  expect_type(result, "list")
})

test_that("get_index_path aborts when max_depth is exceeded", {
  # Build a call nested 10 levels deep: f(f(f(...)))
  deep_expr <- rlang::call2("f", rlang::sym("var"))
  for (i in seq_len(10)) {
    deep_expr <- rlang::call2("f", deep_expr)
  }
  expect_error(
    get_index_path(deep_expr, max_depth = 2L),
    "maximum depth"
  )
})

test_that("get_index_path works normally within depth limit", {
  expr <- quote(aes(x = var, y = var))
  result <- get_index_path(expr)
  expect_length(result, 2)
})

test_that("expr_remove_null aborts when max_depth is exceeded", {
  null_sym <- rlang::sym("_NULL_PLACEHOLDER")
  # Build a deeply nested call structure
  deep_expr <- rlang::call2("f", null_sym)
  for (i in seq_len(10)) {
    deep_expr <- rlang::call2("g", deep_expr)
  }
  expect_error(
    expr_remove_null(deep_expr, max_depth = 2L),
    "maximum depth"
  )
})

test_that("expr_remove_null works normally within depth limit", {
  null_sym <- rlang::sym("_NULL_PLACEHOLDER")
  expr <- rlang::call2("f", rlang::sym("a"), null_sym)
  result <- expr_remove_null(expr)
  remaining <- as.list(result)[-1]
  expect_false(any(vapply(remaining, function(e) {
    is.symbol(e) && identical(e, null_sym)
  }, logical(1))))
})

test_that("expr_remove_emptycall2 aborts when max_depth is exceeded", {
  # Build a deeply nested non-gg call
  deep_expr <- rlang::call2("unknown_func", rlang::sym("a"))
  for (i in seq_len(10)) {
    deep_expr <- rlang::call2("wrapper", deep_expr)
  }
  expect_error(
    expr_remove_emptycall2(deep_expr, max_depth = 2L),
    "maximum depth"
  )
})

test_that("expr_remove_emptycall2 works normally within depth limit", {
  expr <- quote(wrapper(geom_point()))
  result <- expr_remove_emptycall2(expr)
  args <- as.list(result)[-1]
  expect_true(length(args) >= 1)
})

# --- validate_expr_safety (denylist default) ------------------------------

test_that("denylist allows safe math expressions", {
  expect_invisible(validate_expr_safety(quote(sqrt(x))))
  expect_invisible(validate_expr_safety(quote(x + y * 2)))
  expect_invisible(validate_expr_safety(quote(log10(abs(x)))))
  expect_invisible(validate_expr_safety(quote(round(mean(x), 2))))
})

test_that("denylist allows string helpers", {
  expect_invisible(validate_expr_safety(quote(paste0("a", "b"))))
  expect_invisible(validate_expr_safety(quote(toupper(x))))
})

test_that("denylist allows logic and type coercion", {
  expect_invisible(validate_expr_safety(quote(ifelse(is.na(x), 0, x))))
  expect_invisible(validate_expr_safety(quote(factor(x))))
  expect_invisible(validate_expr_safety(quote(as.numeric(x))))
})

test_that("denylist allows ggplot2 aes helpers", {
  expect_invisible(validate_expr_safety(quote(aes(x = v, y = v))))
  expect_invisible(validate_expr_safety(quote(after_stat(count))))
})

test_that("denylist allows constructors and data ops", {
  expect_invisible(validate_expr_safety(quote(c(1, 2, 3))))
  expect_invisible(validate_expr_safety(quote(seq(1, 10))))
  expect_invisible(validate_expr_safety(quote(rev(sort(x)))))
})

test_that("denylist allows bare symbols and constants", {
  expect_invisible(validate_expr_safety(quote(x)))
  expect_invisible(validate_expr_safety(42))
  expect_invisible(validate_expr_safety("text"))
})

test_that("denylist allows anonymous functions", {
  expect_invisible(
    validate_expr_safety(quote(function(x) x + 1))
  )
})

test_that("denylist rejects system commands", {
  expect_error(
    validate_expr_safety(quote(system("rm -rf /"))),
    "not allowed"
  )
  expect_error(
    validate_expr_safety(quote(system2("ls"))),
    "not allowed"
  )
})

test_that("denylist rejects file operations", {
  expect_error(
    validate_expr_safety(quote(readLines("/etc/passwd"))),
    "not allowed"
  )
  expect_error(
    validate_expr_safety(quote(file.remove("x"))),
    "not allowed"
  )
})

test_that("denylist rejects meta-eval and side effects", {
  expect_error(
    validate_expr_safety(quote(eval(quote(1 + 1)))),
    "not allowed"
  )
  expect_error(
    validate_expr_safety(quote(library(ggplot2))),
    "not allowed"
  )
  expect_error(
    validate_expr_safety(quote(Sys.setenv(FOO = "bar"))),
    "not allowed"
  )
})

test_that("denylist rejects indirect dangerous calls", {
  expect_error(
    validate_expr_safety(quote(do.call(system, list("ls")))),
    "not allowed"
  )
})

test_that("denylist rejects namespaced dangerous calls", {
  expect_error(
    validate_expr_safety(quote(base::system("ls"))),
    "not allowed"
  )
})

# --- expr_check = FALSE ---------------------------------------------------

test_that("expr_check = FALSE skips all validation", {
  expect_invisible(
    validate_expr_safety(quote(system("ls")), expr_check = FALSE)
  )
  expect_invisible(
    validate_expr_safety(quote(eval(parse(text = "1"))),
                         expr_check = FALSE)
  )
})

# --- custom deny_list -----------------------------------------------------

test_that("custom deny_list blocks specified functions", {
  custom <- list(deny_list = c("log", "exp"))
  expect_error(
    validate_expr_safety(quote(log(x)), expr_check = custom),
    "not allowed"
  )
  expect_invisible(
    validate_expr_safety(quote(sqrt(x)), expr_check = custom)
  )
})

# --- custom allow_list (strict mode) --------------------------------------

test_that("custom allow_list blocks unlisted functions", {
  custom <- list(allow_list = c("sqrt", "+"))
  expect_invisible(
    validate_expr_safety(quote(sqrt(x)), expr_check = custom)
  )
  expect_error(
    validate_expr_safety(quote(log(x)), expr_check = custom),
    "not in the allowlist"
  )
})

# --- deny_list + allow_list conflict resolution ---------------------------

test_that("deny_list removes entries from allow_list", {
  custom <- list(
    allow_list = c("sqrt", "log", "+"),
    deny_list = c("log")
  )
  expect_invisible(
    validate_expr_safety(quote(sqrt(x)), expr_check = custom)
  )
  expect_error(
    validate_expr_safety(quote(log(x)), expr_check = custom),
    "not in the allowlist"
  )
})

# --- resolve_expr_check edge cases ----------------------------------------

test_that("empty list falls back to default denylist", {
  resolved <- resolve_expr_check(list())
  expect_equal(resolved$mode, "denylist")
  expect_equal(resolved$fns, unsafe_expr_denylist)
})

test_that("invalid expr_check type errors", {
  expect_error(
    resolve_expr_check("bad"),
    "expr_check must be TRUE, FALSE"
  )
})

# --- ptr_resolve_expr_expr with expr_check context ------------------------

test_that("ptr_resolve_expr_expr allows safe expr with default check", {
  ctx <- list(expr_check = TRUE)
  result <- ptr_resolve_expr_expr("sqrt(x)", list(), ctx)
  expect_equal(result, quote(sqrt(x)))
})

test_that("ptr_resolve_expr_expr blocks dangerous expr with default check", {
  ctx <- list(expr_check = TRUE)
  expect_error(
    ptr_resolve_expr_expr("system('ls')", list(), ctx),
    "not allowed"
  )
})

test_that("ptr_resolve_expr_expr allows dangerous expr when check = FALSE", {
  ctx <- list(expr_check = FALSE)
  result <- ptr_resolve_expr_expr("system('ls')", list(), ctx)
  expect_equal(result, quote(system("ls")))
})

test_that("ptr_resolve_expr_expr returns missing for empty input", {
  ctx_on <- list(expr_check = TRUE)
  ctx_off <- list(expr_check = FALSE)
  expect_equal(
    ptr_resolve_expr_expr("", list(), ctx_on),
    ptr_missing_expr()
  )
  expect_equal(
    ptr_resolve_expr_expr(NULL, list(), ctx_on),
    ptr_missing_expr()
  )
  expect_equal(
    ptr_resolve_expr_expr("", list(), ctx_off),
    ptr_missing_expr()
  )
})

test_that("ptr_resolve_expr_expr respects custom allow_list", {
  ctx <- list(expr_check = list(allow_list = c("sqrt", "+")))
  expect_equal(
    ptr_resolve_expr_expr("sqrt(x)", list(), ctx),
    quote(sqrt(x))
  )
  expect_error(
    ptr_resolve_expr_expr("log(x)", list(), ctx),
    "not in the allowlist"
  )
})

# --- expr_check on ptr_server_state ---------------------------------------

test_that("ptr_server_state expr_check defaults to TRUE", {
  state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  expect_true(state$expr_check)
})

test_that("ptr_server_state expr_check = FALSE is stored", {
  state <- ptr_server_state(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
    expr_check = FALSE
  )
  expect_false(state$expr_check)
})

# --- ptr_verbose ----------------------------------------------------------

test_that("ptr_verbose defaults to TRUE", {
  withr::local_options(list(ggpaintr.verbose = NULL))
  expect_true(ptr_verbose())
})

test_that("ptr_verbose respects option", {
  withr::local_options(list(ggpaintr.verbose = FALSE))
  expect_false(ptr_verbose())
})

test_that("ptr_verbose TRUE when option is TRUE", {
  withr::local_options(list(ggpaintr.verbose = TRUE))
  expect_true(ptr_verbose())
})
