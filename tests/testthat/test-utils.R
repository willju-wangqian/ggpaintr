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

test_that("get_fun_names returns NULL for constants and other types", {
  expect_null(get_fun_names(42L))
  expect_null(get_fun_names("text"))
  expect_null(get_fun_names(TRUE))
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

test_that("get_index_path finds a named 'data' argument as a path", {
  expr <- quote(geom_point(data = mydf, aes(x = a)))
  paths <- get_index_path(expr)
  # 'data' named arg should be captured
  data_path_found <- any(vapply(paths, function(p) {
    length(p) == 1 && p == 2
  }, logical(1)))
  expect_true(data_path_found)
})

test_that("get_index_path returns empty list when no placeholders", {
  expr <- quote(f(a = 1, b = "hello"))
  paths <- get_index_path(expr)
  expect_length(paths, 0)
})

test_that("get_index_path recurses into nested calls", {
  expr <- quote(ggplot(data = mtcars, aes(x = var, y = var)))
  paths <- get_index_path(expr)
  # 'data = mtcars' is captured as a data named-arg path, plus two var paths
  # nested inside aes().  At least 3 paths total.
  expect_true(length(paths) >= 3)
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
  expect_equal(encode_id(c(2, 3), "ggplot"),    "ggplot+2+3")
  expect_equal(encode_id(c(2),    "geom_point"), "geom_point+2")
})

test_that("encode_id works with a length-3 path", {
  expect_equal(encode_id(c(3, 2, 1), "labs"), "labs+3+2+1")
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

test_that("expr_remove_emptycall2 removes an empty non-ggplot call with a message", {
  expr <- quote(f(unknown_func()))
  expect_message(
    result <- expr_remove_emptycall2(expr),
    regexp = "unknown_func"
  )
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
    `geom_point+2` = quote(geom_point()),
    `labs+3` = quote(labs()),
    `facet_wrap+4` = quote(facet_wrap()),
    `theme+5` = quote(theme())
  )
  result <- suppressMessages(ptr_remove_empty_nonstandalone_layers(expr_list))
  expect_true("ggplot" %in% names(result))
  expect_true("geom_point+2" %in% names(result))
  expect_null(result[["labs+3"]])
  expect_null(result[["facet_wrap+4"]])
  expect_null(result[["theme+5"]])
})

test_that("ptr_remove_empty_nonstandalone_layers keeps layers with arguments", {
  expr_list <- list(
    ggplot = quote(ggplot(data = iris)),
    `labs+2` = quote(labs(title = "hello")),
    `theme+3` = quote(theme(legend.position = "top"))
  )
  result <- ptr_remove_empty_nonstandalone_layers(expr_list)
  expect_length(result, 3)
})

test_that("ptr_remove_empty_nonstandalone_layers keeps empty geom layers", {
  expr_list <- list(
    ggplot = quote(ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width))),
    `geom_point+2` = quote(geom_point()),
    `stat_smooth+3` = quote(stat_smooth())
  )
  result <- ptr_remove_empty_nonstandalone_layers(expr_list)
  expect_length(result, 3)
})

test_that("check_remove_null handles a single non-NULL element", {
  x <- list(a = 42)
  expect_equal(check_remove_null(x), list(a = 42))
})
