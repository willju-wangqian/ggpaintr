# Tests for the RStudio placeholder addin's pure logic (no RStudio needed):
# the text transform, the type classifier, and the type-aware ordering.

# ---- ptr_addin_build_replacement --------------------------------------------

test_that("non-empty selection is wrapped as keyword(selection)", {
  r <- ptr_addin_build_replacement("ppVar", "mpg")
  expect_equal(r$text, "ppVar(mpg)")
  # caret lands at the end of the inserted text
  expect_equal(r$caret, nchar("ppVar(mpg)"))
})

test_that("surrounding whitespace in the selection is trimmed", {
  expect_equal(ptr_addin_build_replacement("ppVar", "  mpg  ")$text, "ppVar(mpg)")
})

test_that("a multi-token selection is wrapped verbatim", {
  expect_equal(ptr_addin_build_replacement("ppExpr", "a + b")$text, "ppExpr(a + b)")
})

test_that("empty selection inserts keyword() with caret between the parens", {
  r <- ptr_addin_build_replacement("ppVar", "")
  expect_equal(r$text, "ppVar()")
  # caret sits just after the "(" : nchar("ppVar") + 1
  expect_equal(r$caret, 6L)
})

test_that("whitespace-only selection is treated as empty", {
  expect_equal(ptr_addin_build_replacement("ppNum", "   ")$text, "ppNum()")
})

test_that("NULL selection is treated as empty", {
  expect_equal(ptr_addin_build_replacement("ppText", NULL)$text, "ppText()")
})

# ---- ptr_addin_build_app_wrap -----------------------------------------------

test_that("a single-line selection is wrapped in { } |> ptr_app()", {
  r <- ptr_addin_build_app_wrap("ggplot(mtcars)")
  expect_equal(r$text, "{\n  ggplot(mtcars)\n} |> \n  ptr_app()")
})

test_that("each line of a multi-line selection is indented two spaces", {
  r <- ptr_addin_build_app_wrap("ggplot(mtcars) +\ngeom_point()")
  expect_equal(r$text, "{\n  ggplot(mtcars) +\n  geom_point()\n} |> \n  ptr_app()")
})

test_that("a trailing newline in the selection is dropped (no extra blank line)", {
  expect_equal(ptr_addin_build_app_wrap("x\n")$text, "{\n  x\n} |> \n  ptr_app()")
})

test_that("an empty selection yields the skeleton with caret on the blank line", {
  r <- ptr_addin_build_app_wrap("")
  expect_equal(r$text, "{\n  \n} |> \n  ptr_app()")
  # caret one row below the start, at column 3 (just past the two-space indent)
  expect_equal(r$caret_row, 1L)
  expect_equal(r$caret_col, 3L)
})

test_that("the caret for a non-empty wrap lands at the end of ptr_app()", {
  r <- ptr_addin_build_app_wrap("ggplot(mtcars)")
  # text has 4 lines; caret on the last (row offset 3), past "  ptr_app()"
  expect_equal(r$caret_row, 3L)
  expect_equal(r$caret_col, nchar("  ptr_app()") + 1L)
})

# ---- ptr_addin_classify -----------------------------------------------------

test_that("classify recognises string, number and symbol", {
  # parent = emptyenv() so symbol resolution can't reach the search path
  e <- new.env(parent = emptyenv())
  expect_equal(ptr_addin_classify('"hello"', env = e), "string")
  expect_equal(ptr_addin_classify("42", env = e), "number")
  expect_equal(ptr_addin_classify("3.5", env = e), "number")
  expect_equal(ptr_addin_classify("mpg", env = e), "symbol")
})

test_that("classify treats a unary-signed number as a number, not a call", {
  e <- new.env()
  expect_equal(ptr_addin_classify("-3", env = e), "number")
  expect_equal(ptr_addin_classify("+7", env = e), "number")
})

test_that("classify recognises a call", {
  e <- new.env()
  expect_equal(ptr_addin_classify("geom_point()", env = e), "call")
  expect_equal(ptr_addin_classify("mean(x)", env = e), "call")
})

test_that("a bare name resolving to a data frame classifies as dataframe", {
  e <- new.env(parent = emptyenv())
  assign("my_df", data.frame(a = 1), envir = e)
  assign("my_vec", 1:3, envir = e)
  expect_equal(ptr_addin_classify("my_df", env = e), "dataframe")
  # a non-data-frame binding stays a plain symbol
  expect_equal(ptr_addin_classify("my_vec", env = e), "symbol")
  # an unbound name is a plain symbol, not a data frame
  expect_equal(ptr_addin_classify("nope_not_bound", env = e), "symbol")
})

test_that("a search-path dataset is detected as a data frame", {
  # mtcars lives in the always-attached datasets package; inherits = TRUE finds
  # it, so it should classify as a data frame, not a plain symbol.
  expect_equal(ptr_addin_classify("mtcars", env = globalenv()), "dataframe")
})

test_that("empty and unparseable selections classify as empty/other", {
  e <- new.env()
  expect_equal(ptr_addin_classify("", env = e), "empty")
  expect_equal(ptr_addin_classify("   ", env = e), "empty")
  expect_equal(ptr_addin_classify("a = ", env = e), "other")
})

# ---- ptr_addin_pickable -----------------------------------------------------

test_that("selectize items carry kw/role/desc and preserve order", {
  items <- ptr_addin_items(ptr_addin_order(ptr_addin_pickable(), "symbol"))
  expect_true(all(vapply(items, function(i) all(c("kw", "role", "desc") %in% names(i)),
                         logical(1))))
  # ordering is inherited from ptr_addin_order: a symbol leads with ppVar
  expect_equal(items[[1]]$kw, "ppVar")
})

test_that("pickable list includes the structural keywords", {
  kws <- names(ptr_addin_pickable())
  expect_true(all(c("ppVar", "ppNum", "ppText", "ppExpr", "ppUpload",
                    "ppLayerOff", "ppVerbSwitch") %in% kws))
})

test_that("a custom-registered placeholder appears in the pickable list", {
  withr::defer(ptr_clear_placeholder("ppFoo"))
  ptr_define_placeholder_value(
    "ppFoo",
    build_ui = function(node, label = NULL, selected = NULL, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  expect_true("ppFoo" %in% names(ptr_addin_pickable()))
})

# ---- ptr_addin_order (the type-aware ranking) -------------------------------

top_kw <- function(kind) ptr_addin_order(ptr_addin_pickable(), kind)[[1]]$keyword
kw_seq <- function(kind) unname(vapply(ptr_addin_order(ptr_addin_pickable(), kind),
                                       function(p) p$keyword, character(1)))

test_that("a string promotes ppText to the top", {
  expect_equal(top_kw("string"), "ppText")
})

test_that("a number promotes ppNum to the top", {
  expect_equal(top_kw("number"), "ppNum")
})

test_that("a data frame promotes the data source (ppUpload) to the top", {
  expect_equal(top_kw("dataframe"), "ppUpload")
})

test_that("a call promotes the structural keywords to the top", {
  expect_equal(kw_seq("call")[1:2], c("ppLayerOff", "ppVerbSwitch"))
})

test_that("a symbol promotes consumers first, then ppExpr, above the rest", {
  seq <- kw_seq("symbol")
  # ppVar (a consumer) leads; ppExpr sits above the non-promoted value/source kws
  expect_equal(seq[[1]], "ppVar")
  expect_lt(match("ppExpr", seq), match("ppNum", seq))
  expect_lt(match("ppExpr", seq), match("ppText", seq))
  expect_lt(match("ppExpr", seq), match("ppUpload", seq))
})

test_that("a custom consumer is promoted alongside ppVar for a symbol, above ppExpr", {
  withr::defer(ptr_clear_placeholder("ppMyCol"))
  ptr_define_placeholder_consumer(
    "ppMyCol",
    build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) NULL,
    resolve_expr = function(value, node, ...) rlang::sym(value)
  )
  seq <- kw_seq("symbol")
  expect_lt(match("ppMyCol", seq), match("ppExpr", seq))
})

test_that("empty/other selection keeps the canonical order (ppVar first)", {
  expect_equal(top_kw("empty"), "ppVar")
  expect_equal(top_kw("other"), "ppVar")
})

test_that("a custom value placeholder joins the value group, not the bottom", {
  withr::defer(ptr_clear_placeholder("ppZeta"))
  ptr_define_placeholder_value(
    "ppZeta",
    build_ui = function(node, label = NULL, selected = NULL, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  seq <- kw_seq("empty")
  # grouped with the value placeholders -> ahead of the source + structural ones
  expect_lt(match("ppZeta", seq), match("ppUpload", seq))
  expect_lt(match("ppZeta", seq), match("ppLayerOff", seq))
})

test_that("a custom consumer groups with consumers, above the built-in values", {
  withr::defer(ptr_clear_placeholder("ppZone"))
  ptr_define_placeholder_consumer(
    "ppZone",
    build_ui = function(node, cols, data, label = NULL, selected = NULL, ...) NULL,
    resolve_expr = function(value, node, ...) rlang::sym(value)
  )
  seq <- kw_seq("empty")
  # a consumer sits ahead of the value placeholders even with no promotion
  expect_lt(match("ppZone", seq), match("ppNum", seq))
})
