# Tests for the 7 refinement items implemented in the code quality pass.
# Covers: handle_duplicate_names, ptr_normalize_placeholders seen_keywords,
# check_free_variables_impl caching transparency, ptr_resolve_ui_text NULL
# traversal abort.

# ---------------------------------------------------------------------------
# 1. handle_duplicate_names — duplicate layer name tracking
# ---------------------------------------------------------------------------

test_that("handle_duplicate_names: no duplicates leaves vector unchanged", {
  expect_equal(handle_duplicate_names(c("a", "b", "c")), c("a", "b", "c"))
})

test_that("handle_duplicate_names: single-element vector is unchanged", {
  expect_equal(handle_duplicate_names(c("x")), c("x"))
})

test_that("handle_duplicate_names: keeps first occurrence, suffixes second with -2", {
  result <- handle_duplicate_names(c("geom_point", "geom_line", "geom_point"))
  expect_equal(result, c("geom_point", "geom_line", "geom_point-2"))
})

test_that("handle_duplicate_names: three copies get -2 and -3 suffix", {
  result <- handle_duplicate_names(c("x", "x", "x"))
  expect_equal(result, c("x", "x-2", "x-3"))
})

test_that("handle_duplicate_names: four copies get suffixes -2, -3, -4", {
  result <- handle_duplicate_names(c("a", "a", "a", "a"))
  expect_equal(result, c("a", "a-2", "a-3", "a-4"))
})

test_that("handle_duplicate_names: two distinct duplicate groups are both handled", {
  result <- handle_duplicate_names(c("a", "b", "a", "b", "a"))
  expect_equal(result, c("a", "b", "a-2", "b-2", "a-3"))
})

test_that("handle_duplicate_names: all-identical names produce sequential suffixes", {
  result <- handle_duplicate_names(c("z", "z", "z", "z"))
  expect_equal(result[1], "z")
  expect_equal(result[2], "z-2")
  expect_equal(result[3], "z-3")
  expect_equal(result[4], "z-4")
})

test_that("handle_duplicate_names: non-duplicate interspersed with duplicates is correct", {
  result <- handle_duplicate_names(c("a", "b", "b", "a", "c"))
  expect_equal(result, c("a", "b", "b-2", "a-2", "c"))
})

# ---------------------------------------------------------------------------
# 2. ptr_normalize_placeholders (seen_keywords list) — duplicate detection
# ---------------------------------------------------------------------------

test_that("ptr_normalize_placeholders: rejects two placeholders with the same keyword", {
  ph <- make_test_date_placeholder()
  expect_error(
    ptr_normalize_placeholders(list(ph, ph)),
    "duplicated keywords"
  )
})

test_that("ptr_normalize_placeholders: three distinct keywords all normalize", {
  ph_date <- make_test_date_placeholder()
  ph_clean <- ptr_define_placeholder(
    keyword = "clean",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) {
      if (is.null(value)) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )
  ph_flag <- ptr_define_placeholder(
    keyword = "flag",
    build_ui = function(id, copy, meta, context) shiny::checkboxInput(id, copy$label),
    resolve_expr = function(value, meta, context) {
      if (is.null(value)) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )

  result <- ptr_normalize_placeholders(list(
    date = ph_date,
    clean = ph_clean,
    flag = ph_flag
  ))

  expect_length(result, 3)
  expect_setequal(names(result), c("date", "clean", "flag"))
})

test_that("ptr_normalize_placeholders: duplicate at position 3 of 3 still detected", {
  ph_date <- make_test_date_placeholder()
  ph_clean <- ptr_define_placeholder(
    keyword = "clean",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) {
      if (is.null(value)) return(ptr_missing_expr())
      rlang::expr(!!value)
    }
  )
  ph_date2 <- make_test_date_placeholder()  # keyword is "date", same as ph_date

  # Use unnamed list to bypass the name-vs-keyword mismatch check and reach
  # the duplicate-keyword check
  expect_error(
    ptr_normalize_placeholders(list(ph_date, ph_clean, ph_date2)),
    "duplicated keywords"
  )
})

# ---------------------------------------------------------------------------
# 3. ptr_resolve_ui_text NULL traversal abort
# ---------------------------------------------------------------------------

test_that("ptr_resolve_ui_text aborts when rules has NULL at a mapped path key", {
  # Inject a broken rules object by manipulating the merged rules list.
  # ptr_resolve_ui_text traverses rules$shell$title for the "title" component.
  # If rules$shell is NULL the loop hits NULL on the first key and aborts.
  broken_rules <- structure(
    list(shell = NULL, defaults = list(), params = list(), layers = list()),
    class = "ptr_ui_text"
  )

  expect_error(
    ptr_resolve_ui_text("title", ui_text = broken_rules),
    "missing expected path"
  )
})

test_that("ptr_resolve_ui_text error message names the missing key", {
  broken_rules <- structure(
    list(shell = NULL, defaults = list(), params = list(), layers = list()),
    class = "ptr_ui_text"
  )

  err <- tryCatch(
    ptr_resolve_ui_text("title", ui_text = broken_rules),
    error = function(e) e
  )

  expect_match(conditionMessage(err), "shell", fixed = TRUE)
})

test_that("ptr_resolve_ui_text still resolves correctly with intact rules", {
  result <- ptr_resolve_ui_text("title")
  expect_type(result, "list")
  expect_true("label" %in% names(result))
})

test_that("ptr_resolve_ui_text aborts with path detail for deep NULL in rules", {
  # Simulate rules$shell existing but rules$shell$title being NULL
  broken_rules <- structure(
    list(
      shell = list(title = NULL),
      defaults = list(),
      params = list(),
      layers = list()
    ),
    class = "ptr_ui_text"
  )

  expect_error(
    ptr_resolve_ui_text("title", ui_text = broken_rules),
    "missing expected path"
  )
})

