# Tests for the 7 refinement items implemented in the code quality pass.
# Covers: handle_duplicate_names, ptr_normalize_placeholders seen_keywords,
# check_free_variables_impl caching transparency, ptr_resolve_ui_text NULL
# traversal abort, and ptr_generate_shiny write error handling.

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
# 3. check_free_variables_impl caching transparency
# ---------------------------------------------------------------------------

test_that("check_free_variables_impl: no warning for body using only formals and base names", {
  fn_body <- quote({ x + y })
  expect_no_warning(
    check_free_variables_impl(fn_body, c("x", "y"), "mykw", "build_ui")
  )
})

test_that("check_free_variables_impl: warns when body references name not in formals", {
  fn_body <- quote({ external_thing + x })
  expect_warning(
    check_free_variables_impl(fn_body, c("x"), "mykw", "build_ui"),
    "external_thing"
  )
})

test_that("check_free_variables_impl: second call returns same result (caching transparent)", {
  fn_body <- quote({ x + outside_name })
  w1 <- tryCatch(
    withCallingHandlers(
      check_free_variables_impl(fn_body, c("x"), "kw", "hook"),
      warning = function(w) { invokeRestart("muffleWarning") }
    ),
    warning = identity
  )

  # Reset cache to ensure second call exercises the cache path
  # (we cannot reset it, but a second call should yield the same warn behavior)
  expect_warning(
    check_free_variables_impl(fn_body, c("x"), "kw", "hook"),
    "outside_name"
  )
})

test_that("check_free_variables_impl: base R operators and keywords are safe", {
  fn_body <- quote({
    if (x > 0) {
      for (i in seq_along(x)) {
        y <- x[[i]] + 1L
      }
    }
  })
  expect_no_warning(
    check_free_variables_impl(fn_body, c("x", "y", "i"), "kw", "resolve_expr")
  )
})

test_that("ptr_check_free_variables: delegates correctly — warns on free variable", {
  fn_expr <- quote(function(id, copy, meta, context) {
    shiny::textInput(id, my_external_var)
  })
  expect_warning(
    ptr_check_free_variables(fn_expr, "mykw", "build_ui"),
    "my_external_var"
  )
})

test_that("ptr_check_free_variables_fn: delegates correctly — warns on free variable", {
  fn_obj <- function(id, copy, meta, context) {
    shiny::textInput(id, another_external_var)
  }
  expect_warning(
    ptr_check_free_variables_fn(fn_obj, "mykw", "build_ui"),
    "another_external_var"
  )
})

test_that("ptr_check_free_variables and _fn agree on a clean function", {
  fn_obj <- function(id, copy, meta, context) {
    shiny::textInput(id, copy$label)
  }
  fn_expr <- quote(function(id, copy, meta, context) {
    shiny::textInput(id, copy$label)
  })

  expect_no_warning(ptr_check_free_variables(fn_expr, "kw", "build_ui"))
  expect_no_warning(ptr_check_free_variables_fn(fn_obj, "kw", "build_ui"))
})

# ---------------------------------------------------------------------------
# 4. ptr_resolve_ui_text NULL traversal abort
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

# ---------------------------------------------------------------------------
# 5. ptr_generate_shiny write error handling
# ---------------------------------------------------------------------------

test_that("ptr_generate_shiny aborts with output path in message on write failure", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  bad_path <- file.path(withr::local_tempdir(), "no_such_dir", "app.R")

  # suppressWarnings: file() emits an R warning before tryCatch converts it
  err <- tryCatch(
    suppressWarnings(ptr_generate_shiny(obj, bad_path, style = FALSE)),
    error = function(e) e
  )

  expect_match(conditionMessage(err), "no_such_dir", fixed = TRUE)
  expect_match(conditionMessage(err), "Failed to write", fixed = TRUE)
})

test_that("ptr_generate_shiny aborts with rlang error class on write failure", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  bad_path <- file.path(withr::local_tempdir(), "missing_subdir", "out.R")

  suppressWarnings(
    expect_error(
      ptr_generate_shiny(obj, bad_path, style = FALSE),
      class = "rlang_error"
    )
  )
})

test_that("ptr_generate_shiny succeeds when output path is valid", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- withr::local_tempfile(fileext = ".R")

  expect_no_error(ptr_generate_shiny(obj, out_file, style = FALSE))
  expect_true(file.exists(out_file))
})

# ---------------------------------------------------------------------------
# 6. ptr_generate_shiny — legacy `...` forwarding removed
# ---------------------------------------------------------------------------

test_that("ptr_generate_shiny errors on any unknown named argument", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()"
  )
  out_file <- withr::local_tempfile(fileext = ".R")

  expect_error(
    ptr_generate_shiny(obj, out_file, legacy_var = TRUE),
    class = "error"
  )
})
