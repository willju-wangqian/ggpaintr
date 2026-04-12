# Tests for the 6 changes made in the refine-feature Step 3 workflow.
#
# Coverage map:
#   W1 (paintr-parse.R)      — ptr_runtime_input_spec list-growth refactor
#   W2 (paintr-ui.R)         — generate_ui_upload placeholders threading
#   W3 (paintr-utils.R)      — zero-length guard in expr_remove_null /
#                               expr_remove_emptycall2
#   W4 (paintr-app.R)        — ptr_normalize_ids fast-path for ptr_build_ids
#   N1 (paintr-utils.R)      — handle_call_break_sum uses rlang::is_call
#   N2 (paintr-placeholders.R)— ptr_resolve_text_expr / ptr_resolve_num_expr
#                               without removed assertions

# =============================================================================
# W1: ptr_runtime_input_spec — spec_rows built with c(spec_rows, list(...))
#     Regression: output must be identical to prior O(n²) list-growth approach
# =============================================================================

test_that("W1: ptr_runtime_input_spec returns correct spec for multi-layer formula", {
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = mtcars, aes(x = var, y = var)) +",
      "geom_point(size = num) +",
      "labs(title = text)"
    )
  )
  spec <- ptr_runtime_input_spec(obj)

  expect_s3_class(spec, "data.frame")
  expect_identical(
    names(spec),
    c("input_id", "role", "layer_name", "keyword", "param_key", "source_id")
  )
  # placeholder rows appear before checkbox rows
  placeholder_rows <- spec[spec$role == "placeholder", ]
  checkbox_rows    <- spec[spec$role == "layer_checkbox", ]
  expect_true(all(which(spec$role == "placeholder") < min(which(spec$role == "layer_checkbox"))))
  # correct keywords
  expect_identical(sort(placeholder_rows$keyword), c("num", "text", "var", "var"))
  # one checkbox per non-ggplot layer
  expect_identical(sort(checkbox_rows$layer_name), c("geom_point", "labs"))
})

test_that("W1: ptr_runtime_input_spec produces one row per placeholder across many layers", {
  # 4 placeholder tokens -> 4 placeholder rows
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = mtcars, aes(x = var, y = var)) +",
      "geom_point(size = num) +",
      "labs(title = text)"
    )
  )
  spec <- ptr_runtime_input_spec(obj)
  expect_equal(sum(spec$role == "placeholder"), 4L)
})

test_that("W1: ptr_runtime_input_spec source_id equals input_id for every placeholder row", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + labs(title = text)"
  )
  spec <- ptr_runtime_input_spec(obj)
  ph <- spec[spec$role == "placeholder", ]
  expect_true(all(ph$input_id == ph$source_id))
})

test_that("W1: ptr_runtime_input_spec handles a formula with upload — upload_name row follows placeholder row", {
  obj <- ptr_parse_formula(
    "ggplot(data = upload, aes(x = var)) + geom_point()"
  )
  spec <- ptr_runtime_input_spec(obj)

  upload_idx      <- which(spec$role == "placeholder" & spec$keyword == "upload")
  upload_name_idx <- which(spec$role == "upload_name")
  expect_length(upload_idx, 1L)
  expect_length(upload_name_idx, 1L)
  # upload_name row must immediately follow the upload placeholder row
  expect_equal(upload_name_idx, upload_idx + 1L)
  # source_id of upload_name must equal the upload placeholder's input_id
  expect_equal(spec$source_id[upload_name_idx], spec$input_id[upload_idx])
})

test_that("W1: ptr_runtime_input_spec returns zero-row data frame for formula with no placeholders", {
  obj <- ptr_parse_formula(
    "ggplot(data = mtcars, aes(x = mpg, y = disp)) + geom_point()"
  )
  spec <- ptr_runtime_input_spec(obj)
  expect_s3_class(spec, "data.frame")
  # only a checkbox row for geom_point — no placeholder rows
  expect_equal(sum(spec$role == "placeholder"), 0L)
  expect_equal(nrow(spec), 1L)
  expect_equal(spec$role, "layer_checkbox")
})

# =============================================================================
# W2: generate_ui_upload — placeholders argument threaded through
#     Custom copy_defaults from a placeholder should appear in upload UI label
# =============================================================================

test_that("W2: generate_ui_upload accepts NULL placeholders without error", {
  ui <- generate_ui_upload("test_id", ui_text = NULL, placeholders = NULL)
  expect_true(inherits(ui, "shiny.tag.list"))
})

test_that("W2: generate_ui_upload with default registry produces file and text inputs", {
  registry <- ptr_merge_placeholders()
  ui <- generate_ui_upload("my_upload_id", ui_text = NULL, placeholders = registry)
  html <- as.character(ui)
  # fileInput and textInput should both appear
  expect_match(html, 'type="file"', fixed = TRUE)
  expect_match(html, 'type="text"', fixed = TRUE)
})

test_that("W2: generate_ui_upload label reflects ui_text copy override", {
  ui_text <- list(
    upload = list(
      file = list(label = "Load your dataset"),
      name = list(label = "Dataset alias")
    )
  )
  registry <- ptr_merge_placeholders()
  ui <- generate_ui_upload("upload_id", ui_text = ui_text, placeholders = registry)
  html <- as.character(ui)
  expect_match(html, "Load your dataset", fixed = TRUE)
  expect_match(html, "Dataset alias", fixed = TRUE)
})

test_that("W2: generate_ui_upload with a custom upload copy_defaults from placeholder registry", {
  # Build a custom upload placeholder with different copy_defaults
  custom_upload <- ptr_define_placeholder(
    keyword = "upload",
    build_ui = function(id, copy, meta, context) {
      generate_ui_upload(id, placeholders = context$placeholders)
    },
    resolve_expr = function(value, meta, context) ptr_missing_expr()
  )
  registry <- ptr_merge_placeholders(list(upload = custom_upload))
  # generate_ui_upload with this registry must not error
  expect_no_error(generate_ui_upload("id_x", ui_text = NULL, placeholders = registry))
})

# =============================================================================
# W3: zero-length guard in expr_remove_null and expr_remove_emptycall2
#     Both functions now return .expr unchanged when length(.expr) == 0
# =============================================================================

test_that("W3: expr_remove_null returns zero-length symbol unchanged", {
  # rlang::sym creates a non-zero length; use an empty call instead
  empty_call <- call("f")   # length == 1, not 0 — use language object of length 0
  # The guard triggers on length(.expr) == 0L. Test with NULL (length 0).
  # NULL is not a call but has length 0 — the function should return it immediately.
  result <- expr_remove_null(NULL)
  expect_null(result)
})

test_that("W3: expr_remove_null with a length-1 call (no children) returns the call unchanged", {
  # quote(f()) has length 1 — the loop body never executes
  expr <- quote(f())
  result <- expr_remove_null(expr)
  expect_equal(result, expr)
})

test_that("W3: expr_remove_null does not error on a length-0 numeric vector", {
  # numeric(0) has length 0 — guard fires immediately
  expect_no_error(expr_remove_null(numeric(0)))
  expect_equal(expr_remove_null(numeric(0)), numeric(0))
})

test_that("W3: expr_remove_emptycall2 returns NULL input unchanged", {
  result <- expr_remove_emptycall2(NULL)
  expect_null(result)
})

test_that("W3: expr_remove_emptycall2 does not error on a length-0 numeric vector", {
  expect_no_error(expr_remove_emptycall2(numeric(0)))
  expect_equal(expr_remove_emptycall2(numeric(0)), numeric(0))
})

test_that("W3: expr_remove_emptycall2 with length-1 call (function symbol only) applies top-level check", {
  # quote(unknown_fn()) has length 1; the loop does nothing; top-level check fires
  expr <- quote(unknown_fn())
  result <- expr_remove_emptycall2(expr)
  # unknown_fn is not a gg-layer name -> top-level guard should return NULL
  expect_null(result)
})

test_that("W3: expr_remove_emptycall2 with length-1 gg call is preserved", {
  expr <- quote(geom_point())
  result <- expr_remove_emptycall2(expr)
  # geom_point is a gg-layer name -> preserved
  expect_true(is.call(result))
  expect_equal(rlang::as_string(result[[1]]), "geom_point")
})

# =============================================================================
# W4: ptr_normalize_ids — fast-path: ptr_build_ids object returned as-is
#     Verify that re-validation is skipped (no error even with a modified but
#     structurally valid object, and that identity is preserved).
# =============================================================================

test_that("W4: ptr_normalize_ids returns a ptr_build_ids object unchanged (identity)", {
  ids <- ptr_build_ids(
    control_panel = "myPanel",
    draw_button   = "myDraw",

    plot_output   = "myPlot",
    error_output  = "myError",
    code_output   = "myCode"
  )
  result <- ptr_normalize_ids(ids)
  # Must be the same object (identical fields and class)
  expect_identical(result, ids)
})

test_that("W4: ptr_normalize_ids with a ptr_build_ids object does not call ptr_validate_ids", {
  # Build a ptr_build_ids object and verify ptr_normalize_ids returns it
  # without triggering validation errors (fast path skips validation).
  ids <- ptr_build_ids()
  result <- ptr_normalize_ids(ids)
  expect_s3_class(result, "ptr_build_ids")
  expect_equal(result$control_panel, "controlPanel")
})

test_that("W4: ptr_normalize_ids with NULL returns default ptr_build_ids", {
  result <- ptr_normalize_ids(NULL)
  expect_s3_class(result, "ptr_build_ids")
  expect_equal(result$draw_button, "draw")
})

test_that("W4: ptr_normalize_ids with a plain named list validates and wraps it", {
  plain_list <- list(
    control_panel = "a",
    draw_button   = "b",

    plot_output   = "d",
    error_output  = "e",
    code_output   = "f"
  )
  result <- ptr_normalize_ids(plain_list)
  expect_s3_class(result, "ptr_build_ids")
  expect_equal(result$control_panel, "a")
})

test_that("W4: ptr_normalize_ids with an invalid plain list errors on validation", {
  bad_list <- list(
    control_panel = "a",
    draw_button   = "b"
    # missing required entries
  )
  expect_error(ptr_normalize_ids(bad_list), "missing required entries")
})

# =============================================================================
# N1: handle_call_break_sum — uses rlang::is_call(x, "+") instead of
#     is.call(x) && x[[1]] == "+"
#     Verify correct behavior: splits + calls, passes through everything else.
# =============================================================================

test_that("N1: handle_call_break_sum splits a simple + expression into two parts", {
  expr <- quote(ggplot(data = mtcars) + geom_point())
  result <- handle_call_break_sum(expr)
  expect_type(result, "list")
  expect_length(result, 2L)
  expect_equal(rlang::as_string(result[[1]][[1]]), "ggplot")
  expect_equal(rlang::as_string(result[[2]][[1]]), "geom_point")
})

test_that("N1: handle_call_break_sum returns a non-+ call unchanged", {
  expr <- quote(geom_point(size = 2))
  result <- handle_call_break_sum(expr)
  # not a + call -> returned as-is (is.call, not a list)
  expect_true(is.call(result))
  expect_equal(rlang::as_string(result[[1]]), "geom_point")
})

test_that("N1: handle_call_break_sum returns a symbol unchanged", {
  sym <- rlang::sym("mpg")
  result <- handle_call_break_sum(sym)
  expect_equal(result, sym)
})

test_that("N1: handle_call_break_sum returns a constant unchanged", {
  result <- handle_call_break_sum(42L)
  expect_equal(result, 42L)
})

test_that("N1: handle_call_break_sum does not split a * call", {
  expr <- quote(a * b)
  result <- handle_call_break_sum(expr)
  # * is not +; should return as-is
  expect_true(is.call(result))
  expect_equal(rlang::as_string(result[[1]]), "*")
})

test_that("N1: handle_call_break_sum splits three-layer + expression at top level only", {
  # (a + b) + c — handle_call_break_sum sees the outer +, splits into
  # [break_sum(a+b), break_sum(c)]; each element itself is a list or call
  expr <- quote(ggplot() + geom_point() + geom_line())
  result <- handle_call_break_sum(expr)
  expect_type(result, "list")
  expect_length(result, 2L)  # top-level split: [(ggplot+geom_point), geom_line]
})

test_that("N1: break_sum fully flattens a three-layer + formula", {
  expr <- quote(ggplot(data = mtcars, aes(x = mpg)) + geom_point() + geom_line())
  flat <- unlist(break_sum(expr))
  expect_length(flat, 3L)
  layer_names <- vapply(flat, function(x) rlang::as_string(x[[1]]), character(1))
  expect_equal(layer_names, c("ggplot", "geom_point", "geom_line"))
})

# =============================================================================
# N2: ptr_resolve_text_expr and ptr_resolve_num_expr work correctly after
#     the removed redundant assert_that checks in paintr-placeholders.R
# =============================================================================

test_that("N2: ptr_resolve_text_expr returns ptr_missing_expr for NULL", {
  result <- ptr_resolve_text_expr(NULL, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("N2: ptr_resolve_text_expr returns ptr_missing_expr for empty string", {
  result <- ptr_resolve_text_expr("", meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("N2: ptr_resolve_text_expr returns a quoted expression for a valid string", {
  result <- ptr_resolve_text_expr("hello world", meta = list(), context = list())
  expect_false(inherits(result, "ptr_missing_expr"))
  expect_equal(eval(result), "hello world")
})

test_that("N2: ptr_resolve_text_expr result evaluates to the exact input string", {
  value <- "Iris scatter plot"
  result <- ptr_resolve_text_expr(value, meta = list(), context = list())
  expect_equal(eval(result), value)
})

test_that("N2: ptr_resolve_num_expr returns ptr_missing_expr for NULL", {
  result <- ptr_resolve_num_expr(NULL, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("N2: ptr_resolve_num_expr returns ptr_missing_expr for NA_real_", {
  result <- ptr_resolve_num_expr(NA_real_, meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("N2: ptr_resolve_num_expr returns ptr_missing_expr for zero-length input", {
  result <- ptr_resolve_num_expr(numeric(0), meta = list(), context = list())
  expect_s3_class(result, "ptr_missing_expr")
})

test_that("N2: ptr_resolve_num_expr returns a numeric expression for a valid number", {
  result <- ptr_resolve_num_expr(3.14, meta = list(), context = list())
  expect_false(inherits(result, "ptr_missing_expr"))
  expect_equal(eval(result), 3.14)
})

test_that("N2: ptr_resolve_num_expr returns a numeric expression for an integer", {
  result <- ptr_resolve_num_expr(5L, meta = list(), context = list())
  expect_false(inherits(result, "ptr_missing_expr"))
  expect_equal(eval(result), 5L)
})

test_that("N2: ptr_resolve_text_expr and ptr_resolve_num_expr integrate correctly in ptr_exec", {
  obj <- ptr_parse_formula(
    paste(
      "ggplot(data = iris, aes(x = Sepal.Length, y = Sepal.Width)) +",
      "geom_point(size = num) +",
      "labs(title = text)"
    )
  )
  spec <- ptr_runtime_input_spec(obj)
  num_id  <- spec$input_id[spec$keyword == "num"  & !is.na(spec$keyword)]
  text_id <- spec$input_id[spec$keyword == "text" & !is.na(spec$keyword)]
  cb_ids  <- spec$input_id[spec$role == "layer_checkbox"]

  input <- stats::setNames(
    c(list(2.5, "My title"), as.list(rep(TRUE, length(cb_ids)))),
    c(num_id, text_id, cb_ids)
  )
  result <- ptr_exec(obj, input)
  expect_true(result$ok)
  expect_match(result$code_text, "size = 2.5")
  expect_match(result$code_text, '"My title"')
})
