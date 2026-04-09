# Helper: build a minimal valid placeholder structure without going through
# ptr_define_placeholder() so we can break individual fields.
# Uses direct field assignment so list overrides truly replace (not merge).
make_raw_placeholder <- function(
  keyword        = "mytype",
  build_ui       = function(id, copy, meta, context) shiny::textInput(id, copy$label),
  resolve_expr   = function(value, meta, context) rlang::expr(!!value),
  resolve_input  = NULL,
  bind_ui        = NULL,
  prepare_eval_env = NULL,
  copy_defaults  = list(label = "Enter a value for {param}"),
  definition_call = NULL
) {
  structure(
    list(
      keyword          = keyword,
      build_ui         = build_ui,
      resolve_expr     = resolve_expr,
      resolve_input    = resolve_input,
      bind_ui          = bind_ui,
      prepare_eval_env = prepare_eval_env,
      copy_defaults    = copy_defaults,
      definition_call  = definition_call
    ),
    class = c("ptr_define_placeholder", "list")
  )
}

# Helper: build a minimal valid ptr_state-like object (no Shiny reactives needed
# for the structural checks that abort before reaching reactive accessor checks).
# Uses direct field assignment so list overrides truly replace (not merge).
make_raw_state <- function(
  obj                 = function() NULL,
  runtime             = function() NULL,
  var_ui_list         = function() NULL,
  raw_ui_text         = NULL,
  effective_ui_text   = NULL,
  placeholders        = ptr_merge_placeholders(),
  custom_placeholders = list(),
  ids                 = ptr_build_ids(),
  envir               = globalenv()
) {
  structure(
    list(
      obj                 = obj,
      runtime             = runtime,
      var_ui_list         = var_ui_list,
      raw_ui_text         = raw_ui_text,
      effective_ui_text   = effective_ui_text,
      placeholders        = placeholders,
      custom_placeholders = custom_placeholders,
      ids                 = ids,
      envir               = envir
    ),
    class = "ptr_state"
  )
}

# ── ptr_validate_ids ──────────────────────────────────────────────────────────

test_that("ptr_validate_ids aborts when ids is not a named list", {
  expect_error(
    ptr_validate_ids(c("a", "b", "c")),
    "ids must be a named list"
  )

  expect_error(
    ptr_validate_ids(list("a", "b")),   # unnamed list
    "ids must be a named list"
  )
})

test_that("ptr_validate_ids aborts on missing required entries", {
  incomplete <- list(
    control_panel  = "cp",
    draw_button    = "draw",
    export_button  = "exp"
    # plot_output, error_output, code_output missing
  )
  expect_error(
    ptr_validate_ids(incomplete),
    "missing required entries"
  )
})

test_that("ptr_validate_ids aborts on extra unsupported entries", {
  full <- list(
    control_panel  = "cp",
    draw_button    = "draw",
    export_button  = "exp",
    plot_output    = "plot",
    error_output   = "err",
    code_output    = "code",
    extra_field    = "extra"
  )
  expect_error(
    ptr_validate_ids(full),
    "unsupported entries"
  )
})

test_that("ptr_validate_ids aborts on non-string or empty string values", {
  # Non-string value
  bad_type <- list(
    control_panel  = 42L,
    draw_button    = "draw",
    export_button  = "exp",
    plot_output    = "plot",
    error_output   = "err",
    code_output    = "code"
  )
  expect_error(
    ptr_validate_ids(bad_type),
    "must be a single non-empty string"
  )

  # Empty string value
  empty_str <- list(
    control_panel  = "  ",   # whitespace only
    draw_button    = "draw",
    export_button  = "exp",
    plot_output    = "plot",
    error_output   = "err",
    code_output    = "code"
  )
  expect_error(
    ptr_validate_ids(empty_str),
    "must be a single non-empty string"
  )

  # Vector of strings (length > 1)
  multi_str <- list(
    control_panel  = c("cp1", "cp2"),
    draw_button    = "draw",
    export_button  = "exp",
    plot_output    = "plot",
    error_output   = "err",
    code_output    = "code"
  )
  expect_error(
    ptr_validate_ids(multi_str),
    "must be a single non-empty string"
  )
})

test_that("ptr_validate_ids aborts on duplicate id values", {
  dupe <- list(
    control_panel  = "shared_id",
    draw_button    = "shared_id",
    export_button  = "exp",
    plot_output    = "plot",
    error_output   = "err",
    code_output    = "code"
  )
  expect_error(
    ptr_validate_ids(dupe),
    "must be unique"
  )
})

test_that("ptr_build_ids accepts defaults and custom values without error", {
  expect_s3_class(ptr_build_ids(), "ptr_build_ids")
  custom <- ptr_build_ids(control_panel = "myPanel", draw_button = "myDraw")
  expect_identical(custom$control_panel, "myPanel")
})

# ── ptr_validate_state ────────────────────────────────────────────────────────

test_that("ptr_validate_state aborts when object does not inherit ptr_state", {
  not_state <- list(obj = function() NULL)
  expect_error(
    ptr_validate_state(not_state),
    "must inherit from 'ptr_state'"
  )
})

test_that("ptr_validate_state aborts on missing required entries", {
  # Build a state that lacks 'runtime' by constructing the list directly
  bad <- structure(
    list(
      obj                 = function() NULL,
      # runtime intentionally omitted
      var_ui_list         = function() NULL,
      raw_ui_text         = NULL,
      effective_ui_text   = NULL,
      placeholders        = ptr_merge_placeholders(),
      custom_placeholders = list(),
      ids                 = ptr_build_ids(),
      envir               = globalenv()
    ),
    class = "ptr_state"
  )
  expect_error(
    ptr_validate_state(bad),
    "missing required entries"
  )
})

test_that("ptr_validate_state aborts when reactive accessors are not functions", {
  bad_obj <- make_raw_state(obj = "not_a_function")
  expect_error(
    ptr_validate_state(bad_obj),
    "reactive accessors must be functions"
  )

  bad_runtime <- make_raw_state(runtime = 42L)
  expect_error(
    ptr_validate_state(bad_runtime),
    "reactive accessors must be functions"
  )

  bad_var_ui <- make_raw_state(var_ui_list = list())
  expect_error(
    ptr_validate_state(bad_var_ui),
    "reactive accessors must be functions"
  )
})

test_that("ptr_validate_state aborts when placeholders is not a ptr_define_placeholder_registry", {
  bad <- make_raw_state(placeholders = list(foo = "bar"))
  expect_error(
    ptr_validate_state(bad),
    "ptr_define_placeholder_registry"
  )
})

# ── ptr_validate_placeholder ──────────────────────────────────────────────────

test_that("ptr_validate_placeholder aborts when object does not inherit ptr_define_placeholder", {
  not_ph <- list(keyword = "mytype", build_ui = function(...) NULL, resolve_expr = function(...) NULL)
  expect_error(
    ptr_validate_placeholder(not_ph),
    "must inherit from 'ptr_define_placeholder'"
  )
})

test_that("ptr_validate_placeholder aborts on missing required entries", {
  # Drop 'build_ui' from the raw structure
  bad <- make_raw_placeholder()
  bad[["build_ui"]] <- NULL
  bad <- structure(bad[names(bad) != "build_ui"], class = c("ptr_define_placeholder", "list"))
  expect_error(
    ptr_validate_placeholder(bad),
    "missing required entries"
  )
})

test_that("ptr_validate_placeholder aborts when keyword is not a single string", {
  bad_num <- make_raw_placeholder(keyword = 123)
  expect_error(
    ptr_validate_placeholder(bad_num),
    "keyword must be a single string"
  )

  bad_vec <- make_raw_placeholder(keyword = c("a", "b"))
  expect_error(
    ptr_validate_placeholder(bad_vec),
    "keyword must be a single string"
  )
})

test_that("ptr_validate_placeholder aborts when build_ui is not a function", {
  bad <- make_raw_placeholder(build_ui = "not_a_function")
  expect_error(
    ptr_validate_placeholder(bad),
    "build_ui must be a function"
  )
})

test_that("ptr_validate_placeholder aborts when optional hooks are not NULL or function", {
  bad_resolve_input <- make_raw_placeholder(resolve_input = "string")
  expect_error(
    ptr_validate_placeholder(bad_resolve_input),
    "resolve_input must be NULL or a function"
  )

  bad_bind_ui <- make_raw_placeholder(bind_ui = 42L)
  expect_error(
    ptr_validate_placeholder(bad_bind_ui),
    "bind_ui must be NULL or a function"
  )

  bad_prep <- make_raw_placeholder(prepare_eval_env = list())
  expect_error(
    ptr_validate_placeholder(bad_prep),
    "prepare_eval_env must be NULL or a function"
  )
})

test_that("ptr_validate_placeholder aborts when copy_defaults is not a named list", {
  # Unnamed list
  bad_unnamed <- make_raw_placeholder(copy_defaults = list("a", "b"))
  expect_error(
    ptr_validate_placeholder(bad_unnamed),
    "copy_defaults must be a named list"
  )

  # Character vector (not a list)
  bad_vec <- make_raw_placeholder(copy_defaults = c(label = "hello"))
  expect_error(
    ptr_validate_placeholder(bad_vec),
    "copy_defaults must be a named list"
  )
})

test_that("ptr_validate_placeholder aborts on unsupported copy_defaults fields", {
  bad <- make_raw_placeholder(copy_defaults = list(label = "ok", bad_field = "x"))
  expect_error(
    ptr_validate_placeholder(bad),
    "unsupported fields"
  )
})

test_that("ptr_validate_placeholder aborts when copy_defaults field values are not single strings", {
  bad_type <- make_raw_placeholder(copy_defaults = list(label = 42L))
  expect_error(
    ptr_validate_placeholder(bad_type),
    "must be a single string"
  )

  bad_vec <- make_raw_placeholder(copy_defaults = list(label = c("a", "b")))
  expect_error(
    ptr_validate_placeholder(bad_vec),
    "must be a single string"
  )
})

# ── ptr_validate_ui_text ──────────────────────────────────────────────────────

test_that("ptr_validate_ui_text aborts when ui_text is not a list", {
  expect_error(
    ptr_validate_ui_text("not_a_list"),
    "ui_text must be a named list"
  )

  expect_error(
    ptr_validate_ui_text(42L),
    "ui_text must be a named list"
  )
})

test_that("ptr_validate_ui_text aborts on unsupported shell entries", {
  expect_error(
    ptr_validate_ui_text(list(shell = list(bad_key = list(label = "x")))),
    "ui_text\\$shell has unsupported entries"
  )
})

test_that("ptr_validate_ui_text aborts on unsupported upload entries", {
  expect_error(
    ptr_validate_ui_text(list(upload = list(bad_key = list(label = "x")))),
    "ui_text\\$upload has unsupported entries"
  )
})

test_that("ptr_validate_ui_text aborts on invalid layer_checkbox leaf", {
  expect_error(
    ptr_validate_ui_text(list(layer_checkbox = list(bad_field = "x"))),
    "unsupported fields"
  )
})

test_that("ptr_validate_ui_text aborts on unsupported defaults entries", {
  expect_error(
    ptr_validate_ui_text(list(defaults = list(unknown_keyword = list(label = "x")))),
    "ui_text\\$defaults has unsupported entries"
  )
})

test_that("ptr_validate_ui_text aborts when params entry is not a list", {
  expect_error(
    ptr_validate_ui_text(list(params = list(x = "not_a_list"))),
    "must be a named list"
  )
})

test_that("ptr_validate_ui_text aborts when params entry contains unknown keywords", {
  expect_error(
    ptr_validate_ui_text(list(params = list(x = list(unknown_keyword = list(label = "x"))))),
    "unsupported keywords"
  )
})

test_that("ptr_validate_ui_text aborts when layers entry is not a list", {
  expect_error(
    ptr_validate_ui_text(list(layers = list(geom_point = "not_a_list"))),
    "must be a named list"
  )
})

test_that("ptr_validate_ui_text aborts when layers entry contains unknown keywords", {
  expect_error(
    ptr_validate_ui_text(list(layers = list(geom_point = list(unknown_keyword = list())))),
    "unsupported keywords"
  )
})

test_that("ptr_validate_ui_text aborts when layers keyword entry is not a list", {
  expect_error(
    ptr_validate_ui_text(list(layers = list(geom_point = list(var = "not_a_list")))),
    "must be a named list"
  )
})

# ── ptr_normalize_placeholders ────────────────────────────────────────────────

test_that("ptr_normalize_placeholders aborts on name-keyword mismatch", {
  ph <- ptr_define_placeholder(
    keyword  = "mytype",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) rlang::expr(!!value)
  )

  # Supply it under a different name than its keyword
  expect_error(
    ptr_normalize_placeholders(list(wrong_name = ph)),
    "does not match placeholder keyword"
  )
})

test_that("ptr_normalize_placeholders accepts unnamed list entries (name equals keyword)", {
  ph <- ptr_define_placeholder(
    keyword  = "mytype",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) rlang::expr(!!value)
  )

  # No name supplied — should succeed and index by keyword
  result <- ptr_normalize_placeholders(list(ph))
  expect_true("mytype" %in% names(result))
})

test_that("ptr_normalize_placeholders accepts correctly named entries", {
  ph <- ptr_define_placeholder(
    keyword  = "mytype",
    build_ui = function(id, copy, meta, context) shiny::textInput(id, copy$label),
    resolve_expr = function(value, meta, context) rlang::expr(!!value)
  )

  result <- ptr_normalize_placeholders(list(mytype = ph))
  expect_true("mytype" %in% names(result))
})
