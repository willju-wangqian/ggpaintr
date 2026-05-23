# PLAN-07 — Unified `validate_input(value, ctx)` hook across value and
# consumer placeholder roles. The hook signature is `function(value, ctx)`
# for both roles; `ctx` is a plain list with named fields `node`,
# `keyword`, `upstream_cols`, `data`. Value-role placeholders see NULL
# `upstream_cols` / `data`; consumer-role placeholders see populated
# fields when the upstream has resolved.

# ---- helpers ---------------------------------------------------------------

.vic_build_ui_value <- function(node, ...) shiny::textInput(node$id, "x")
.vic_build_ui_consumer <- function(node, cols, data, ...) {
  shiny::selectInput(node$id, "x", choices = cols)
}
.vic_resolve_expr <- function(value, node, ...) value
.vic_resolve_expr_sym <- function(value, node, ...) rlang::sym(value)
.vic_unique_kw <- function(prefix) {
  paste0(prefix, "_", as.integer(Sys.time()), "_",
         sample.int(.Machine$integer.max, 1L))
}

# ---- value-role: constructor + registration --------------------------------

test_that("ptr_define_placeholder_value() accepts validate_input(value, ctx) and stores it", {
  kw <- .vic_unique_kw("vicv")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  validator <- function(value, ctx) if (value > 0) TRUE else "must be positive"
  fn <- ggpaintr:::ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .vic_build_ui_value,
    resolve_expr = .vic_resolve_expr,
    validate_input = validator
  )
  expect_true(is.function(fn))
  entry <- ggpaintr:::ptr_registry_lookup(kw)
  expect_true(is.list(entry))
  expect_identical(entry$validate_input, validator)
})

test_that("ptr_define_placeholder_value() rejects legacy (value, upstream_cols) signature", {
  kw <- .vic_unique_kw("vicv")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  expect_error(
    ggpaintr:::ptr_define_placeholder_value(
      keyword = kw,
      build_ui = .vic_build_ui_value,
      resolve_expr = .vic_resolve_expr,
      validate_input = function(value, upstream_cols) TRUE
    ),
    regexp = "validate_input.*ctx"
  )
})

# ---- consumer-role: constructor + registration -----------------------------

test_that("ptr_define_placeholder_consumer() accepts validate_input(value, ctx) and stores it", {
  kw <- .vic_unique_kw("vicc")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  validator <- function(value, ctx) {
    if (value %in% ctx$upstream_cols) TRUE else "no such column"
  }
  fn <- ggpaintr:::ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = .vic_build_ui_consumer,
    resolve_expr = .vic_resolve_expr_sym,
    validate_input = validator
  )
  expect_true(is.function(fn))
  entry <- ggpaintr:::ptr_registry_lookup(kw)
  expect_identical(entry$validate_input, validator)
})

test_that("ptr_define_placeholder_consumer() rejects legacy (value, upstream_cols) signature", {
  kw <- .vic_unique_kw("vicc")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  expect_error(
    ggpaintr:::ptr_define_placeholder_consumer(
      keyword = kw,
      build_ui = .vic_build_ui_consumer,
      resolve_expr = .vic_resolve_expr_sym,
      validate_input = function(value, upstream_cols) TRUE
    ),
    regexp = "validate_input.*ctx"
  )
})

# ---- value-role: substitute walker invokes hook with NULL upstream fields --

test_that("value-role substitute walker invokes validate_input with NULL ctx$upstream_cols/data and aborts on string return", {
  kw <- .vic_unique_kw("vicv")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  # Validator stashes the ctx fields and returns "bad" so substitute
  # aborts; the error message must contain BOTH the literal "bad" and
  # the placeholder's keyword.
  seen <- new.env(parent = emptyenv())
  ggpaintr:::ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .vic_build_ui_value,
    resolve_expr = .vic_resolve_expr,
    validate_input = function(value, ctx) {
      seen$node_keyword <- ctx$node$keyword
      seen$keyword <- ctx$keyword
      seen$upstream_cols <- ctx$upstream_cols
      seen$data <- ctx$data
      "bad"
    }
  )
  # Build a minimal placeholder node directly (no formula parse needed).
  node <- ggpaintr:::ptr_ph_value(
    id = paste0("test_", kw), keyword = kw,
    expr = rlang::sym(kw)
  )
  snapshot <- stats::setNames(list("any-value"), paste0("test_", kw))
  expect_error(
    ggpaintr:::ptr_substitute(node, input_snapshot = snapshot),
    regexp = paste0("Invalid value for placeholder `", kw,
                    "`: bad"),
    fixed = TRUE
  )
  # Confirm the value-role ctx really had NULL upstream fields.
  expect_null(seen$upstream_cols)
  expect_null(seen$data)
  expect_identical(seen$keyword, kw)
  expect_identical(seen$node_keyword, kw)
})

test_that("value-role validate_input accepts TRUE / NULL and proceeds to resolve_expr", {
  kw <- .vic_unique_kw("vicv")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  ggpaintr:::ptr_define_placeholder_value(
    keyword = kw,
    build_ui = .vic_build_ui_value,
    resolve_expr = function(value, node, ...) value,
    validate_input = function(value, ctx) {
      # The stopifnot ensures ctx contains exactly the documented fields.
      stopifnot(is.null(ctx$upstream_cols), is.null(ctx$data))
      stopifnot(identical(ctx$keyword, node$keyword))
      TRUE
    }
  )
  node <- ggpaintr:::ptr_ph_value(
    id = paste0("test_", kw), keyword = kw,
    expr = rlang::sym(kw)
  )
  snapshot <- stats::setNames(list("ok"), paste0("test_", kw))
  res <- ggpaintr:::ptr_substitute(node, input_snapshot = snapshot)
  expect_s3_class(res, "ptr_literal")
})

# ---- consumer-role: substitute walker forwards data via ctx ---------------

test_that("consumer-role validate_input reads ctx$data to reject a non-numeric column", {
  kw <- .vic_unique_kw("vicc")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  ggpaintr:::ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = .vic_build_ui_consumer,
    resolve_expr = .vic_resolve_expr_sym,
    validate_input = function(value, ctx) {
      if (!is.numeric(ctx$data[[value]])) {
        return(paste0("column `", value, "` is not numeric"))
      }
      TRUE
    }
  )
  node_id <- paste0("test_", kw)
  node <- ggpaintr:::ptr_ph_data_consumer(
    id = node_id, keyword = kw,
    expr = rlang::sym(kw)
  )
  df <- data.frame(x = c(1, 2, 3), g = c("a", "b", "c"),
                   stringsAsFactors = FALSE)
  # Set value to "g" (the character column) — should reject with the
  # validator's literal message.
  snapshot_g <- stats::setNames(list("g"), node_id)
  expect_error(
    ggpaintr:::ptr_substitute(
      node, input_snapshot = snapshot_g,
      upstream_cols = stats::setNames(list(names(df)), node_id),
      upstream_data = stats::setNames(list(df), node_id)
    ),
    regexp = "column `g` is not numeric", fixed = TRUE
  )
  # Set value to "x" (the numeric column) — should accept and resolve.
  snapshot_x <- stats::setNames(list("x"), node_id)
  res <- ggpaintr:::ptr_substitute(
    node, input_snapshot = snapshot_x,
    upstream_cols = stats::setNames(list(names(df)), node_id),
    upstream_data = stats::setNames(list(df), node_id)
  )
  expect_s3_class(res, "ptr_literal")
  expect_identical(res$expr, rlang::sym("x"))
})

test_that("consumer-role ctx$upstream_cols matches the cols vector passed to build_ui", {
  kw <- .vic_unique_kw("vicc")
  withr::defer(try(ggpaintr:::ptr_clear_placeholder(kw), silent = TRUE))
  # Stash the cols arg from build_ui and ctx$upstream_cols from the
  # validator into the same env; they MUST be identical character vectors.
  stash <- new.env(parent = emptyenv())
  ggpaintr:::ptr_define_placeholder_consumer(
    keyword = kw,
    build_ui = function(node, cols, data, ...) {
      stash$build_ui_cols <- cols
      shiny::selectInput(node$id, "x", choices = cols)
    },
    resolve_expr = .vic_resolve_expr_sym,
    validate_input = function(value, ctx) {
      stash$validator_upstream_cols <- ctx$upstream_cols
      TRUE
    }
  )
  node_id <- paste0("test_", kw)
  node <- ggpaintr:::ptr_ph_data_consumer(
    id = node_id, keyword = kw,
    expr = rlang::sym(kw)
  )
  cols <- c("mpg", "cyl", "wt")
  df <- mtcars[, cols]
  # Trigger build_ui directly (consumer's `cols` arg is whatever caller
  # passes; the runtime path passes the upstream-cols vector). Then run
  # substitute to populate `validator_upstream_cols`.
  registry_entry <- ggpaintr:::ptr_registry_lookup(kw)
  invisible(registry_entry$build_ui(node, cols = cols, data = df, label = NULL))
  snapshot <- stats::setNames(list("mpg"), node_id)
  ggpaintr:::ptr_substitute(
    node, input_snapshot = snapshot,
    upstream_cols = stats::setNames(list(cols), node_id),
    upstream_data = stats::setNames(list(df), node_id)
  )
  expect_identical(stash$build_ui_cols, stash$validator_upstream_cols)
})

# ---- ppVar built-in regression --------------------------------------------

test_that("ppVar built-in validate_input returns documented length-mismatch string for length != 1", {
  res <- ggpaintr:::ptr_builtin_var_validate_input(
    c("mpg", "wt"),
    list(upstream_cols = c("mpg", "wt", "cyl"), data = NULL)
  )
  expect_identical(res, "Expected a single column; got 2.")
})

test_that("ppVar built-in validate_input returns documented unknown-column string for missing column", {
  res <- ggpaintr:::ptr_builtin_var_validate_input(
    "hp",
    list(upstream_cols = c("mpg", "wt"), data = NULL)
  )
  expect_identical(res, "Column `hp` is not in the data.")
})

test_that("ppVar built-in accepts a valid column and substitute returns rlang::sym(value)", {
  res <- ggpaintr:::ptr_builtin_var_validate_input(
    "mpg",
    list(upstream_cols = c("mpg", "wt"), data = NULL)
  )
  expect_true(isTRUE(res))
  # Round-trip through the consumer substitute walker to confirm
  # validate_input + resolve_expr land on rlang::sym("mpg").
  node_id <- "ppvar_node_x"
  node <- ggpaintr:::ptr_ph_data_consumer(
    id = node_id, keyword = "ppVar",
    expr = rlang::sym("ppVar")
  )
  cols <- c("mpg", "wt")
  out <- ggpaintr:::ptr_substitute(
    node,
    input_snapshot = stats::setNames(list("mpg"), node_id),
    upstream_cols = stats::setNames(list(cols), node_id),
    upstream_data = stats::setNames(list(mtcars[, cols]), node_id)
  )
  expect_s3_class(out, "ptr_literal")
  expect_identical(out$expr, rlang::sym("mpg"))
})
