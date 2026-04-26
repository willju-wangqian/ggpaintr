make_expr_list <- function(...) {
  nms <- c(...)
  stats::setNames(rep(list(NULL), length(nms)), nms)
}

# ---------------------------------------------------------------------------
# Backward compatibility
# ---------------------------------------------------------------------------

test_that("NULL input returns all-TRUE for non-ggplot layers", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_smooth")
  result <- ptr_resolve_checkbox_defaults(NULL, expr_list)
  expect_identical(result, c(geom_point = TRUE, geom_smooth = TRUE))
})

test_that("empty list input is identical to NULL", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_smooth")
  expect_identical(
    ptr_resolve_checkbox_defaults(list(), expr_list),
    ptr_resolve_checkbox_defaults(NULL, expr_list)
  )
})

test_that("formula with no non-ggplot layers returns named logical(0)", {
  expr_list <- make_expr_list("ggplot")
  result <- ptr_resolve_checkbox_defaults(NULL, expr_list)
  expect_identical(result, stats::setNames(logical(0), character(0)))
})

# ---------------------------------------------------------------------------
# Sparse override (Q1)
# ---------------------------------------------------------------------------

test_that("named entries override; missing keys default to TRUE", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_smooth", "labs")
  result <- ptr_resolve_checkbox_defaults(
    list(geom_smooth = FALSE),
    expr_list
  )
  expect_identical(
    result,
    c(geom_point = TRUE, geom_smooth = FALSE, labs = TRUE)
  )
})

test_that("entries apply in formula order regardless of list order", {
  expr_list <- make_expr_list("ggplot", "a", "b", "c")
  result <- ptr_resolve_checkbox_defaults(
    list(c = FALSE, a = FALSE),
    expr_list
  )
  expect_identical(names(result), c("a", "b", "c"))
  expect_identical(unname(result), c(FALSE, TRUE, FALSE))
})

# ---------------------------------------------------------------------------
# Unknown keys (Q3)
# ---------------------------------------------------------------------------

test_that("unknown keys produce a single cli warn naming all of them", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_warning(
    ptr_resolve_checkbox_defaults(
      list(geom_typo = FALSE, geom_other = FALSE),
      expr_list
    ),
    "geom_typo|geom_other"
  )
})

test_that("unknown keys are dropped; recognized keys still applied", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_smooth")
  result <- suppressWarnings(
    ptr_resolve_checkbox_defaults(
      list(geom_typo = FALSE, geom_smooth = FALSE),
      expr_list
    )
  )
  expect_identical(result, c(geom_point = TRUE, geom_smooth = FALSE))
})

test_that("only-unknown input warns but returns all-TRUE", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  result <- suppressWarnings(
    ptr_resolve_checkbox_defaults(list(nope = FALSE), expr_list)
  )
  expect_identical(result, c(geom_point = TRUE))
})

test_that("ggplot key is treated as unknown (not a checkbox layer)", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_warning(
    ptr_resolve_checkbox_defaults(list(ggplot = FALSE), expr_list),
    "ggplot"
  )
})

# ---------------------------------------------------------------------------
# Duplicate-layer addressing (Q4) and length mismatch (Q5)
# ---------------------------------------------------------------------------

test_that("vector value applies positionally within a duplicate group", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_point-2", "geom_smooth")
  result <- ptr_resolve_checkbox_defaults(
    list(geom_point = c(TRUE, FALSE)),
    expr_list
  )
  expect_identical(
    result,
    c(geom_point = TRUE, "geom_point-2" = FALSE, geom_smooth = TRUE)
  )
})

test_that("scalar value against duplicate group fills first slot, pads TRUE", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_point-2")
  result <- ptr_resolve_checkbox_defaults(
    list(geom_point = FALSE),
    expr_list
  )
  expect_identical(result, c(geom_point = FALSE, "geom_point-2" = TRUE))
})

test_that("direct deduped key addresses one specific instance", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_point-2")
  result <- ptr_resolve_checkbox_defaults(
    list(`geom_point-2` = FALSE),
    expr_list
  )
  expect_identical(result, c(geom_point = TRUE, "geom_point-2" = FALSE))
})

test_that("direct key wins; group key fills the rest", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_point-2", "geom_point-3")
  result <- ptr_resolve_checkbox_defaults(
    list(`geom_point-2` = TRUE, geom_point = FALSE),
    expr_list
  )
  # direct key consumes geom_point-2 first; group key applies positionally
  # over remaining slots (geom_point, geom_point-3) starting at index 1
  expect_identical(
    result,
    c(geom_point = FALSE, "geom_point-2" = TRUE, "geom_point-3" = TRUE)
  )
})

test_that("short vector pads with TRUE without warning", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_point-2", "geom_point-3")
  expect_no_warning(
    result <- ptr_resolve_checkbox_defaults(
      list(geom_point = c(FALSE, FALSE)),
      expr_list
    )
  )
  expect_identical(
    result,
    c(geom_point = FALSE, "geom_point-2" = FALSE, "geom_point-3" = TRUE)
  )
})

test_that("long vector warns and truncates", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_point-2")
  expect_warning(
    result <- ptr_resolve_checkbox_defaults(
      list(geom_point = c(TRUE, FALSE, TRUE)),
      expr_list
    ),
    "geom_point"
  )
  expect_identical(result, c(geom_point = TRUE, "geom_point-2" = FALSE))
})

# ---------------------------------------------------------------------------
# Strict validation (Q7)
# ---------------------------------------------------------------------------

test_that("non-list input errors", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_error(
    ptr_resolve_checkbox_defaults(c(geom_point = TRUE), expr_list),
    "named list"
  )
  expect_error(
    ptr_resolve_checkbox_defaults("nope", expr_list),
    "named list"
  )
})

test_that("unnamed list errors", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_error(
    ptr_resolve_checkbox_defaults(list(TRUE, FALSE), expr_list),
    "named"
  )
})

test_that("partially-named list errors", {
  expr_list <- make_expr_list("ggplot", "geom_point", "geom_smooth")
  expect_error(
    ptr_resolve_checkbox_defaults(list(geom_point = TRUE, FALSE), expr_list),
    "named"
  )
})

test_that("duplicate names in input list errors", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_error(
    ptr_resolve_checkbox_defaults(
      list(geom_point = TRUE, geom_point = FALSE),
      expr_list
    ),
    "duplicate"
  )
})

test_that("non-logical value errors", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_error(
    ptr_resolve_checkbox_defaults(list(geom_point = "off"), expr_list),
    "logical"
  )
  expect_error(
    ptr_resolve_checkbox_defaults(list(geom_point = 0), expr_list),
    "logical"
  )
  expect_error(
    ptr_resolve_checkbox_defaults(list(geom_point = 1L), expr_list),
    "logical"
  )
})

test_that("NA value errors", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_error(
    ptr_resolve_checkbox_defaults(list(geom_point = NA), expr_list),
    "NA"
  )
  expect_error(
    ptr_resolve_checkbox_defaults(
      list(geom_point = c(TRUE, NA)),
      expr_list
    ),
    "NA"
  )
})

test_that("length-0 value errors", {
  expr_list <- make_expr_list("ggplot", "geom_point")
  expect_error(
    ptr_resolve_checkbox_defaults(list(geom_point = logical(0)), expr_list),
    "length"
  )
})

# ---------------------------------------------------------------------------
# Integration: ptr_server_state stores resolved vector on ptr_state
# ---------------------------------------------------------------------------

test_that("ptr_server_state exposes resolved vector on ptr_state$checkbox_defaults", {
  state <- ptr_server_state(
    formula = "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + geom_smooth()",
    checkbox_defaults = list(geom_smooth = FALSE)
  )
  expect_identical(
    state$checkbox_defaults,
    c(geom_point = TRUE, geom_smooth = FALSE)
  )
})

test_that("ptr_server_state default (NULL) yields all-TRUE", {
  state <- ptr_server_state(
    formula = "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point() + geom_smooth()"
  )
  expect_identical(
    state$checkbox_defaults,
    c(geom_point = TRUE, geom_smooth = TRUE)
  )
})

test_that("ptr_server_state errors are surfaced from validator", {
  expect_error(
    ptr_server_state(
      formula = "ggplot(data = mtcars, aes(x = var, y = var)) + geom_point()",
      checkbox_defaults = list(geom_point = "off")
    ),
    "logical"
  )
})

# ---------------------------------------------------------------------------
# Render path: ui_insert_checkbox honors checkbox_defaults
# ---------------------------------------------------------------------------

extract_checkbox_value <- function(tag) {
  # checkboxInput renders a div with a child input whose `checked` attr
  # is "checked" when value=TRUE and absent (or NULL) when value=FALSE.
  html <- as.character(tag)
  grepl("checked=\"checked\"", html, fixed = TRUE)
}

test_that("ui_insert_checkbox emits value=FALSE when checkbox_defaults says so", {
  ui <- list()
  out <- ui_insert_checkbox(
    ui, "geom_point",
    ui_text = NULL,
    ns_fn = shiny::NS(NULL),
    checkbox_defaults = c(geom_point = FALSE)
  )
  expect_false(extract_checkbox_value(out[[1]]))
})

test_that("ui_insert_checkbox emits value=TRUE when key absent", {
  ui <- list()
  out <- ui_insert_checkbox(
    ui, "geom_point",
    ui_text = NULL,
    ns_fn = shiny::NS(NULL),
    checkbox_defaults = c(geom_smooth = FALSE)
  )
  expect_true(extract_checkbox_value(out[[1]]))
})

test_that("ui_insert_checkbox emits value=TRUE when checkbox_defaults is NULL", {
  ui <- list()
  out <- ui_insert_checkbox(
    ui, "geom_point",
    ui_text = NULL,
    ns_fn = shiny::NS(NULL),
    checkbox_defaults = NULL
  )
  expect_true(extract_checkbox_value(out[[1]]))
})

test_that("ggplot layer never gets a checkbox regardless of checkbox_defaults", {
  ui <- list("dummy" = "x")
  out <- ui_insert_checkbox(
    ui, "ggplot",
    ui_text = NULL,
    ns_fn = shiny::NS(NULL),
    checkbox_defaults = c(ggplot = FALSE)
  )
  expect_identical(out, list("dummy" = "x"))
})
