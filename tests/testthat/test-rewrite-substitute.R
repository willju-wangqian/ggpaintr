# P8 — substitute. Walks the typed tree applying input snapshot + shared
# bindings. Each placeholder is replaced by ptr_literal / ptr_user_expr /
# ptr_missing. Layer activation toggles via active_input_id.

# Helpers ---------------------------------------------------------------------

.id_of <- function(r, kw) {
  ph <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == kw)
  ph[[1]]$id
}

# P8.1–P8.4 text -------------------------------------------------------------

test_that("P8.1 text strips matched leading/trailing double quotes", {
  r <- ptr_translate('ggplot(mtcars) + labs(title = text)')
  id <- .id_of(r, "text")
  sub <- ptr_substitute(r, input_snapshot = setNames(list('"hello"'), id))
  lit <- find_nodes(sub, is_ptr_literal)
  values <- unlist(lapply(lit, function(l) if (is.character(l$expr)) l$expr else NA_character_))
  expect_true("hello" %in% values)
})

test_that("P8.2 text strips matched single quotes", {
  r <- ptr_translate('ggplot(mtcars) + labs(title = text)')
  id <- .id_of(r, "text")
  sub <- ptr_substitute(r, input_snapshot = setNames(list("'hello'"), id))
  lit <- find_nodes(sub, is_ptr_literal)
  values <- unlist(lapply(lit, function(l) if (is.character(l$expr)) l$expr else NA_character_))
  expect_true("hello" %in% values)
})

test_that("P8.3 text leaves unbalanced quotes alone", {
  r <- ptr_translate('ggplot(mtcars) + labs(title = text)')
  id <- .id_of(r, "text")
  sub <- ptr_substitute(r, input_snapshot = setNames(list('"hello'), id))
  lit <- find_nodes(sub, is_ptr_literal)
  values <- unlist(lapply(lit, function(l) if (is.character(l$expr)) l$expr else NA_character_))
  expect_true('"hello' %in% values)
})

test_that("P8.4 text leaves bare text unchanged", {
  r <- ptr_translate('ggplot(mtcars) + labs(title = text)')
  id <- .id_of(r, "text")
  sub <- ptr_substitute(r, input_snapshot = setNames(list("hello"), id))
  lit <- find_nodes(sub, is_ptr_literal)
  values <- unlist(lapply(lit, function(l) if (is.character(l$expr)) l$expr else NA_character_))
  expect_true("hello" %in% values)
})

# P8.5–P8.8 num --------------------------------------------------------------

test_that("P8.5 num returns ptr_missing for NULL input", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  id <- .id_of(r, "num")
  sub <- ptr_substitute(r, input_snapshot = setNames(list(NULL), id))
  expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
  expect_equal(length(find_nodes(sub, is_ptr_ph_value)), 0L)
})

test_that("P8.6 num returns ptr_missing for NA", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  id <- .id_of(r, "num")
  sub <- ptr_substitute(r, input_snapshot = setNames(list(NA_real_), id))
  expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
})

test_that("P8.7 num returns ptr_missing for empty inputs", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  id <- .id_of(r, "num")
  for (val in list(numeric(0), character(0), "")) {
    sub <- ptr_substitute(r, input_snapshot = setNames(list(val), id))
    expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
  }
})

test_that("P8.8 num returns the literal for valid input", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  id <- .id_of(r, "num")
  sub <- ptr_substitute(r, input_snapshot = setNames(list(5), id))
  lits <- find_nodes(sub, is_ptr_literal)
  expect_true(any(vapply(lits, function(l) is.numeric(l$expr) && length(l$expr) == 1L && l$expr == 5, logical(1))))
})

# P8.9–P8.11 expr ------------------------------------------------------------

test_that("P8.9 expr returns ptr_user_expr(parsed) for valid input", {
  r <- ptr_translate("ggplot(mtcars) + expr")
  id <- .id_of(r, "expr")
  sub <- ptr_substitute(r, input_snapshot = setNames(list("geom_smooth(method = 'lm')"), id))
  ues <- find_nodes(sub, is_ptr_user_expr)
  expect_equal(length(ues), 1L)
  expect_true(is.call(ues[[1]]$inner))
  expect_equal(as.character(ues[[1]]$inner[[1]]), "geom_smooth")
})

test_that("P8.10 expr returns ptr_missing for empty string", {
  r <- ptr_translate("ggplot(mtcars) + expr")
  id <- .id_of(r, "expr")
  sub <- ptr_substitute(r, input_snapshot = setNames(list(""), id))
  expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
})

test_that("P8.11 expr rejects multi-expression input", {
  r <- ptr_translate("ggplot(mtcars) + expr")
  id <- .id_of(r, "expr")
  expect_error(
    ptr_substitute(r, input_snapshot = setNames(list("geom_smooth()\nlabs()"), id)),
    "exactly one expression"
  )
})

# P8.12–P8.14 var ------------------------------------------------------------

test_that("P8.12 var returns symbol for valid column choice", {
  r <- ptr_translate("ggplot(mtcars, aes(x = var))")
  id <- .id_of(r, "var")
  sub <- ptr_substitute(r, input_snapshot = setNames(list("mpg"), id))
  lits <- find_nodes(sub, is_ptr_literal)
  expect_true(any(vapply(lits, function(l) is.symbol(l$expr) && as.character(l$expr) == "mpg", logical(1))))
})

test_that("P8.13 var aborts on column not in upstream", {
  r <- ptr_translate("ggplot(mtcars, aes(x = var))")
  id <- .id_of(r, "var")
  expect_error(
    ptr_substitute(
      r,
      input_snapshot = stats::setNames(list("cyl"), id),
      upstream_cols = stats::setNames(list(c("mpg")), id)
    ),
    "not in upstream"
  )
})

test_that("P8.14 var aborts on multiple selected columns", {
  # Arity is now policed by `var`'s own resolve_expr (the framework no
  # longer rejects multi-value consumer inputs — that allowed legacy
  # multi-column patterns like a `colvars` selectInput to resolve to
  # `c(...)`).
  r <- ptr_translate("ggplot(mtcars, aes(x = var))")
  id <- .id_of(r, "var")
  expect_error(
    ptr_substitute(r, input_snapshot = setNames(list(c("mpg", "hp")), id)),
    "single column name"
  )
})

# P8.15–P8.17 upload ---------------------------------------------------------

test_that("P8.15 upload returns symbol for valid name (via companion id)", {
  r <- ptr_translate("ggplot(data = upload)")
  src <- find_nodes(r, is_ptr_ph_data_source)[[1]]
  sub <- ptr_substitute(r, input_snapshot = setNames(list("my_data"), src$companion_id))
  lits <- find_nodes(sub, is_ptr_literal)
  expect_true(any(vapply(lits, function(l) is.symbol(l$expr) && as.character(l$expr) == "my_data", logical(1))))
})

test_that("P8.16 upload aborts on injection attempt (invalid R name)", {
  r <- ptr_translate("ggplot(data = upload)")
  src <- find_nodes(r, is_ptr_ph_data_source)[[1]]
  expect_error(
    ptr_substitute(r, input_snapshot = setNames(list("x; system('id')"), src$companion_id)),
    "valid R name"
  )
})

test_that("P8.17 upload returns ptr_missing for empty companion name", {
  r <- ptr_translate("ggplot(data = upload)")
  src <- find_nodes(r, is_ptr_ph_data_source)[[1]]
  for (val in list("", NULL)) {
    sub <- ptr_substitute(r, input_snapshot = setNames(list(val), src$companion_id))
    expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
  }
})

# P8.18 expr provenance, P8.20–P8.22 -----------------------------------------

test_that("P8.18 expr placeholder produces ptr_user_expr provenance", {
  r <- ptr_translate("ggplot(mtcars) + expr")
  id <- .id_of(r, "expr")
  sub <- ptr_substitute(r, input_snapshot = setNames(list("theme()"), id))
  ues <- find_nodes(sub, is_ptr_user_expr)
  expect_equal(length(ues), 1L)
})

test_that("P8.20/P8.21 custom resolve_expr return-type whitelist enforced", {
  ptr_define_placeholder_value(
    keyword = "badret",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) new.env()
  )
  on.exit(suppressWarnings(ptr_register_builtins()))
  r <- ptr_translate("ggplot(mtcars) + labs(title = badret)")
  id <- .id_of(r, "badret")
  expect_error(
    ptr_substitute(r, input_snapshot = setNames(list("anything"), id)),
    "unsupported type"
  )
})

test_that("P8.22 ptr_missing propagates positionally (without prune)", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  sub <- ptr_substitute(r, input_snapshot = list())
  expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
})

test_that("P8.23 mixed NULL and non-NULL inputs", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num, color = text)")
  num_id <- .id_of(r, "num")
  text_id <- .id_of(r, "text")
  sub <- ptr_substitute(r, input_snapshot = setNames(list(5, NULL), c(num_id, text_id)))
  lits <- find_nodes(sub, is_ptr_literal)
  expect_true(any(vapply(lits, function(l) is.numeric(l$expr) && length(l$expr) == 1L && l$expr == 5, logical(1))))
  expect_true(length(find_nodes(sub, is_ptr_missing)) >= 1L)
})

test_that("P8.24 shared bindings override snapshot", {
  r <- ptr_translate('ggplot(mtcars) + geom_point(size = num(shared = "size"))')
  sub <- ptr_substitute(
    r,
    input_snapshot = list(),
    shared_bindings = list(size = function() 7)
  )
  lits <- find_nodes(sub, is_ptr_literal)
  expect_true(any(vapply(lits, function(l) is.numeric(l$expr) && length(l$expr) == 1L && l$expr == 7, logical(1))))
})

test_that("P8.26 empty `text` input prunes to ptr_missing (named arg drops)", {
  # `labs(title = text, x = text, y = text)` with all three text inputs
  # blank should collapse to nothing, not `labs(title = "", x = "", y = "")`.
  r <- ptr_translate("ggplot(mtcars) + geom_point() + labs(title = text, x = text, y = text)")
  ph_ids <- vapply(
    find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "text"),
    function(p) p$id, character(1)
  )
  snap <- stats::setNames(as.list(rep("", length(ph_ids))), ph_ids)
  pruned <- ptr_prune(ptr_substitute(r, input_snapshot = snap))
  rendered <- ptr_render(pruned)
  expect_false(grepl("labs\\(", rendered))
  expect_false(grepl('title = ""', rendered, fixed = TRUE))
})

test_that("P8.27 logical NA from numericInput prunes a `num` placeholder", {
  # numericInput emits logical `NA` (not NA_real_) when blank; the
  # earlier `is.numeric(value) && all(is.na(value))` check missed this
  # because is.numeric(NA) is FALSE.
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  num_id <- .id_of(r, "num")
  snap <- stats::setNames(list(NA), num_id)  # logical NA
  pruned <- ptr_prune(ptr_substitute(r, input_snapshot = snap))
  rendered <- ptr_render(pruned)
  expect_false(grepl("size = ", rendered, fixed = TRUE))
  expect_false(grepl("NA_real_", rendered, fixed = TRUE))
})

test_that("non-numeric string in `num` snapshot prunes the arg", {
  # Adversarial path: programmatic `Shiny.setInputValue("...num", "abc")`
  # bypasses numericInput's client-side validation. as.numeric("abc") is
  # NA_real_ with a warning; resolve_expr returns NULL so substitute drops
  # the arg rather than rendering `size = NA_real_`.
  r <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  num_id <- .id_of(r, "num")
  snap <- stats::setNames(list("abc"), num_id)
  pruned <- ptr_prune(ptr_substitute(r, input_snapshot = snap))
  rendered <- ptr_render(pruned)
  expect_false(grepl("size = ", rendered, fixed = TRUE))
  expect_false(grepl("NA_real_", rendered, fixed = TRUE))
})

test_that("P8.25 P5 re-screens denied calls returned by resolve_expr", {
  # Spec L98: when `resolve_expr` returns a language object that contains a
  # denied call, P5 must catch it at substitute time. Translate-time P5 saw
  # only the formula with the placeholder; the user-typed `expr` value is
  # parsed and inserted by `ptr_builtin_expr_resolve_expr`, so the denylist
  # walker has to run again before the result enters the tree.
  r <- ptr_translate("ggplot(mtcars) + geom_point() + facet_wrap(expr)")
  expr_id <- .id_of(r, "expr")
  expect_error(
    ptr_substitute(r, input_snapshot = setNames(list("system('rm -rf /')"),
                                                expr_id)),
    "system"
  )
})
