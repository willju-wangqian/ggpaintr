# ADR 0020 PLAN-03 — `ptr_tree_structural_equal()` excludes the two new
# UI-state-only slots (`default_active`, `default_stage_enabled`) so that
# trees differing only in those fields compare equal — the comparator's
# contract: "do these two trees compute the same plot?"
#
# Also asserts that `tests/testthat/test-adr12-consumer-uniformity.R` no
# longer carries an inline `ptr_tree_structural_equal()` definition and
# instead delegates to the real `ggpaintr:::ptr_tree_structural_equal()`
# (drift-audit-D6 invariant from the plan: re-implementations of the
# canonical comparator silently fork the contract).

# Local helper, mirrors Plan 01's `test-pp-off-translate.R`: the AST-
# translation path runs via `ptr_translate(rlang::expr_text(expr))`
# (annotated path). `ptr_translate` itself is string-only.
ptr_translate_annot <- function(expr) {
  ptr_translate(rlang::expr_text(expr))
}

# ---- SC1: exclusion list literal ------------------------------------------

test_that("comparator exclusion list contains the two new UI-state slots", {
  src <- readLines(
    test_path("..", "..", "R", "paintr-nodes.R"),
    warn = FALSE
  )
  body <- paste(src, collapse = "\n")
  # ADR-0021 PLAN-01: exclusion list grew from 4 names to 7 — the new sibling
  # UI-metadata fields `stage_id`, `has_user_control`, `stage_label` are all
  # UI-routing/labelling metadata, not load-bearing execution-AST pieces. The
  # lockdown regex is re-anchored on the new 7-name literal in the same order
  # it appears in the source (prefix-preserved), so accidental drift of the
  # canonical exclusion list still trips this guard.
  expect_match(
    body,
    'setdiff\\(names\\(a\\),\\s*c\\("op",\\s*"expr",\\s*"default_active",\\s*"default_stage_enabled",\\s*"stage_id",\\s*"has_user_control",\\s*"stage_label"\\)\\)',
    fixed = FALSE
  )
  expect_match(
    body,
    'setdiff\\(names\\(b\\),\\s*c\\("op",\\s*"expr",\\s*"default_active",\\s*"default_stage_enabled",\\s*"stage_id",\\s*"has_user_control",\\s*"stage_label"\\)\\)',
    fixed = FALSE
  )
})

# ---- SC2: default_active differs → trees still equal ----------------------

test_that("ptr_tree_structural_equal: trees differing only on default_active compare equal", {
  a_layer <- ggpaintr:::ptr_layer(
    name = "geom_point",
    expr = quote(geom_point()),
    default_active = TRUE
  )
  b_layer <- ggpaintr:::ptr_layer(
    name = "geom_point",
    expr = quote(geom_point()),
    default_active = FALSE
  )
  a_root <- ggpaintr:::ptr_root(layers = list(a_layer))
  b_root <- ggpaintr:::ptr_root(layers = list(b_layer))
  expect_true(ggpaintr:::ptr_tree_structural_equal(a_root, b_root))
})

# ---- SC3: default_stage_enabled differs → trees still equal --------------

test_that("ptr_tree_structural_equal: trees differing only on default_stage_enabled compare equal", {
  a_call <- ggpaintr:::ptr_call(
    fun = quote(mutate),
    args = list(),
    expr = quote(mutate()),
    default_stage_enabled = TRUE
  )
  b_call <- ggpaintr:::ptr_call(
    fun = quote(mutate),
    args = list(),
    expr = quote(mutate()),
    default_stage_enabled = FALSE
  )
  a_pipe <- ggpaintr:::ptr_pipeline(
    stages = list(a_call), op = "|>", expr = NULL
  )
  b_pipe <- ggpaintr:::ptr_pipeline(
    stages = list(b_call), op = "|>", expr = NULL
  )
  expect_true(ggpaintr:::ptr_tree_structural_equal(a_pipe, b_pipe))
})

# ---- SC4: non-excluded fields still drive inequality ---------------------

test_that("ptr_tree_structural_equal: trees differing on `name` compare unequal", {
  a_layer <- ggpaintr:::ptr_layer(
    name = "geom_point", expr = quote(geom_point())
  )
  b_layer <- ggpaintr:::ptr_layer(
    name = "geom_line", expr = quote(geom_point())
  )
  expect_false(ggpaintr:::ptr_tree_structural_equal(a_layer, b_layer))
})

test_that("ptr_tree_structural_equal: trees differing on `fun` compare unequal", {
  a_call <- ggpaintr:::ptr_call(
    fun = quote(mutate), args = list(), expr = quote(mutate())
  )
  b_call <- ggpaintr:::ptr_call(
    fun = quote(filter), args = list(), expr = quote(mutate())
  )
  expect_false(ggpaintr:::ptr_tree_structural_equal(a_call, b_call))
})

# ---- SC5: ppLayerOff(..., FALSE) ≡ bare layer (translate level) ----------
# Note: the plan's BDD names `ptr_translate(quote(...))` but the
# canonical AST-translation entrypoint in this codebase is
# `ptr_translate_annot()` (see Plan 01's test-pp-off-translate.R for the
# identical pattern). `ptr_translate()` is string-only. The Then clause
# ("tree-structural equality on these forms") is preserved verbatim.

test_that("ppLayerOff(..., FALSE) is structurally equal to the bare layer", {
  t_off <- ptr_translate_annot(quote(ggplot() + ppLayerOff(geom_point(), FALSE)))
  t_bare <- ptr_translate_annot(quote(ggplot() + geom_point()))
  expect_true(ggpaintr:::ptr_tree_structural_equal(t_off, t_bare))
})

# ---- SC6: ppLayerOff(..., TRUE) ≡ bare layer (NEW capability) ------------

test_that("ppLayerOff(..., TRUE) is structurally equal to the bare layer (default_active excluded)", {
  t_off <- ptr_translate_annot(quote(ggplot() + ppLayerOff(geom_point(), TRUE)))
  t_bare <- ptr_translate_annot(quote(ggplot() + geom_point()))
  expect_true(ggpaintr:::ptr_tree_structural_equal(t_off, t_bare))
})

# ---- SC8: test-adr12-consumer-uniformity.R uses the real comparator ------

test_that("test-adr12-consumer-uniformity.R no longer defines its own ptr_tree_structural_equal", {
  src <- readLines(
    test_path("test-adr12-consumer-uniformity.R"),
    warn = FALSE
  )
  inline_def <- grep("^ptr_tree_structural_equal\\s*<-\\s*function", src)
  expect_length(inline_def, 0L)
  uses_real <- grep("ggpaintr:::ptr_tree_structural_equal", src)
  expect_gte(length(uses_real), 1L)
})
