# PLAN-02 (ADR 0012) — unit tests for `ptr_tree_structural_equal`, the
# auditor's mechanical equality check that ignores non-structural metadata
# (`$op`, `$expr`) and recurses into list slots / `ptr_node` slots
# element-wise. Used by the canonical-tree identity invariant in
# `test-classify-calls-lift.R`.

test_that("ptr_tree_structural_equal ignores $op", {
  a <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars)), ptr_literal(quote(filter(x > 1)))),
    op = "|>",
    expr = quote(mtcars |> filter(x > 1))
  )
  b <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars)), ptr_literal(quote(filter(x > 1)))),
    op = "%>%",
    expr = quote(mtcars %>% filter(x > 1))
  )
  expect_true(ggpaintr:::ptr_tree_structural_equal(a, b))
})

test_that("ptr_tree_structural_equal ignores $expr on the pipeline node", {
  a <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars))),
    op = "|>",
    expr = quote(mtcars |> filter(x > 1))
  )
  b <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars))),
    op = "|>",
    expr = quote(SOMETHING_ELSE_ENTIRELY)
  )
  expect_true(ggpaintr:::ptr_tree_structural_equal(a, b))
})

test_that("ptr_tree_structural_equal returns FALSE on a stages-list mismatch", {
  a <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars)), ptr_literal(quote(filter(x > 1)))),
    op = "|>",
    expr = quote(mtcars |> filter(x > 1))
  )
  c_other <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars)), ptr_literal(quote(arrange(y)))),
    op = "|>",
    expr = quote(mtcars |> arrange(y))
  )
  expect_false(ggpaintr:::ptr_tree_structural_equal(a, c_other))
})

test_that("ptr_tree_structural_equal returns FALSE when class differs", {
  a <- ptr_literal(quote(x))
  b <- ptr_missing()
  expect_false(ggpaintr:::ptr_tree_structural_equal(a, b))
})

test_that("ptr_tree_structural_equal compares list slots by element name", {
  call_a <- ptr_call(
    fun = quote(filter),
    args = list(a = ptr_literal(1), b = ptr_literal(2)),
    expr = quote(filter(a = 1, b = 2))
  )
  call_b <- ptr_call(
    fun = quote(filter),
    args = list(b = ptr_literal(2), a = ptr_literal(1)),
    expr = quote(filter(b = 2, a = 1))
  )
  # Different argument order => different names() => not structurally equal.
  expect_false(ggpaintr:::ptr_tree_structural_equal(call_a, call_b))
})

test_that("ptr_tree_structural_equal returns TRUE on identical literals", {
  expect_true(ggpaintr:::ptr_tree_structural_equal(
    ptr_literal(quote(x)), ptr_literal(quote(x))
  ))
})

test_that("ptr_tree_structural_equal returns FALSE on differing leaf values", {
  expect_false(ggpaintr:::ptr_tree_structural_equal(
    ptr_literal(1L), ptr_literal(2L)
  ))
})

test_that("ptr_tree_structural_equal handles atomic non-list scalars", {
  expect_true(ggpaintr:::ptr_tree_structural_equal("abc", "abc"))
  expect_false(ggpaintr:::ptr_tree_structural_equal("abc", "xyz"))
  expect_true(ggpaintr:::ptr_tree_structural_equal(quote(x), quote(x)))
})

# Regression: TODO 7. Two trees built by independent ptr_translate() calls
# with structurally identical formulas must compare equal even though their
# `shiny::NS(...)` closures are not `identical()` (each call creates a fresh
# enclosing env). Fixed by dropping `node$ns_fn <- ns_fn` in ptr_assign_ids —
# the namespacing function is plumbed end-to-end as a parameter (UI build)
# and lives on `state$server_ns_fn` / `state$ui_ns_fn` (server), so the root
# slot was redundant.
test_that("ptr_tree_structural_equal: independent trees with NS(NULL) compare equal", {
  formula <- "ggplot(mtcars, aes(x = mpg)) + geom_point()"
  t1 <- ptr_translate(formula, ns_fn = shiny::NS(NULL))
  t2 <- ptr_translate(formula, ns_fn = shiny::NS(NULL))
  expect_true(ggpaintr:::ptr_tree_structural_equal(t1, t2))
})

test_that("ptr_tree_structural_equal: namespace prefix does not affect equality", {
  formula <- "ggplot(mtcars, aes(x = mpg)) + geom_point()"
  t_top <- ptr_translate(formula, ns_fn = shiny::NS(NULL))
  t_mod <- ptr_translate(formula, ns_fn = shiny::NS("mod1"))
  # Raw ids stay on nodes; ns_fn is applied only at emit/binding time, so
  # the canonical tree shape is independent of the chosen namespace.
  expect_true(ggpaintr:::ptr_tree_structural_equal(t_top, t_mod))
})
