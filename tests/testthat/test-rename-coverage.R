# PLAN-08 — rename-coverage smoke tests (ADR 0009 §H1). These pin the
# externally observable contract of the breaking rename: the new
# `pp*` keywords are registered, the old `var`/`num`/`text`/`expr`/`upload`
# keywords are unregistered, the exported callables behave as identity
# (or guard for `ppUpload`), and a `ppVar(mpg)` formula round-trips through
# `ptr_translate()` with the default seed pinned on `node$default`.

test_that("ppX keywords are registered; old keywords are not", {
  kws <- ptr_registry_keywords()
  for (kw in c("ppVar", "ppNum", "ppText", "ppExpr", "ppUpload")) {
    expect_true(kw %in% kws, info = kw)
  }
  for (kw in c("var", "num", "text", "expr", "upload")) {
    expect_false(kw %in% kws, info = kw)
  }
})

test_that("ptr_translate accepts ppVar(mpg) and pins the default seed", {
  r <- ptr_translate("ggplot(mtcars, aes(x = ppVar(mpg))) + geom_point()")
  ph <- ggpaintr:::find_nodes(r,
    function(x) ggpaintr:::is_ptr_placeholder(x) && x$keyword == "ppVar"
  )
  expect_equal(length(ph), 1L)
  expect_equal(ph[[1]]$keyword, "ppVar")
  expect_identical(ph[[1]]$default, "mpg")
})

test_that("old `var(mpg)` no longer translates into a placeholder", {
  # After PLAN-08, `var` is no longer a registered keyword, so the
  # parser treats `var(mpg)` as a plain R call (which ggplot2 will
  # evaluate at render time — and fail, since `var` expects a numeric
  # vector). The contract here is that no `ptr_placeholder` node is
  # created from the old name — the rename is hard, not aliased.
  r <- ptr_translate("ggplot(mtcars, aes(x = var(mpg))) + geom_point()")
  ph <- ggpaintr:::find_nodes(r,
    function(x) ggpaintr:::is_ptr_placeholder(x) && x$keyword == "var"
  )
  expect_equal(length(ph), 0L)
})

test_that("ppVar / ppNum / ppText / ppExpr are identity outside ptr_app()", {
  expect_identical(ppNum(5), 5)
  expect_identical(ppText("hello"), "hello")
  expect_identical(ppVar(quote(mpg)), quote(mpg))
  expect_identical(ppExpr(quote(x + 1)), quote(x + 1))
  # Identity inside ggplot2's tidy-eval: wrapped form equals bare form.
  p_bare    <- ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg))
  p_wrapped <- ggplot2::ggplot(mtcars, ggplot2::aes(x = ppVar(mpg)))
  bb_bare    <- ggplot2::ggplot_build(p_bare    + ggplot2::geom_histogram(bins = 10))
  bb_wrapped <- ggplot2::ggplot_build(p_wrapped + ggplot2::geom_histogram(bins = 10))
  expect_equal(bb_wrapped$data[[1]]$x, bb_bare$data[[1]]$x)
})

test_that("ppUpload aborts when called outside ptr_app()", {
  expect_error(ppUpload(), regexp = "ptr_app")
})
