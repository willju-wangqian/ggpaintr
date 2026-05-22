# P1 — translate. One test_that per BDD scenario in core-rewrite-bdd.md.

test_that("P1.1 simple ggplot+geom_point produces two layers, no pipeline", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point()")
  expect_s3_class(r, "ptr_root")
  expect_equal(length(r$layers), 2L)
  expect_equal(vapply(r$layers, function(l) l$name, character(1)),
               c("ggplot", "geom_point"))
  expect_s3_class(r$layers[[1]]$data_arg, "ptr_literal")
  expect_identical(r$layers[[1]]$data_arg$expr, quote(mtcars))
  walk_root <- function(node, found = FALSE) {
    if (is_ptr_pipeline(node)) return(TRUE)
    if (is.list(node) && !is_ptr_node(node)) {
      for (x in node) found <- walk_root(x, found) || found
    } else if (is_ptr_node(node)) {
      for (nm in names(node)) found <- walk_root(node[[nm]], found) || found
    }
    found
  }
  expect_false(walk_root(r))
})

test_that("P1.2 single-stage `%>%` chain canonicalises to a non-pipeline data_arg", {
  # PLAN-02 (ADR 0012 §1): pipes are pure first-arg-insertion sugar.
  # `mtcars %>% ggplot(...)` desugars to `ggplot(mtcars, ...)`, a one-call
  # chain — the lift rejects single-stage chains (GATE 0). The resulting
  # data_arg is the bare-symbol source, not a 1-stage pipeline.
  r <- ptr_translate("mtcars %>% ggplot(aes(x = mpg))")
  expect_equal(length(r$layers), 1L)
  expect_equal(r$layers[[1]]$name, "ggplot")
  expect_s3_class(r$layers[[1]]$data_arg, "ptr_literal")
  expect_identical(r$layers[[1]]$data_arg$expr, quote(mtcars))
})

test_that("P1.3 single-stage `|>` chain canonicalises to a non-pipeline data_arg", {
  # `|>` and `%>%` now produce structurally-identical trees (ADR 0012 §1).
  r <- ptr_translate("mtcars |> ggplot(aes(x = mpg))")
  expect_s3_class(r$layers[[1]]$data_arg, "ptr_literal")
  expect_identical(r$layers[[1]]$data_arg$expr, quote(mtcars))
})

test_that("P1.4 multi-stage mixed-pipe chain lifts to one canonical `|>` pipeline", {
  # The desugar pass collapses `%>%` AND `|>` to nested-call form before the
  # lift runs, so the user's original pipe-operator choice is no longer
  # recoverable from the tree (ADR §5 OQ2 — deferred). Every lifted
  # pipeline carries `$op = "|>"` regardless of surface form. There are NO
  # nested ptr_pipeline nodes — the always-aggressive descent produces one
  # flat pipeline (ADR 0012 §1: the tree is semantic, not syntactic).
  # The chain depth above the source must be >= 2 for the lift to fire.
  r <- ptr_translate(
    "mtcars %>% head(ppNum) %>% subset(mpg > 0) |> ggplot(aes(x = mpg))"
  )
  da <- r$layers[[1]]$data_arg
  expect_s3_class(da, "ptr_pipeline")
  expect_equal(da$op, "|>")
  # 3 stages: source (mtcars) + head(ppNum) + subset(mpg > 0).
  expect_equal(length(da$stages), 3L)
  expect_s3_class(da$stages[[1L]], "ptr_literal")
  expect_identical(da$stages[[1L]]$expr, quote(mtcars))
  expect_s3_class(da$stages[[2L]], "ptr_call")
  expect_s3_class(da$stages[[3L]], "ptr_call")
  # No nested ptr_pipeline — always-aggressive descent flattens.
  expect_false(any(vapply(da$stages, is_ptr_pipeline, logical(1))))
})

test_that("P1.5 top-level + split into ordered layers", {
  r <- ptr_translate("ggplot(mtcars) + geom_point() + geom_smooth() + labs(title = 'x')")
  expect_equal(length(r$layers), 4L)
  expect_equal(vapply(r$layers, function(l) l$name, character(1)),
               c("ggplot", "geom_point", "geom_smooth", "labs"))
})

test_that("P1.6 duplicate layer names get -2/-3 suffixes", {
  r <- ptr_translate("ggplot(mtcars) + geom_point() + geom_point() + geom_point()")
  expect_equal(vapply(r$layers, function(l) l$name, character(1)),
               c("ggplot", "geom_point", "geom_point-2", "geom_point-3"))
})

test_that("P1.7 pkg::fn head parses with bare layer name", {
  r <- ptr_translate("ggplot2::ggplot(mtcars) + ggplot2::geom_point()")
  expect_equal(vapply(r$layers, function(l) l$name, character(1)),
               c("ggplot", "geom_point"))
  expect_true(is.call(r$layers[[1]]$expr))
  head1 <- r$layers[[1]]$expr[[1]]
  expect_true(is.call(head1) && identical(head1[[1]], quote(`::`)))
})

test_that("P1.8 pkg:::fn head parses with bare layer name; ::: preserved", {
  r <- ptr_translate("pkg:::internal_fn() + geom_point()")
  expect_equal(r$layers[[1]]$name, "internal_fn")
  head1 <- r$layers[[1]]$expr[[1]]
  expect_true(is.call(head1) && identical(head1[[1]], quote(`:::`)))
})

test_that("P1.9 bare-symbol expr at trailing layer is ptr_ph_value", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point() + ppExpr")
  expect_s3_class(r$layers[[3]], "ptr_ph_value")
  expect_equal(r$layers[[3]]$keyword, "ppExpr")
})

test_that("P1.10 quoted 'ppVar' is a string literal, not a placeholder", {
  r <- ptr_translate('ggplot(mtcars, aes(x = "ppVar", y = "ppVar")) + geom_point()')
  ggp <- r$layers[[1]]
  aes_node <- ggp$children[[1]]
  expect_s3_class(aes_node, "ptr_call")
  for (child in aes_node$args) {
    expect_s3_class(child, "ptr_literal")
    expect_type(child$expr, "character")
    expect_equal(child$expr, "ppVar")
  }
})

test_that("P1.11 single-layer formula without + is one layer", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg, y = hp))")
  expect_equal(length(r$layers), 1L)
  expect_equal(r$layers[[1]]$name, "ggplot")
})

test_that("P1.12 multi-expression formulas rejected", {
  expect_error(ptr_translate("ggplot(mtcars)\nggplot(iris)"),
               "must be a single ggplot expression")
  expect_error(ptr_translate("ggplot(mtcars); ggplot(iris)"),
               "must be a single ggplot expression")
})

test_that("P1.13 empty/whitespace formulas rejected", {
  expect_error(ptr_translate(""), "empty or whitespace")
  expect_error(ptr_translate(" "), "empty or whitespace")
  expect_error(ptr_translate("\n"), "empty or whitespace")
  expect_error(ptr_translate("\t\n"), "empty or whitespace")
})

test_that("P1.14 depth limit triggers abort", {
  build_nested <- function(n) {
    s <- "x"
    for (i in seq_len(n)) s <- paste0("f(", s, ")")
    s
  }
  expect_error(ptr_translate(build_nested(15), max_depth = 10L),
               "nested too deeply")
  expect_error(ptr_translate(build_nested(11), max_depth = 10L),
               "nested too deeply")
  expect_silent(ptr_translate(build_nested(10), max_depth = 10L,
                              expr_check = FALSE))
})

test_that("P1.15 type guards reject non-string formula args", {
  expect_error(ptr_translate(NULL))
  expect_error(ptr_translate(123))
  expect_error(ptr_translate(c("a", "b")))
  expect_error(ptr_translate(quote(x)))
})

test_that("P1.16 call-form var with shared parses to consumer with shared key", {
  r <- ptr_translate('ggplot(aes(x = ppVar(shared = "axis"), y = ppVar(shared = "axis"))) + geom_point()')
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 2L)
  for (c in consumers) {
    expect_equal(c$keyword, "ppVar")
    expect_equal(c$shared, "axis")
  }
})

test_that("P1.17 call-form with no args equals bare-symbol form", {
  r1 <- ptr_translate("ggplot(aes(x = ppVar())) + geom_point()")
  r2 <- ptr_translate("ggplot(aes(x = ppVar)) + geom_point()")
  c1 <- find_nodes(r1, is_ptr_ph_data_consumer)
  c2 <- find_nodes(r2, is_ptr_ph_data_consumer)
  expect_equal(length(c1), 1L)
  expect_equal(length(c2), 1L)
  expect_equal(class(c1[[1]])[1], class(c2[[1]])[1])
  expect_null(c1[[1]]$shared)
  expect_null(c2[[1]]$shared)
  expect_equal(c1[[1]]$keyword, c2[[1]]$keyword)
})

test_that("P1.18 call-form rejects unknown args", {
  expect_error(
    ptr_translate('ggplot(aes(x = ppVar(unknown = "x"))) + geom_point()'),
    "unknown"
  )
})

test_that("P1.19 call-form accepts positional default-arg (ADR 0009)", {
  # After PLAN-08, `ppVar` declares a `default_arg =
  # ptr_default_symbol_or_string()` validator, so a positional bareword
  # or string is accepted as the initial seed value. The translated
  # node carries it on `node$default`.
  r <- ptr_translate('ggplot(aes(x = ppVar("x_axis"))) + geom_point()')
  ph <- find_nodes(r, function(x) is_ptr_placeholder(x) && x$keyword == "ppVar")
  expect_equal(length(ph), 1L)
  expect_identical(ph[[1]]$default, "x_axis")
})

test_that("P1.20 empty shared string rejected", {
  expect_error(
    ptr_translate('ggplot(aes(x = ppVar(shared = ""))) + geom_point()'),
    "non-empty"
  )
})

test_that("P1.21 comments are dropped by parser; tree has no record", {
  r <- ptr_translate("mtcars |> head(ppNum) |> # trim rows\nggplot(aes(x = mpg))")
  expect_s3_class(r, "ptr_root")
  serialized <- deparse(r$expr, width.cutoff = 500L)
  expect_false(any(grepl("#", serialized)))
})
