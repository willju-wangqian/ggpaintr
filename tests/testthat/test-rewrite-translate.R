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

test_that("P1.2 magrittr pipe preserves op '%>%' on data_arg", {
  r <- ptr_translate("mtcars %>% ggplot(aes(x = mpg))")
  expect_equal(length(r$layers), 1L)
  expect_equal(r$layers[[1]]$name, "ggplot")
  expect_s3_class(r$layers[[1]]$data_arg, "ptr_pipeline")
  expect_equal(r$layers[[1]]$data_arg$op, "%>%")
})

test_that("P1.3 native pipe preserved as op '|>'", {
  r <- ptr_translate("mtcars |> ggplot(aes(x = mpg))")
  expect_s3_class(r$layers[[1]]$data_arg, "ptr_pipeline")
  expect_equal(r$layers[[1]]$data_arg$op, "|>")
})

test_that("P1.4 mixed pipe chain preserves both ops in tree", {
  r <- ptr_translate("mtcars %>% head(num) |> ggplot(aes(x = mpg))")
  da <- r$layers[[1]]$data_arg
  expect_s3_class(da, "ptr_pipeline")
  expect_equal(da$op, "|>")
  inner <- da$stages[[1]]
  expect_s3_class(inner, "ptr_pipeline")
  expect_equal(inner$op, "%>%")
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
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point() + expr")
  expect_s3_class(r$layers[[3]], "ptr_ph_value")
  expect_equal(r$layers[[3]]$keyword, "expr")
})

test_that("P1.10 quoted 'var' is a string literal, not a placeholder", {
  r <- ptr_translate('ggplot(mtcars, aes(x = "var", y = "var")) + geom_point()')
  ggp <- r$layers[[1]]
  aes_node <- ggp$children[[1]]
  expect_s3_class(aes_node, "ptr_call")
  for (child in aes_node$args) {
    expect_s3_class(child, "ptr_literal")
    expect_type(child$expr, "character")
    expect_equal(child$expr, "var")
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
  r <- ptr_translate('ggplot(aes(x = var(shared = "axis"), y = var(shared = "axis"))) + geom_point()')
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 2L)
  for (c in consumers) {
    expect_equal(c$keyword, "var")
    expect_equal(c$shared, "axis")
  }
})

test_that("P1.17 call-form with no args equals bare-symbol form", {
  r1 <- ptr_translate("ggplot(aes(x = var())) + geom_point()")
  r2 <- ptr_translate("ggplot(aes(x = var)) + geom_point()")
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
    ptr_translate('ggplot(aes(x = var(unknown = "x"))) + geom_point()'),
    "unknown"
  )
})

test_that("P1.19 call-form rejects positional args", {
  expect_error(
    ptr_translate('ggplot(aes(x = var("x_axis"))) + geom_point()'),
    "positional"
  )
})

test_that("P1.20 empty shared string rejected", {
  expect_error(
    ptr_translate('ggplot(aes(x = var(shared = ""))) + geom_point()'),
    "non-empty"
  )
})

test_that("P1.21 comments are dropped by parser; tree has no record", {
  r <- ptr_translate("mtcars |> head(num) |> # trim rows\nggplot(aes(x = mpg))")
  expect_s3_class(r, "ptr_root")
  serialized <- deparse(r$expr, width.cutoff = 500L)
  expect_false(any(grepl("#", serialized)))
})
