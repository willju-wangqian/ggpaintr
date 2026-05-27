# PLAN-04 — Expression-mode capture at ptr_app / ptr_server.
# One test_that per BDD scenario in
# dev/plans/placeholder-rename-expr-mode/04-expression-mode-capture.html.

# Helper: a structural fingerprint of a translated root that abstracts
# whitespace and the surface pipe form. Captures layer count, each
# layer's keyword set (placeholder ids per layer), per-layer call name,
# and placeholder ids in document order.
translated_fingerprint <- function(root) {
  layer_names <- vapply(root$layers, function(l) l$name, character(1))
  per_layer_ids <- lapply(root$layers, function(l) {
    ids <- character()
    walk <- function(node) {
      if (is.list(node)) {
        if (!is.null(node$id) && is.character(node$id) &&
            length(node$id) == 1L) {
          ids <<- c(ids, node$id)
        }
        for (nm in names(node)) walk(node[[nm]])
        if (is.null(names(node))) for (x in node) walk(x)
      }
    }
    walk(l)
    sort(unique(ids))
  })
  list(
    layer_count = length(root$layers),
    layer_names = layer_names,
    per_layer_ids = per_layer_ids
  )
}

test_that("string-mode unchanged: dispatch is a no-op for string scalars", {
  formula_str <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  expect_identical(
    ggpaintr:::ptr_capture_formula(formula_str, parent.frame()),
    formula_str
  )
  # Translating the string mode directly matches translating after dispatch.
  direct <- ptr_translate(formula_str)
  dispatched <- ptr_translate(
    ggpaintr:::ptr_capture_formula(formula_str, parent.frame())
  )
  expect_identical(translated_fingerprint(direct),
                   translated_fingerprint(dispatched))
})

test_that("expression-mode produces an equivalent translated AST", {
  formula_str <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  expr_captured <- rlang::expr(
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  )
  str_from_expr <- ggpaintr:::ptr_capture_formula(
    expr_captured, parent.frame()
  )
  expect_type(str_from_expr, "character")
  expect_length(str_from_expr, 1L)

  root_str <- ptr_translate(formula_str)
  root_expr <- ptr_translate(str_from_expr)
  fp_str <- translated_fingerprint(root_str)
  fp_expr <- translated_fingerprint(root_expr)

  expect_equal(fp_expr$layer_count, fp_str$layer_count)
  expect_equal(fp_expr$layer_names, fp_str$layer_names)
  expect_equal(fp_expr$per_layer_ids, fp_str$per_layer_ids)
})

test_that("symbol holding a string is resolved in envir", {
  f <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  resolved <- ggpaintr:::ptr_capture_formula(quote(f), environment())
  expect_identical(resolved, f)
})

test_that("pre-quoted wrappers are unwrapped one layer (E1.a)", {
  # rlang::expr() at the captured root unwraps.
  wrapped_expr <- rlang::expr(rlang::expr(
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  ))
  out_expr <- ggpaintr:::ptr_capture_formula(wrapped_expr, parent.frame())
  expect_type(out_expr, "character")
  expect_match(out_expr, "ggplot", fixed = TRUE)
  expect_no_match(out_expr, "rlang::expr", fixed = TRUE)

  # quote() at the captured root unwraps.
  wrapped_q <- rlang::expr(quote(
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  ))
  out_q <- ggpaintr:::ptr_capture_formula(wrapped_q, parent.frame())
  expect_match(out_q, "ggplot", fixed = TRUE)
  expect_no_match(out_q, "^quote\\(")

  # bquote() at the captured root unwraps.
  wrapped_bq <- rlang::expr(bquote(
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  ))
  out_bq <- ggpaintr:::ptr_capture_formula(wrapped_bq, parent.frame())
  expect_match(out_bq, "ggplot", fixed = TRUE)
  expect_no_match(out_bq, "^bquote\\(")
})

test_that("top-level `{ ... }` is unwrapped so `|> ptr_app()` works", {
  # `{ expr } |> ptr_app()` is the motivating shape: braces let the
  # user pipe an entire built-up ggplot expression into `ptr_app()`
  # at the bottom. `ptr_capture_formula` unwraps `{` and returns the
  # last sub-expression as a string.
  wrapped_brace <- rlang::expr({
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  })
  out_brace <- ggpaintr:::ptr_capture_formula(wrapped_brace, parent.frame())
  expect_type(out_brace, "character")
  expect_length(out_brace, 1L)
  expect_match(out_brace, "ggplot", fixed = TRUE)
  expect_no_match(out_brace, "^\\{")

  # Multi-statement block: keep only the last expression (R's `{}`
  # semantics). Earlier statements (helpers / temporaries) are dropped.
  wrapped_multi <- rlang::expr({
    x <- 1
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  })
  out_multi <- ggpaintr:::ptr_capture_formula(wrapped_multi, parent.frame())
  expect_match(out_multi, "ggplot", fixed = TRUE)
  expect_no_match(out_multi, "x <- 1", fixed = TRUE)

  # End-to-end through `ptr_app()`: the braced + piped form builds
  # a shinyApp just like the bare expression form.
  app_brace <- ptr_app({
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  })
  expect_s3_class(app_brace, "shiny.appobj")
})

test_that("invalid input is rejected with a clear message", {
  expect_error(
    ggpaintr:::ptr_capture_formula(NULL, parent.frame()),
    "single string or .*unquoted ggplot expression"
  )
  expect_error(
    ggpaintr:::ptr_capture_formula(42, parent.frame()),
    "single string or .*unquoted ggplot expression"
  )
  expect_error(
    ggpaintr:::ptr_capture_formula(c("a", "b"), parent.frame()),
    "single string or .*unquoted ggplot expression"
  )
  # Unresolved symbol surfaces the dedicated message naming the variable.
  expect_error(
    ggpaintr:::ptr_capture_formula(
      quote(unbound_formula_xyz), new.env(parent = emptyenv())
    ),
    "unbound_formula_xyz"
  )
})

test_that("ptr_app accepts both modes and rejects invalid input end-to-end", {
  f <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  # String mode end-to-end (smoke): builds a shinyApp object.
  app_str <- ptr_app(f)
  expect_s3_class(app_str, "shiny.appobj")
  # Expression mode end-to-end (smoke).
  app_expr <- ptr_app(
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  )
  expect_s3_class(app_expr, "shiny.appobj")
  # Symbol-holding-string mode (f is bound in the local test frame).
  app_sym <- ptr_app(f)
  expect_s3_class(app_sym, "shiny.appobj")
  # Invalid scalar / NULL / vector all abort at the boundary.
  expect_error(
    ptr_app(42),
    "single string or .*unquoted ggplot expression"
  )
  expect_error(
    ptr_app(NULL),
    "single string or .*unquoted ggplot expression"
  )
  expect_error(
    ptr_app(c("a", "b")),
    "single string or .*unquoted ggplot expression"
  )
})

test_that("ptr_server rejects invalid input at the public boundary", {
  # `ptr_server()` immediately calls `shiny::moduleServer()`, which
  # requires an active Shiny session; both well-formed modes therefore
  # error downstream with "session must be a ShinySession ...". What
  # this plan owns is the public-boundary dispatch: invalid `formula`
  # must abort with the dispatch's own message *before* moduleServer
  # is reached.
  expect_error(
    ptr_server(42, "p"),
    "single string or .*unquoted ggplot expression"
  )
  expect_error(
    ptr_server(NULL, "p"),
    "single string or .*unquoted ggplot expression"
  )
  # Well-formed inputs make it past the dispatch and fail on the
  # session check (proves the dispatch accepted both modes).
  expect_error(
    ptr_server(
      "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()", "p"
    ),
    "session must be"
  )
  expect_error(
    ptr_server(
      ggplot(mtcars, aes(x = var, y = var)) + geom_point(), "p"
    ),
    "session must be"
  )
})

test_that("ptr_app_components accepts both modes", {
  f <- "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()"
  parts_str <- ggpaintr:::ptr_app_components(f)
  parts_expr <- ggpaintr:::ptr_app_components(
    ggplot(mtcars, aes(x = var, y = var)) + geom_point()
  )
  expect_named(parts_str, c("ui", "server"))
  expect_named(parts_expr, c("ui", "server"))
})

test_that("native pipe in string mode survives preprocessing", {
  formula_str <- "mtcars |> ggplot(aes(x = ppVar)) + geom_histogram()"
  expect_error(ptr_translate(formula_str), NA)
})

test_that("native pipe in expression mode is desugared before capture", {
  expr_captured <- rlang::expr(
    mtcars |> ggplot(aes(x = var)) + geom_histogram()
  )
  out <- ggpaintr:::ptr_capture_formula(expr_captured, parent.frame())
  expect_type(out, "character")
  # R's parser desugars `|>` into a nested call shape; the deparsed
  # text shows `ggplot(mtcars, ...)` and no surviving `|>` token.
  expect_match(out, "ggplot(mtcars,", fixed = TRUE)
  expect_no_match(out, "|>", fixed = TRUE)
  expect_error(ptr_translate(out), NA)
})

test_that("!! splicing into expression mode works (rlang::enexpr behaviour)", {
  # Verify enexpr-driven splicing inside a wrapper helper, since
  # ptr_capture_formula receives the already-enexpr-captured node.
  capture_via <- function(formula) {
    ggpaintr:::ptr_capture_formula(rlang::enexpr(formula), parent.frame())
  }
  col <- rlang::sym("mpg")
  out <- capture_via(
    ggplot(mtcars, aes(x = !!col, y = var)) + geom_point()
  )
  expect_match(out, "x = mpg", fixed = TRUE)
})
