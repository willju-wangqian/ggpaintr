# PLAN-05 — Dual-mode renderer (erased vs preserved placeholders).
#
# Tests cover:
#   - Default arg matches existing renderer output on baseline trees.
#   - preserve_placeholders = FALSE === no-arg form on placeholder-bearing trees.
#   - preserve_placeholders = TRUE emits ppX(current_pick) for var/num/text/expr
#     and ppX() when current_pick is absent.
#   - Non-placeholder structure (whitespace, indentation, +-joins, pipe chains)
#     agrees character-for-character across modes.
#   - Shared placeholders carry `shared = "k"` at every occurrence (G2.a).

# ---- helpers --------------------------------------------------------------

# Build a synthetic placeholder node with a hand-set current_pick. We bypass
# the registry-aware substitute pass because PLAN-05's contract is renderer-
# only; PLAN-07/08 wire current_pick during substitution. We construct nodes
# via the public ptr_ph_value / ptr_ph_data_consumer constructors and then
# attach `current_pick` directly (renderer reads node$current_pick).
ph_value <- function(keyword, pick = NULL, shared = NULL,
                     param = "x", expr = rlang::sym("x")) {
  n <- ptr_ph_value(id = NA_character_, keyword = keyword, param = param,
                    expr = expr, shared = shared)
  if (!is.null(pick) || !missing(pick)) n$current_pick <- pick
  n
}

ph_consumer <- function(keyword, pick = NULL, shared = NULL,
                        param = "x", expr = rlang::sym("x")) {
  n <- ptr_ph_data_consumer(id = NA_character_, keyword = keyword, param = param,
                            expr = expr, shared = shared)
  if (!is.null(pick) || !missing(pick)) n$current_pick <- pick
  n
}

# ---- default mode matches no-arg form -------------------------------------

test_that("default arg matches no-arg form on a baseline tree", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(aes(x = mpg, y = hp))")
  expect_equal(ptr_render(r), ptr_render(r, preserve_placeholders = FALSE))
})

test_that("preserve_placeholders = FALSE on a placeholder-bearing tree matches no-arg form", {
  # The translate->substitute->prune->render pipeline replaces placeholders
  # with literal nodes before render is called; preserve = FALSE must remain
  # backward-compatible with that flow.
  r <- ptr_translate("ggplot(mtcars) + geom_point(aes(x = var))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  expect_equal(ptr_render(p), ptr_render(p, preserve_placeholders = FALSE))
})

test_that("preserve_placeholders = FALSE preserves today's expr_text fallback on raw placeholder nodes", {
  # Erase mode on a not-yet-substituted placeholder node falls back to
  # rlang::expr_text(node$expr) -- today's behaviour. The test pins that
  # behaviour as we add the preserve branch.
  n <- ph_value("ppVar", pick = "mpg", expr = quote(ppVar(mpg)))
  expect_equal(ptr_render(n), "ppVar(mpg)")
})

# ---- preserve mode: each placeholder kind ---------------------------------

test_that("preserve mode emits ppVar(mpg) for a consumer placeholder with symbol-or-string pick", {
  n <- ph_consumer("ppVar", pick = "mpg")
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("ppVar(mpg)", out, fixed = TRUE))
})

test_that("preserve mode emits ppVar(`miles per gallon`) for non-syntactic column name", {
  n <- ph_consumer("ppVar", pick = "miles per gallon")
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("ppVar(`miles per gallon`)", out, fixed = TRUE))
})

test_that("preserve mode emits ppText(\"hello\") for string-default placeholder", {
  n <- ph_value("ppText", pick = "hello")
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl('ppText("hello")', out, fixed = TRUE))
})

test_that("preserve mode emits ppNum(5) for numeric-default placeholder", {
  n <- ph_value("ppNum", pick = 5)
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("ppNum(5)", out, fixed = TRUE))
})

test_that("preserve mode emits ppExpr(x + 1) for expression-default placeholder", {
  n <- ph_value("ppExpr", pick = quote(x + 1))
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("ppExpr(x + 1)", out, fixed = TRUE))
})

test_that("preserve mode handles missing current_pick with empty arg list", {
  n <- ph_consumer("ppVar", pick = NULL)
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("ppVar()", out, fixed = TRUE))
})

# ---- legacy keyword vocabulary (var/num/text/expr) ------------------------

test_that("preserve mode emits bareword for legacy `var` keyword", {
  n <- ph_consumer("var", pick = "mpg")
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("var(mpg)", out, fixed = TRUE))
})

test_that("preserve mode emits quoted string for legacy `text` keyword", {
  n <- ph_value("text", pick = "hi")
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl('text("hi")', out, fixed = TRUE))
})

test_that("preserve mode emits literal for legacy `num` keyword", {
  n <- ph_value("num", pick = 7)
  out <- ptr_render(n, preserve_placeholders = TRUE)
  expect_true(grepl("num(7)", out, fixed = TRUE))
})

# ---- shared placeholders (G2.a) -------------------------------------------

test_that("preserve mode emits shared=\"k\" at every occurrence of a shared placeholder", {
  # Two consumer placeholders sharing key "k", both inside one aes() call.
  ph1 <- ph_consumer("ppVar", pick = "mpg", shared = "k")
  ph2 <- ph_consumer("ppVar", pick = "mpg", shared = "k")
  call_node <- ptr_call(
    fun = quote(aes),
    args = list(x = ph1, y = ph2),
    expr = quote(aes(x = ppVar(mpg, shared = "k"), y = ppVar(mpg, shared = "k")))
  )
  out <- ptr_render(call_node, preserve_placeholders = TRUE)
  # Both x= and y= carry the shared annotation.
  expect_true(grepl('x = ppVar(mpg, shared = "k")', out, fixed = TRUE))
  expect_true(grepl('y = ppVar(mpg, shared = "k")', out, fixed = TRUE))
})

# ---- structural agreement across modes ------------------------------------

test_that("non-placeholder structure agrees across modes (whitespace, +-joins, indentation)", {
  # Construct a tree with placeholders embedded; render in both modes, replace
  # every placeholder-call text segment with a token, then compare. They must
  # be identical char-for-char outside the placeholder regions.
  # Use ptr_translate to build a syntactically valid layered tree, then
  # surgically replace specific arg positions with placeholder nodes so the
  # outer scaffold (layers, +-joins, indentation) is real. We then render
  # both modes; preserve emits ppX(...) while erase falls back to expr_text
  # (since these are raw placeholder nodes, not yet substitute_walk'd).
  r <- ptr_translate("ggplot(mtcars) + geom_point(aes(x = mpg), size = 5)")
  ph1 <- ph_consumer("ppVar", pick = "mpg")
  ph2 <- ph_value("ppNum", pick = 5)
  # geom_point layer -> first child is aes(...) call -> replace x arg.
  r$layers[[2]]$children[[1]]$args$x <- ph1
  # geom_point layer -> replace size arg with ppNum placeholder.
  r$layers[[2]]$children$size <- ph2

  out_erase    <- ptr_render(r, preserve_placeholders = FALSE)
  out_preserve <- ptr_render(r, preserve_placeholders = TRUE)

  # Strip the placeholder text segments and confirm the surrounding scaffold
  # matches: both runs must have identical " +\n  " joins and `aes(...)` /
  # `geom_point(...)` shells. We compare by normalising any ppX(...) or any
  # x = <bareword>/<number> to a single marker.
  norm <- function(s) {
    # Erase mode falls back to expr_text(node$expr) on raw placeholder nodes
    # (no upstream substitute pass here), which yields "x" (the synthetic
    # expr we attached). Preserve mode emits "ppVar(mpg)" / "ppNum(5)".
    # Normalise both shapes to a single <PH> marker so the surrounding
    # scaffold (layers, +-joins, aes()/geom_point() shells) is compared
    # character-for-character.
    s <- gsub("pp[A-Za-z]+\\([^)]*\\)", "<PH>", s)
    s <- gsub("x = [A-Za-z_][A-Za-z0-9_.]*", "x = <PH>", s)
    s <- gsub("size = [A-Za-z0-9_.]+", "size = <PH>", s)
    s
  }
  # Both renders must have the same " +\n  " join structure.
  expect_equal(
    length(strsplit(out_erase, " +\n  ", fixed = TRUE)[[1]]),
    length(strsplit(out_preserve, " +\n  ", fixed = TRUE)[[1]])
  )
  # The non-placeholder scaffolding survives normalisation identically.
  expect_equal(norm(out_erase), norm(out_preserve))
})

test_that("pipe chain structure is identical across modes", {
  r <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = var))")
  e <- ptr_render(r, preserve_placeholders = FALSE)
  p <- ptr_render(r, preserve_placeholders = TRUE)
  # Both must split the chain across three pipe segments with identical join.
  expect_equal(length(gregexpr("\\|>", e)[[1]]),
               length(gregexpr("\\|>", p)[[1]]))
  # Erase mode on the raw (un-substituted) tree falls back to expr_text;
  # preserve mode emits ppX call shape. Either way the |> joins match.
  expect_true(grepl("|>", e, fixed = TRUE))
  expect_true(grepl("|>", p, fixed = TRUE))
})

# ---- preserved output is parseable plain R --------------------------------

test_that("preserve mode output is valid R that parses back to a call", {
  n <- ph_value("ppText", pick = "hello")
  out <- ptr_render(n, preserve_placeholders = TRUE)
  parsed <- tryCatch(rlang::parse_expr(out), error = function(e) NULL)
  expect_false(is.null(parsed))
  expect_true(rlang::is_call(parsed))
})
