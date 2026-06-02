# P9 — prune. Drop missing and empty-after-substitution. One S3 visitor.

test_that("P9.1 positional missing arg escalates the call", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(data = subset(mtcars, mpg >= ppNum))")
  s <- ptr_substitute(r, input_snapshot = list())  # num missing
  p <- ptr_prune(s)
  # geom_point's data subset(mtcars, mpg >= ptr_missing()) should escalate
  # so geom_point ends up with no children. geom_point is standalone-eligible
  # so the layer survives empty.
  layers <- p$layers
  geom_layer <- Filter(function(l) l$name == "geom_point", layers)[[1]]
  expect_equal(length(geom_layer$children), 0L)
})

test_that("P9.2 named missing arg dropped", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(color = ppText, size = 3)")
  s <- ptr_substitute(r, input_snapshot = list())  # text missing
  p <- ptr_prune(s)
  geom <- Filter(function(l) l$name == "geom_point", p$layers)[[1]]
  arg_names <- names(geom$children) %||% rep_len("", length(geom$children))
  expect_false("color" %in% arg_names)
  expect_true("size" %in% arg_names)
})

test_that("P9.3 empty call in remove_set escalates (theme dropped)", {
  r <- ptr_translate("ggplot(mtcars) + theme(plot.title = ppText)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  expect_false(any(vapply(p$layers, function(l) l$name == "theme", logical(1))))
})

test_that("P9.4 empty call NOT in remove_set survives", {
  r <- ptr_translate("ggplot(mtcars) + pcp_theme(title = ppText)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  expect_true(any(vapply(p$layers, function(l) l$name == "pcp_theme", logical(1))))
})

test_that("P9.5 standalone-eligible empty layer survives even when in remove_set", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(color = ppText)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s, safe_to_remove = c("geom_point"))
  expect_true(any(vapply(p$layers, function(l) l$name == "geom_point", logical(1))))
})

test_that("P9.6 user safe_to_remove extends remove_set", {
  r <- ptr_translate("ggplot(mtcars) + pcp_theme(title = ppText)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s, safe_to_remove = c("pcp_theme"))
  expect_false(any(vapply(p$layers, function(l) l$name == "pcp_theme", logical(1))))
})

test_that("P9.7 positional missing in pipeline stage drops the arg, keeps the call", {
  # Per relaxed P9 (P12.1): empty `num` drops the arg from `head(num)`,
  # leaving `head()` empty. `head` is NOT in default_drop_when_empty, so the
  # call survives and renders as-is — eval relies on head's default n = 6.
  # PLAN-02 (ADR 0012 §1): the lift requires a chain depth >= 2 to fire.
  # A 2-stage chain (filter + head) above the source produces the canonical
  # 3-stage pipeline shape the test originally exercised under the legacy
  # `|>` translator.
  r <- ptr_translate(
    "mtcars |> filter(mpg > 10) |> head(ppNum) |> ggplot(aes(x = mpg))"
  )
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  ggp <- p$layers[[1]]
  expect_true(is_ptr_pipeline(ggp$data_arg))
  # 3 stages: source (mtcars) + filter(mpg > 10) + head().
  expect_equal(length(ggp$data_arg$stages), 3L)
  head_stage <- ggp$data_arg$stages[[3L]]
  expect_true(is_ptr_call(head_stage))
  expect_equal(bare_call_name(head_stage$fun), "head")
  expect_equal(length(head_stage$args), 0L)
})

test_that("P9.7b empty call IN default_drop_when_empty drops from pipeline", {
  # `aes` is in default_drop_when_empty. With var empty, `aes(x = var)` becomes
  # `aes()` empty -> drop sentinel -> the wrapping pipeline-or-arg drops it.
  r <- ptr_translate("mtcars |> ggplot(aes(x = ppVar))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  ggp <- p$layers[[1]]
  # data_arg is the bare `mtcars` literal (no verb stages above source,
  # so the lift's `is_ptr_call(da)` filter skipped it); ggplot's children
  # should not include the empty aes() arg.
  arg_names <- names(ggp$children) %||% rep_len("", length(ggp$children))
  expect_false(any(vapply(ggp$children, function(c) {
    is_ptr_call(c) && identical(bare_call_name(c$fun), "aes")
  }, logical(1))))
})

test_that("P9.8 pipeline collapses to single stage when only one remains", {
  # Inner pipeline; last stage is missing -> should collapse
  pl <- ptr_pipeline(
    stages = list(ptr_literal(quote(mtcars)), ptr_missing()),
    op = "|>", expr = quote(mtcars)
  )
  pruned <- ptr_prune(pl)
  expect_true(is_ptr_literal(pruned))
})

test_that("P9.9 pipeline with all stages dropped becomes missing", {
  pl <- ptr_pipeline(
    stages = list(ptr_missing(), ptr_missing()),
    op = "|>", expr = quote(NULL)
  )
  pruned <- ptr_prune(pl)
  expect_true(is_ptr_missing(pruned))
})

test_that("P9.10 ptr_user_expr never pruned", {
  ue <- ptr_user_expr(quote(theme()))
  pruned <- ptr_prune(ue)
  expect_true(is_ptr_user_expr(pruned))
  expect_equal(deparse(pruned$inner), "theme()")
})

test_that("P9.11 ptr_user_expr with nested zero-arg call survives", {
  ue <- ptr_user_expr(quote(theme(plot.title = element_text())))
  pruned <- ptr_prune(ue)
  expect_true(is_ptr_user_expr(pruned))
})

test_that("P9.13 unary operator escalation (! ptr_missing -> missing)", {
  call_node <- ptr_call(
    fun = quote(`!`),
    args = list(ptr_missing()),
    expr = quote(!x)
  )
  pruned <- ptr_prune(call_node)
  expect_true(is_ptr_missing(pruned))
})

test_that("P9.14 layer with active = FALSE dropped entirely", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + geom_point() + geom_smooth()")
  # Toggle geom_smooth off via active_input_id
  smooth_id <- Filter(function(l) l$name == "geom_smooth", r$layers)[[1]]$active_input_id
  s <- ptr_substitute(r, input_snapshot = setNames(list(FALSE), smooth_id))
  p <- ptr_prune(s)
  expect_false(any(vapply(p$layers, function(l) l$name == "geom_smooth", logical(1))))
})

test_that("P9.16 namespaced call name matched against remove_set by bare name", {
  call_node <- ptr_call(
    fun = quote(rlang::abort),
    args = list(message = ptr_missing()),
    expr = quote(rlang::abort(message = x))
  )
  pruned <- ptr_prune(call_node, safe_to_remove = "abort")
  expect_true(is_ptr_missing(pruned))
})

test_that("P9.18 invalid safe_to_remove rejected", {
  expect_error(ptr_prune(ptr_root(layers = list(), expr = NULL), safe_to_remove = list("a")))
  expect_error(ptr_prune(ptr_root(layers = list(), expr = NULL), safe_to_remove = NA_character_))
  expect_error(ptr_prune(ptr_root(layers = list(), expr = NULL), safe_to_remove = "::"))
  expect_error(ptr_prune(ptr_root(layers = list(), expr = NULL), safe_to_remove = ""))
})

test_that("P9.19 safe_to_remove = NULL accepted; default applies", {
  r <- ptr_translate("ggplot(mtcars) + theme(plot.title = ppText)")
  s <- ptr_substitute(r, input_snapshot = list())
  expect_silent(p <- ptr_prune(s, safe_to_remove = NULL))
  expect_false(any(vapply(p$layers, function(l) l$name == "theme", logical(1))))
})

test_that("P9.21 top-level theme(plot.title = ppText) collapses with ppText missing", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + theme(plot.title = ppText)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  layer_names <- vapply(p$layers, function(l) l$name, character(1))
  expect_false("theme" %in% layer_names)
})

test_that("P9.22 top-level standalone geom_point() survives even when in remove_set", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(colour = ppVar)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s, safe_to_remove = c("geom_point"))
  layer_names <- vapply(p$layers, function(l) l$name, character(1))
  expect_true("geom_point" %in% layer_names)
})

test_that("P9.17 depth limit triggers abort", {
  # Build a hand-rolled 105-deep typed AST of nested ptr_call nodes that
  # bypasses translate (otherwise P1/P5's depth caps would catch it first).
  # ptr_prune's pre-walk guard mirrors the depth=100 contract used by
  # check_translate_depth (P1) and validate_expr_safety (P5).
  inner <- ptr_literal(quote(x))
  for (i in seq_len(110)) {
    inner <- ptr_call(rlang::sym("f"), list(inner), expr = NULL)
  }
  expect_error(ptr_prune(inner), "depth")
})

# ---- P9.23–P9.26 — gap-fillers from test-prune-empty-substitution.R ----

test_that("P9.23 facet_wrap(vars(ppVar)) drops when ppVar missing (cascade)", {
  r <- ptr_translate("ggplot(mtcars) + geom_point() + facet_wrap(vars(ppVar))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  layer_names <- vapply(p$layers, function(l) l$name, character(1))
  expect_false("facet_wrap" %in% layer_names)
})

test_that("P9.24 annotation_custom(grob = expr) drops when expr missing", {
  r <- ptr_translate(
    "ggplot(mtcars) + geom_point() + annotation_custom(grob = ppExpr)"
  )
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  layer_names <- vapply(p$layers, function(l) l$name, character(1))
  expect_false("annotation_custom" %in% layer_names)
})

test_that("P9.25 binary operator inside data_arg escalates when operand missing", {
  # `subset(mtcars, mpg > num)` with num missing: the `>` operator has a
  # positional missing operand → escalates to ptr_missing per P9 (G2);
  # the surrounding subset call's positional second arg is now missing →
  # subset itself drops the arg. Exercises G2 (operator escalation) and
  # the nested-cascade rule together. Note: post-ADR-0012 the data_arg
  # lifts to a `ptr_pipeline` and the rendered output may contain the
  # `|>` token; assert on the operand symbol `mpg` instead of the raw
  # `>` character (which would false-positive on `|>`).
  r <- ptr_translate("ggplot(subset(mtcars, mpg > ppNum)) + geom_point()")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  rendered <- ptr_render(p)
  expect_false(grepl("mpg", rendered, fixed = TRUE))
  expect_false(grepl("ppNum", rendered, fixed = TRUE))
})

test_that("P9.26 anonymous-head call (paren-wrapped) is preserved by prune", {
  # `(function(x) x)(text)` with text supplied via input_snapshot — the
  # call-head is itself a call (not a symbol). The pruner can't match
  # `call_name` to remove_set, so the wrapper survives.
  r <- ptr_translate(
    "ggplot(mtcars) + geom_point() + labs(title = (function(x) x)(ppText))"
  )
  ph_id <- find_nodes(
    r,
    function(n) is_ptr_placeholder(n) && n$keyword == "ppText"
  )[[1]]$id
  s <- ptr_substitute(r, input_snapshot = stats::setNames(list("hi"), ph_id))
  p <- ptr_prune(s)
  rendered <- ptr_render(p)
  expect_true(grepl("function", rendered, fixed = TRUE))
})

test_that("P9.x base::subset / base::transform empty pipeline stages drop by default", {
  # Base R data-verbs are the analogs of dplyr::filter / dplyr::mutate (already
  # in the default drop set). A deselected placeholder leaves an empty
  # subset()/transform() stage which is a no-op (identity on the piped data),
  # so it must drop by DEFAULT (no safe_to_remove) for clean rendered code --
  # parity with the tidyverse verbs that already drop. See default_drop_when_empty().
  for (verb in c("subset", "transform")) {
    r <- ptr_translate(
      sprintf("mtcars |> %s(ppExpr) |> head(2) |> ggplot()", verb),
      expr_check = FALSE
    )
    s <- ptr_substitute(r, input_snapshot = list())  # ppExpr missing
    p <- ptr_prune(s)                                 # DEFAULT remove_set only
    code <- ptr_render(p)
    expect_false(grepl(paste0(verb, "\\(\\)"), code),
                 info = paste0(verb, "() no-op stage should be dropped by default"))
    expect_match(code, "head(2)", fixed = TRUE)       # rest of the pipeline intact
  }
})
