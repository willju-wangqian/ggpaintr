# P9 — prune. Drop missing and empty-after-substitution. One S3 visitor.

test_that("P9.1 positional missing arg escalates the call", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(data = subset(mtcars, mpg >= num))")
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
  r <- ptr_translate("ggplot(mtcars) + geom_point(color = text, size = 3)")
  s <- ptr_substitute(r, input_snapshot = list())  # text missing
  p <- ptr_prune(s)
  geom <- Filter(function(l) l$name == "geom_point", p$layers)[[1]]
  arg_names <- names(geom$children) %||% rep_len("", length(geom$children))
  expect_false("color" %in% arg_names)
  expect_true("size" %in% arg_names)
})

test_that("P9.3 empty call in remove_set escalates (theme dropped)", {
  r <- ptr_translate("ggplot(mtcars) + theme(plot.title = text)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  expect_false(any(vapply(p$layers, function(l) l$name == "theme", logical(1))))
})

test_that("P9.4 empty call NOT in remove_set survives", {
  r <- ptr_translate("ggplot(mtcars) + pcp_theme(title = text)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  expect_true(any(vapply(p$layers, function(l) l$name == "pcp_theme", logical(1))))
})

test_that("P9.5 standalone-eligible empty layer survives even when in remove_set", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(color = text)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s, safe_to_remove = c("geom_point"))
  expect_true(any(vapply(p$layers, function(l) l$name == "geom_point", logical(1))))
})

test_that("P9.6 user safe_to_remove extends remove_set", {
  r <- ptr_translate("ggplot(mtcars) + pcp_theme(title = text)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s, safe_to_remove = c("pcp_theme"))
  expect_false(any(vapply(p$layers, function(l) l$name == "pcp_theme", logical(1))))
})

test_that("P9.7 positional missing in pipeline stage drops the arg, keeps the call", {
  # Per relaxed P9 (P12.1): empty `num` drops the arg from `head(num)`,
  # leaving `head()` empty. `head` is NOT in default_drop_when_empty, so the
  # call survives and renders as-is — eval relies on head's default n = 6.
  r <- ptr_translate("mtcars |> head(num) |> ggplot(aes(x = mpg))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  ggp <- p$layers[[1]]
  expect_true(is_ptr_pipeline(ggp$data_arg))
  expect_equal(length(ggp$data_arg$stages), 2L)
  head_stage <- ggp$data_arg$stages[[2L]]
  expect_true(is_ptr_call(head_stage))
  expect_equal(bare_call_name(head_stage$fun), "head")
  expect_equal(length(head_stage$args), 0L)
})

test_that("P9.7b empty call IN default_drop_when_empty drops from pipeline", {
  # `aes` is in default_drop_when_empty. With var empty, `aes(x = var)` becomes
  # `aes()` empty -> drop sentinel -> the wrapping pipeline-or-arg drops it.
  r <- ptr_translate("mtcars |> ggplot(aes(x = var))")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  ggp <- p$layers[[1]]
  # data_arg pipeline has just mtcars (single stage -> collapsed to literal),
  # ggplot's children should not include the empty aes() arg.
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
  r <- ptr_translate("ggplot(mtcars) + theme(plot.title = text)")
  s <- ptr_substitute(r, input_snapshot = list())
  expect_silent(p <- ptr_prune(s, safe_to_remove = NULL))
  expect_false(any(vapply(p$layers, function(l) l$name == "theme", logical(1))))
})

test_that("P9.21 top-level theme(plot.title = text) collapses with text missing", {
  r <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + theme(plot.title = text)")
  s <- ptr_substitute(r, input_snapshot = list())
  p <- ptr_prune(s)
  layer_names <- vapply(p$layers, function(l) l$name, character(1))
  expect_false("theme" %in% layer_names)
})

test_that("P9.22 top-level standalone geom_point() survives even when in remove_set", {
  r <- ptr_translate("ggplot(mtcars) + geom_point(colour = var)")
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
