# P5 — safety. Walks the typed tree; each visit delegates to the existing
# `validate_expr_safety` walker. P5.1–P5.15 exercise translate-time checks;
# P5.16/P5.17 exercise the post-substitute recheck inside `ptr_eval`;
# P5.18 confirms an empty post-prune layer list is reported as a no-layers
# error rather than as a safety abort.

test_that("P5.1 denylist symbol blocked (`system`)", {
  expect_error(ptr_translate("ggplot(mtcars) + geom_point(data = system('id'))"),
               "system")
})

test_that("P5.2 bare denylist symbol blocked (higher-order arg)", {
  expect_error(ptr_translate("ggplot(mtcars) + sapply(1, system)"),
               "system")
})

test_that("P5.3 namespaced denylist call blocked", {
  expect_error(ptr_translate("ggplot(mtcars) + base::system('id')"),
               "system")
})

test_that("P5.4 IIFE wrapping denylist call blocked", {
  expect_error(ptr_translate("(function(x) x)(system('id'))"),
               "system")
})

test_that("P5.5 anonymous function body checked", {
  expect_error(
    ptr_translate("ggplot(mtcars) + sapply(1:3, function(x) file.remove('a'))"),
    "file.remove"
  )
})

test_that("P5.6 lambda formal default value checked", {
  expect_error(
    ptr_translate("ggplot(mtcars) + (function(x = system('id')) x)()"),
    "system"
  )
})

test_that("P5.7 pairlist contents checked", {
  # `function(x = system("id")) x` parses with a pairlist of formals; the
  # default value `system("id")` lives inside the pairlist and must be walked.
  e <- parse(text = "function(x = system('id')) x")[[1]]
  expect_error(validate_expr_safety(e), "system")
})

test_that("P5.8 string literal in denylist blocked", {
  # `c()` head is benign; the denylisted "system" appears only as a string
  # literal arg. The walker must descend into character literals.
  expect_error(ptr_translate("ggplot(mtcars) + labs(title = c('system', 'x'))"),
               "system")
})

test_that("P5.9 safe expression unaffected", {
  expect_silent(
    ptr_translate("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point() + scale_x_log10()")
  )
})

test_that("P5.10 expr_check = FALSE bypasses safety", {
  expect_silent(
    ptr_translate("ggplot(mtcars) + geom_point(data = system('id'))",
                  expr_check = FALSE)
  )
})

test_that("P5.11 allowlist mode rejects unlisted call", {
  expect_error(
    ptr_translate(
      "ggplot(mtcars) + my_unlisted_fn()",
      expr_check = list(allow_list = c("ggplot", "geom_point"))
    )
  )
})

test_that("P5.12 custom deny_list adds entries", {
  expect_error(
    ptr_translate("ggplot(mtcars) + my_dangerous_fn()",
                  expr_check = list(deny_list = c("my_dangerous_fn"))),
    "my_dangerous_fn"
  )
})

test_that("P5.13 super-assign blocked", {
  expect_error(ptr_translate("ggplot(mtcars) + (x <<- 1)"))
})

test_that("P5.14 assignment form `body<-` blocked", {
  expect_error(ptr_translate("`body<-`(f, value = quote(x))"),
               "body<-")
})

test_that("P5.15 depth limit triggers safety abort", {
  # The legacy `validate_expr_safety` walker has its own depth limit (100).
  # Build a deep call programmatically and walk it directly.
  e <- quote(x)
  for (i in seq_len(110)) e <- call("f", e)
  expect_error(validate_expr_safety(e), "depth")
})

test_that("P5.16 custom resolve_expr returning denylisted character literal blocked", {
  # Custom value placeholder whose resolve_expr returns the string "system".
  # Substitute wraps it in ptr_literal("system"); the layer's eval expression
  # ends up containing the string literal. P11 re-runs validate_expr_safety
  # per layer, which descends into character literals (cf. P5.8) and aborts.
  ptr_define_placeholder_value(
    keyword = "danger",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) "system"
  )
  on.exit({
    ptr_registry_v2_clear()
    ptr_register_builtins()
  })
  r <- ptr_translate("ggplot(mtcars) + labs(title = danger)")
  id <- find_nodes(r,
                   function(x) is_ptr_placeholder(x) && x$keyword == "danger")[[1]]$id
  s <- ptr_substitute(r, input_snapshot = stats::setNames(list("anything"), id))
  p <- ptr_prune(s)
  expect_error(ptr_eval(p), "system")
})

test_that("P5.17 ptr_eval re-runs safety on each layer post-substitute", {
  # Build a ptr_root by hand whose layer is a ptr_user_expr containing a
  # denylist call. Bypassing translate proves the per-layer recheck inside
  # ptr_eval is independent of the translate-time check.
  bad_layer <- ptr_user_expr(quote(system("id")))
  root <- ptr_root(layers = list(bad_layer))
  expect_error(ptr_eval(root), "system")
})

test_that("P5.18 empty layers report as no-layers, not as a safety error", {
  empty <- ptr_root(layers = list(), expr = NULL)
  expect_error(ptr_eval(empty), "[Nn]o layers")
  e <- tryCatch(ptr_eval(empty), error = identity)
  expect_false(grepl("denylist|not allowed|depth|maliciously",
                     conditionMessage(e)))
})
