# P5 — safety. Walks the typed tree; each visit delegates to the existing
# `validate_expr_safety` walker. Phase 1 covers P5.1–P5.15 + P5.18.
# P5.16 (denied-char-literal returned by resolve_expr) and P5.17
# (post-substitution recheck) are deferred to P8/P11 phases.

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

test_that("P5.18 post-prune empty layers surface as 'no layers' (deferred)", {
  skip("P5.18: P11 phase wires the no-layers vs safety distinction.")
})
