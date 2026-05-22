# Pure-function tests for the resugar_pipeline transform (ADR 0012 §3.1)
# and its terminal-grounding predicate (ADR 0012 §3.2). These exercise the
# AST helpers in isolation — no ptr_translate, no Shiny.

test_that("resugar_pipeline splits a two-call chain into source + one stage", {
  expr <- quote(filter(penguins, x > 1))
  out <- resugar_pipeline(expr)
  expect_equal(length(out), 2L)
  expect_identical(out[[1]], quote(penguins))
  expect_identical(out[[2]], quote(filter(x > 1)))
})

test_that("resugar_pipeline splits a three-call chain into source + two stages in user order", {
  expr <- quote(mutate(filter(penguins, x > 1), y = z + 1))
  out <- resugar_pipeline(expr)
  expect_equal(length(out), 3L)
  expect_identical(out[[1]], quote(penguins))
  expect_identical(out[[2]], quote(filter(x > 1)))
  expect_identical(out[[3]], quote(mutate(y = z + 1)))
})

test_that("resugar_pipeline preserves named args on stage calls", {
  expr <- quote(mutate(filter(penguins, x > 1), new_var = y + 1))
  out <- resugar_pipeline(expr)
  expect_identical(out[[3]], quote(mutate(new_var = y + 1)))
})

test_that("resugar_pipeline on a single non-chain call returns just the call", {
  expr <- quote(penguins)
  out <- resugar_pipeline(expr)
  expect_equal(length(out), 1L)
  expect_identical(out[[1]], quote(penguins))
})

test_that("resugar_pipeline strips a single call whose first-arg is a non-call (final-strip pass)", {
  # ggplot(penguins, aes(x)) — first arg is `penguins` symbol, not a call.
  # Loop never enters, but the final-strip pass peels this call too —
  # `penguins` becomes the source, `ggplot(aes(x))` becomes the one stage.
  # (In the broader lift, maybe_lift_data_arg's early-out on
  # `is.call(expr[[2L]])` keeps such single-call data_args from being
  # lifted — this assertion only documents resugar_pipeline's behaviour.)
  expr <- quote(ggplot(penguins, aes(x)))
  out <- resugar_pipeline(expr)
  expect_equal(length(out), 2L)
  expect_identical(out[[1]], quote(penguins))
  expect_identical(out[[2]], quote(ggplot(aes(x))))
})

test_that("resugar_pipeline bottoms out at an opaque-call terminal (loop sees it as the source)", {
  # mutate(filter(make_data(), x > 1), y = z + 1) — deepest first-arg is
  # make_data(), itself a call. resugar_pipeline still walks down (it's
  # purely structural); the caller's terminal-grounding check is what
  # vetoes the lift.
  expr <- quote(mutate(filter(make_data(), x > 1), y = z + 1))
  out <- resugar_pipeline(expr)
  expect_equal(length(out), 3L)
  expect_identical(out[[1]], quote(make_data()))
  expect_true(is.call(out[[1]]))   # terminal_grounds_for_lift would return FALSE
})

test_that("terminal_grounds_for_lift is TRUE for placeholder-source symbols (ppUpload)", {
  # The lift's only role-source placeholder in the stock registry is
  # `ppUpload`. Any future role="source" entry would also satisfy.
  expect_true(terminal_grounds_for_lift(quote(ppUpload)))
})

test_that("terminal_grounds_for_lift is FALSE for bare non-placeholder data names", {
  # Plain nested-call form with bare data names (`mtcars`, `penguins`)
  # must NOT lift — downstream paintr-disable / paintr-prune still rely on
  # the ptr_call shape for these. Plan 04 ("deletion of the per-layer
  # fast-path") handles the migration; this plan stops at placeholder-
  # source-fronted chains.
  expect_false(terminal_grounds_for_lift(quote(penguins)))
  expect_false(terminal_grounds_for_lift(quote(mtcars)))
  expect_false(terminal_grounds_for_lift(quote(iris)))
})

test_that("terminal_grounds_for_lift is FALSE for value/consumer placeholders (only source role lifts)", {
  # ppVar / ppNum / ppText / ppExpr are value- or consumer-role; they are
  # not legitimate data sources and must not ground a lift even when they
  # happen to appear as the deepest first-arg.
  expect_false(terminal_grounds_for_lift(quote(ppVar)))
  expect_false(terminal_grounds_for_lift(quote(ppNum)))
  expect_false(terminal_grounds_for_lift(quote(ppText)))
})

test_that("terminal_grounds_for_lift is FALSE for calls (opaque terminal)", {
  expect_false(terminal_grounds_for_lift(quote(make_data())))
  expect_false(terminal_grounds_for_lift(quote(read_csv("foo.csv"))))
  expect_false(terminal_grounds_for_lift(quote(data.frame(x = 1))))
})

test_that("terminal_grounds_for_lift is FALSE for literals (no lift on data.frame(x=1)-style chains)", {
  # Bare literals as the deepest first-arg arise only after final-strip
  # peels an opaque-source call (e.g., `data.frame(x = 1)` exposes `1`).
  # The narrowing to placeholder-source-symbol vetoes such cases — the
  # chain stays a plain ptr_call instead of spuriously lifting.
  expect_false(terminal_grounds_for_lift("foo"))
  expect_false(terminal_grounds_for_lift(42))
  expect_false(terminal_grounds_for_lift(TRUE))
})
