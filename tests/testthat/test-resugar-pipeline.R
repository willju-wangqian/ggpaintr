# PLAN-02 (ADR 0012) — unit tests for the pure-AST canonical-pipeline-lift
# engine: `desugar_pipes_to_nested`, `resugar_pipeline_stages`,
# `rebuild_nested_from_stages`, `try_lift_to_pipeline`. Each test_that maps
# directly to a BDD scenario in the plan; the `Then` clause is the contract.

# ---- Step 1 — desugar -----------------------------------------------------

test_that("desugar_pipes_to_nested rewrites %>% to nested call form", {
  expr <- quote(penguins %>% filter(x > 1) %>% mutate(y = z))
  expected <- quote(mutate(filter(penguins, x > 1), y = z))
  expect_identical(ggpaintr:::desugar_pipes_to_nested(expr), expected)
})

test_that("desugar_pipes_to_nested rewrites the native-pipe sentinel to nested form", {
  # `preprocess_native_pipe()` substitutes `|>` -> `%ptrPipeNative%` BEFORE
  # parse so native-pipe surface survives translation; desugar collapses the
  # sentinel to nested form the same way it collapses `%>%`.
  expr <- str2lang(paste(
    "penguins %ptrPipeNative% filter(x > 1) %ptrPipeNative% mutate(y = z)"
  ))
  expected <- quote(mutate(filter(penguins, x > 1), y = z))
  expect_identical(ggpaintr:::desugar_pipes_to_nested(expr), expected)
})

test_that("desugar_pipes_to_nested handles mixed surface form", {
  expr <- quote(mutate(filter(penguins, x > 1), y = z) %>% summarise(n = n()))
  expected <- quote(summarise(mutate(filter(penguins, x > 1), y = z), n = n()))
  expect_identical(ggpaintr:::desugar_pipes_to_nested(expr), expected)
})

test_that("desugar_pipes_to_nested is a no-op when there are no pipes", {
  expr <- quote(mutate(filter(penguins, x > 1), y = z))
  expect_identical(ggpaintr:::desugar_pipes_to_nested(expr), expr)
})

test_that("desugar_pipes_to_nested preserves arg names through non-pipe descent", {
  expr <- quote(ggplot(data = penguins %>% filter(x > 1), mapping = aes(x = a)))
  expected <- quote(ggplot(data = filter(penguins, x > 1), mapping = aes(x = a)))
  expect_identical(ggpaintr:::desugar_pipes_to_nested(expr), expected)
})

# ---- Step 2 + 3 — split + rebuild round trip -----------------------------

test_that("resugar+rebuild round-trips on a chain with a named first arg", {
  expr <- quote(ggplot(data = penguins, mapping = aes(x = bill_length)))
  parts <- ggpaintr:::resugar_pipeline_stages(expr)
  rebuilt <- ggpaintr:::rebuild_nested_from_stages(parts)
  expect_identical(rebuilt, expr)
  expect_equal(parts$stages[[1L]]$first_arg_name, "data")
})

test_that("resugar+rebuild round-trips on a 3-level chain (positional)", {
  expr <- quote(summarise(mutate(filter(penguins, x > 1), y = z), n = n()))
  parts <- ggpaintr:::resugar_pipeline_stages(expr)
  rebuilt <- ggpaintr:::rebuild_nested_from_stages(parts)
  expect_identical(rebuilt, expr)
  expect_identical(parts$source, quote(penguins))
  expect_equal(length(parts$stages), 3L)
})

test_that("resugar+rebuild round-trips when the first arg of an inner call is named", {
  # `ggplot(data = penguins)` is the deepest call; mutate / filter wrap it
  # positionally. The captured `first_arg_name` chain is c("data", "", "")
  # and rebuild must restore the "data" name on the inner ggplot call.
  expr <- quote(summarise(mutate(filter(ggplot(data = penguins), x > 1), y = z), n = n()))
  parts <- ggpaintr:::resugar_pipeline_stages(expr)
  rebuilt <- ggpaintr:::rebuild_nested_from_stages(parts)
  expect_identical(rebuilt, expr)
  fans <- vapply(parts$stages, function(s) s$first_arg_name, character(1))
  expect_equal(fans, c("data", "", "", ""))
})

# ---- Step 4 — try_lift gates ---------------------------------------------

test_that("try_lift fires on every surface form with the same canonical shape", {
  e_native   <- str2lang(paste(
    "penguins %ptrPipeNative% filter(ppVar > 1) %ptrPipeNative% mutate(y = ppNum)"
  ))
  e_magrittr <- quote(penguins %>% filter(ppVar > 1) %>% mutate(y = ppNum))
  e_nested   <- quote(mutate(filter(penguins, ppVar > 1), y = ppNum))

  r_n <- ggpaintr:::try_lift_to_pipeline(e_native)
  r_m <- ggpaintr:::try_lift_to_pipeline(e_magrittr)
  r_s <- ggpaintr:::try_lift_to_pipeline(e_nested)

  expect_true(isTRUE(r_n$success))
  expect_true(isTRUE(r_m$success))
  expect_true(isTRUE(r_s$success))

  expect_identical(r_n$parts$source, r_m$parts$source)
  expect_identical(r_m$parts$source, r_s$parts$source)
  expect_equal(length(r_n$parts$stages), length(r_m$parts$stages))
  expect_equal(length(r_m$parts$stages), length(r_s$parts$stages))
})

test_that("try_lift refuses input with no stages above source (bare symbol)", {
  # A bare symbol can never reach `try_lift` in production (the classify_calls
  # pass filters non-`ptr_call` data_args out beforehand), but the helper is
  # called directly here to confirm GATE 0 still rejects the 0-stage case.
  res <- ggpaintr:::try_lift_to_pipeline(quote(penguins))
  expect_false(isTRUE(res$success))
  expect_equal(res$reason, "no-stages")
})

test_that("try_lift refuses an opaque-call source on a multi-stage chain", {
  expr <- quote(mutate(filter(make_data(), ppVar > 1), y = z))
  res <- ggpaintr:::try_lift_to_pipeline(expr)
  expect_false(isTRUE(res$success))
  expect_equal(res$reason, "opaque-call-source")
})

test_that("try_lift rejects an opaque-call source on a single-stage chain", {
  # `filter(make_data(), ppVar > 1)` splits to source = `make_data()` (a call)
  # and stages = list(filter(ppVar > 1)). The 1-stage case now passes GATE 0;
  # GATE 2 (opaque-call grounding) is the rejection reason.
  expr <- quote(filter(make_data(), ppVar > 1))
  res <- ggpaintr:::try_lift_to_pipeline(expr)
  expect_false(isTRUE(res$success))
  expect_equal(res$reason, "opaque-call-source")
})

test_that("try_lift refuses a literal-value source", {
  # `c(1, 2, 3)` is itself a call, so the engine descends through it; the
  # deepest non-call after the post-loop split is the literal `1`. GATE 2
  # rejects with `non-data-source` (a literal isn't a structurally-valid
  # data source even though it isn't a call).
  expr <- quote(mutate(filter(c(1, 2, 3), x > 1), y = z))
  res <- ggpaintr:::try_lift_to_pipeline(expr)
  expect_false(isTRUE(res$success))
  expect_equal(res$reason, "non-data-source")
})

test_that("try_lift fires on a two-stage chain whose source is a bare symbol", {
  expr <- quote(mutate(filter(penguins, ppVar > 1), y = ppNum))
  res <- ggpaintr:::try_lift_to_pipeline(expr)
  expect_true(isTRUE(res$success))
  expect_identical(res$parts$source, quote(penguins))
  expect_equal(length(res$parts$stages), 2L)
})
