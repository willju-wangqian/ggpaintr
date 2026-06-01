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

# ---- PLAN-01 (ADR 0012 §5 OQ2) — op_hint threads through lift -----------

test_that("build_pipeline_from_lift threads op_hint into ptr_pipeline$op", {
  # SC9: `build_pipeline_from_lift` accepts an `op_hint` argument that is
  # stamped on the returned `ptr_pipeline$op`. The default value (`"|>"`)
  # preserves today's behavior for any caller not yet threading the hint.
  parts <- ggpaintr:::try_lift_to_pipeline(quote(head(mtcars, 2)))$parts
  expect_equal(ggpaintr:::build_pipeline_from_lift(parts, op_hint = "%>%")$op, "%>%")
  expect_equal(ggpaintr:::build_pipeline_from_lift(parts, op_hint = "|>")$op, "|>")
  expect_equal(ggpaintr:::build_pipeline_from_lift(parts)$op, "|>")
})

# ---- Regression — a value placeholder (e.g. `ppExpr(y ~ x)`) is captured
# state, never a pipeline data source. Before the fix the aggressive descent
# walked THROUGH `ppExpr(...)` into the formula and lifted its LHS `y` as the
# "data source", turning
#   stats::predict(lm(ppExpr(y ~ x), data = ppUpload(d)))
# into `y |> ~(x) |> lm() |> predict()` -- which boots empty and evals
# "object 'y' not found", and strips the ppExpr formula default from its
# textarea. Two guards enforce the contract: the descent loop stops at any
# placeholder call, and the grounding gate accepts only a symbol or a
# SOURCE-role placeholder as the pipeline source.

test_that("is_data_source_placeholder_call accepts only source-role placeholders", {
  expect_true(ggpaintr:::is_data_source_placeholder_call(quote(ppUpload(d))))
  expect_false(ggpaintr:::is_data_source_placeholder_call(quote(ppExpr(y ~ x))))
  expect_false(ggpaintr:::is_data_source_placeholder_call(quote(ppVar(mpg))))
  expect_false(ggpaintr:::is_data_source_placeholder_call(quote(head(mtcars))))
})

test_that("resugar does not descend into a value placeholder's arguments", {
  # The formula `y ~ x` must remain whole inside `ppExpr(...)`, never bisected
  # into a `y` source + a `~(x)` stage.
  parts <- ggpaintr:::resugar_pipeline_stages(quote(lm(ppExpr(y ~ x), data = d)))
  expect_identical(parts$source, quote(ppExpr(y ~ x)))
  expect_false(identical(parts$source, quote(y)))
})

test_that("try_lift rejects a value-placeholder data source", {
  res <- ggpaintr:::try_lift_to_pipeline(
    quote(stats::predict(lm(ppExpr(y ~ x), data = ppUpload(d))))
  )
  expect_false(isTRUE(res$success))
  expect_equal(res$reason, "opaque-call-source")
})

test_that("translate keeps a computed formula data source as an opaque call", {
  root <- ggpaintr:::ptr_translate(
    paste0("stats::predict(lm(ppExpr(y ~ x), data = ppUpload(d))) |> ",
           "ggplot(aes(.fitted, .resid)) + geom_point()")
  )
  data_arg <- root$layers[[1L]]$data_arg
  expect_s3_class(data_arg, "ptr_call")
  expect_false(inherits(data_arg, "ptr_pipeline"))
})

test_that("an ordinary source-placeholder pipeline still lifts", {
  # Guard regression: a SOURCE-role placeholder at the pipeline head is the
  # normal `ppUpload(d) |> filter(...)` shape and MUST still lift.
  res <- ggpaintr:::try_lift_to_pipeline(
    quote(dplyr::filter(ppUpload(d), cyl == 4))
  )
  expect_true(isTRUE(res$success))
  expect_identical(res$parts$source, quote(ppUpload(d)))
})

# --- Regression: a bare `~` formula (no placeholder wrapper) is also atomic.
# Before the fix, the aggressive descent tore `lm(y ~ x, data = d)` into
# `y |> ~(x) |> lm(data = d)` (mistaking the formula LHS for the data frame),
# and a two-sided formula in a layer arg rendered as `facet_grid(~(a, b))`
# instead of infix. A formula is never a pipeline stage, never a data source,
# and always renders infix.

test_that("is_formula_call recognises one- and two-sided formulas", {
  expect_true(ggpaintr:::is_formula_call(quote(y ~ x)))
  expect_true(ggpaintr:::is_formula_call(quote(~x)))
  expect_false(ggpaintr:::is_formula_call(quote(a + b)))
  expect_false(ggpaintr:::is_formula_call(quote(f(x))))
  expect_false(ggpaintr:::is_formula_call(quote(x)))
})

test_that("resugar does not tear a bare formula into pipeline stages", {
  parts <- ggpaintr:::resugar_pipeline_stages(quote(lm(y ~ x, data = d)))
  # The formula stays whole inside `lm(...)`; its LHS `y` is never the source.
  expect_false(identical(parts$source, quote(y)))
  res <- ggpaintr:::try_lift_to_pipeline(
    quote(stats::predict(lm(y ~ x, data = mtcars)))
  )
  expect_false(isTRUE(res$success))
})

test_that("a bare formula in a computed data source renders intact and infix", {
  root <- ggpaintr:::ptr_translate(
    paste0("stats::predict(lm(mpg ~ wt, data = mtcars)) |> ",
           "ggplot(aes(.fitted, .resid)) + geom_point()")
  )
  res <- ggpaintr:::ptr_complete_expr_safe(
    root, snapshot = list(), eval_env = new.env(parent = globalenv())
  )
  expect_true(isTRUE(res$ok))
  expect_match(res$code_text, "lm(mpg ~ wt, data = mtcars)", fixed = TRUE)
  expect_false(grepl("~(", res$code_text, fixed = TRUE))   # not the prefix form
  expect_false(grepl("|> ~", res$code_text, fixed = TRUE)) # not torn into a pipe
})

test_that("a two-sided formula in a layer argument renders infix", {
  root <- ggpaintr:::ptr_translate(
    "ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_grid(gear ~ cyl)"
  )
  res <- ggpaintr:::ptr_complete_expr_safe(
    root, snapshot = list(), eval_env = new.env(parent = globalenv())
  )
  expect_true(isTRUE(res$ok))
  expect_match(res$code_text, "facet_grid(gear ~ cyl)", fixed = TRUE)
})

test_that("a one-sided formula layer arg is unchanged", {
  root <- ggpaintr:::ptr_translate(
    "ggplot(mtcars, aes(wt, mpg)) + geom_point() + facet_wrap(~cyl)"
  )
  res <- ggpaintr:::ptr_complete_expr_safe(
    root, snapshot = list(), eval_env = new.env(parent = globalenv())
  )
  expect_true(isTRUE(res$ok))
  expect_match(res$code_text, "facet_wrap(~cyl)", fixed = TRUE)
})

# --- Regression: explicit parentheses are load-bearing in model formulas
# (`a * (b + c)` != `(a * b) + c`) and must render verbatim. The generic
# call renderer used to emit a `(` node as `((x)` (a doubled, unbalanced
# paren) -- valid R in the typed tree (so the plot still evaluated) but a
# broken, unparseable code panel.

test_that("explicit parentheses in a formula survive rendering verbatim", {
  root <- ggpaintr:::ptr_translate(paste0(
    "stats::predict(lm(mpg ~ wt * (cyl + hp), data = mtcars)) |> ",
    "ggplot(aes(.fitted, .resid)) + geom_point()"
  ))
  res <- ggpaintr:::ptr_complete_expr_safe(
    root, snapshot = list(), eval_env = new.env(parent = globalenv())
  )
  expect_true(isTRUE(res$ok))
  expect_silent(parse(text = res$code_text))            # code panel is valid R
  expect_match(res$code_text, "mpg ~ wt * (cyl + hp)", fixed = TRUE)
  expect_false(grepl("((cyl", res$code_text, fixed = TRUE))
})

test_that("a parenthesis node renders as `(x)`, not `((x)` (non-formula)", {
  root <- ggpaintr:::ptr_translate(
    "ggplot(mtcars, aes(wt, mpg)) + geom_point(size = (2 + 1) * 2)"
  )
  res <- ggpaintr:::ptr_complete_expr_safe(
    root, snapshot = list(), eval_env = new.env(parent = globalenv())
  )
  expect_true(isTRUE(res$ok))
  expect_silent(parse(text = res$code_text))
  expect_match(res$code_text, "(2 + 1) * 2", fixed = TRUE)
})
