# Stage-disable — gate-2 manual escape. Spec: .claude/specs/stage-disable-checkbox.md

.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- predicate / helpers --------------------------------------------------

# ADR 0012 §1: the canonical-pipeline lift fires for any chain with at
# least one verb stage above the source (GATE 0 = "non-empty stages").
# The disable tests below use 2-stage chains (e.g., `filter(...) |>
# select(...)` above a bare-symbol source) so each test has a real
# stage_id to flip — the same tests would also exercise the lift on a
# 1-stage chain, but a 2-stage shape lets us assert disable-walk's
# stage-index bookkeeping.

test_that("is_data_chain_call: ptr_call with placeholder qualifies", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  pipe <- tree$layers[[1L]]$data_arg
  filter_stage <- pipe$stages[[2L]]
  expect_true(is_data_chain_call(filter_stage))
})

test_that("is_data_chain_call: ptr_call without placeholder rejected", {
  tree <- ptr_translate("mtcars |> filter(year > 2000) |> select(mpg) |> ggplot()")
  pipe <- tree$layers[[1L]]$data_arg
  filter_stage <- pipe$stages[[2L]]
  expect_false(is_data_chain_call(filter_stage))
})

test_that("is_data_chain_call: bare symbol rejected", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  mtcars_stage <- tree$layers[[1L]]$data_arg$stages[[1L]]
  expect_false(is_data_chain_call(mtcars_stage))
})

test_that("data_arg_position: named .data wins over first positional", {
  call <- ptr_call(
    rlang::sym("filter"),
    list(.data = ptr_literal(quote(mtcars)),
         ptr_ph_value(keyword = "ppNum", expr = quote(num))),
    expr = quote(filter(.data = mtcars, num))
  )
  expect_equal(data_arg_position(call), 1L)
})

test_that("data_arg_position: first positional when no named", {
  call <- ptr_call(
    rlang::sym("filter"),
    list(ptr_literal(quote(mtcars)),
         ptr_ph_value(keyword = "ppNum", expr = quote(num))),
    expr = quote(filter(mtcars, num))
  )
  expect_equal(data_arg_position(call), 1L)
})

test_that("data_arg_position: NULL when no candidate", {
  call <- ptr_call(rlang::sym("foo"), list(), expr = quote(foo()))
  expect_null(data_arg_position(call))
})

# ---- assign_stage_ids (P4 hook) -------------------------------------------

test_that("assign_stage_ids: pipeline stage with placeholder gets stage_id", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  pipe <- tree$layers[[1L]]$data_arg
  expect_null(pipe$stages[[1L]]$stage_id)            # mtcars symbol
  expect_equal(pipe$stages[[2L]]$stage_id,
               "ggplot_2_stage_enabled")            # filter(num)
})

test_that("assign_stage_ids: stage without placeholder skipped", {
  # 3-stage chain so the lift fires: filter(ppNum) carries a placeholder
  # and earns a stage_id; select(mpg) has no placeholder so it is skipped.
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  stages <- tree$layers[[1L]]$data_arg$stages
  expect_equal(stages[[2L]]$stage_id, "ggplot_2_stage_enabled")
  expect_null(stages[[3L]]$stage_id)
})

test_that("assign_stage_ids: 1-verb data_arg lifts and stage 2 gets a stage_id", {
  # ADR 0012 §1: a single verb stage above source (`filter(mtcars, ppNum)`)
  # lifts to a canonical 2-stage `ptr_pipeline`. Stage 1 is the source
  # (mtcars literal, no stage_id). Stage 2 is the filter call (carries
  # the ppNum placeholder), so it earns a stage_id at index 2.
  tree <- ptr_translate("ggplot() + geom_point(data = filter(mtcars, ppNum))")
  geom <- Filter(function(l) l$name == "geom_point", tree$layers)[[1L]]
  pipe <- geom$data_arg
  expect_true(is_ptr_pipeline(pipe))
  expect_equal(length(pipe$stages), 2L)
  expect_null(pipe$stages[[1L]]$stage_id)
  expect_equal(pipe$stages[[2L]]$stage_id, "geom_point_2_stage_enabled")
})

test_that("assign_stage_ids: nested non-pipeline calls each get a stage_id", {
  # PLAN-02 (ADR 0012 §1): the layer's data_arg now uses the lifted
  # pipeline shape — `filter(head(mtcars, ppNum), ppNum)` is a 2-stage
  # chain above the bare-symbol source mtcars, so the lift fires and
  # produces a 3-stage pipeline. Each stage that carries a placeholder
  # earns a stage_id from its position in the canonical pipeline rather
  # than from a nested-call path.
  tree <- ptr_translate(
    "ggplot() + geom_point(data = filter(head(mtcars, ppNum), ppNum))"
  )
  geom <- Filter(function(l) l$name == "geom_point", tree$layers)[[1L]]
  pipe <- geom$data_arg
  expect_true(is_ptr_pipeline(pipe))
  # Stage 1 = source (mtcars literal), no stage_id.
  expect_null(pipe$stages[[1L]]$stage_id)
  # Stage 2 = head(ppNum), and stage 3 = filter(ppNum). Both carry
  # placeholders, so both earn stage_ids derived from the canonical path.
  expect_equal(pipe$stages[[2L]]$stage_id, "geom_point_2_stage_enabled")
  expect_equal(pipe$stages[[3L]]$stage_id, "geom_point_3_stage_enabled")
})

test_that("assign_stage_ids: re-translate of identical formula is deterministic", {
  t1 <- ptr_translate("mtcars |> filter(ppNum) |> ggplot()")
  t2 <- ptr_translate("mtcars |> filter(ppNum) |> ggplot()")
  expect_equal(collect_stage_ids(t1), collect_stage_ids(t2))
})

# ---- disable_walk ---------------------------------------------------------

test_that("disable_walk: empty stage_enabled is identity", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  expect_identical(disable_walk(tree, list()), tree)
})

test_that("disable_walk: TRUE-valued stage left alone", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  sid <- "ggplot_2_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(TRUE), sid))
  pipe <- out$layers[[1L]]$data_arg
  # Lifted 3-stage pipeline: source + filter(ppNum) + select(mpg).
  expect_equal(length(pipe$stages), 3L)
})

test_that("disable_walk: FALSE-valued pipeline stage drops from stages list", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  sid <- "ggplot_2_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(FALSE), sid))
  pipe <- out$layers[[1L]]$data_arg
  # Disabling stage 2 (filter) leaves source + select(mpg) = 2 stages.
  expect_equal(length(pipe$stages), 2L)
  expect_true(is_ptr_literal(pipe$stages[[1L]]))
})

test_that("disable_walk: FALSE 1-verb lifted pipeline drops the verb stage", {
  # ADR 0012 §1: `data = filter(mtcars, ppNum)` lifts to a 2-stage pipeline
  # (mtcars + filter). Disabling stage 2 drops the filter, leaving a
  # source-only pipeline whose lone stage is the `mtcars` literal.
  # `disable_walk.ptr_pipeline` does not auto-collapse 1-stage results to
  # the bare source (unlike `prune_walk.ptr_pipeline`) — that's an
  # eval-time concern, not a tree-shape one.
  tree <- ptr_translate("ggplot() + geom_point(data = filter(mtcars, ppNum))")
  sid <- "geom_point_2_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(FALSE), sid))
  geom <- Filter(function(l) l$name == "geom_point", out$layers)[[1L]]
  expect_true(is_ptr_pipeline(geom$data_arg))
  expect_equal(length(geom$data_arg$stages), 1L)
  expect_true(is_ptr_literal(geom$data_arg$stages[[1L]]))
  expect_equal(deparse(geom$data_arg$stages[[1L]]$expr), "mtcars")
})

test_that("disable_walk: nested calls — outer disabled exposes inner", {
  # PLAN-02: the lifted shape is a 3-stage pipeline (mtcars + head + filter).
  # Disabling the outermost stage (the filter — `geom_point_3_stage_enabled`)
  # drops it; the surviving pipeline is mtcars + head.
  tree <- ptr_translate(
    "ggplot() + geom_point(data = filter(head(mtcars, ppNum), ppNum))"
  )
  filter_sid <- "geom_point_3_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(FALSE), filter_sid))
  geom <- Filter(function(l) l$name == "geom_point", out$layers)[[1L]]
  pipe <- geom$data_arg
  expect_true(is_ptr_pipeline(pipe))
  expect_equal(length(pipe$stages), 2L)
  head_stage <- pipe$stages[[2L]]
  expect_true(is_ptr_call(head_stage))
  expect_equal(bare_call_name(head_stage$fun), "head")
})

test_that("disable_walk: nested calls — both disabled collapses to bare data", {
  # PLAN-02: with both manipulation stages disabled the lifted pipeline
  # carries only its source stage; `prune_walk.ptr_pipeline` then collapses
  # the single-stage pipeline to that stage at render/eval time. We pipe
  # through `ptr_prune` here to exercise the full disable -> prune fold.
  tree <- ptr_translate(
    "ggplot() + geom_point(data = filter(head(mtcars, ppNum), ppNum))"
  )
  out <- disable_walk(tree, list(
    "geom_point_2_stage_enabled" = FALSE,  # head(ppNum)
    "geom_point_3_stage_enabled" = FALSE   # filter(ppNum)
  ))
  pruned <- ptr_prune(out)
  geom <- Filter(function(l) l$name == "geom_point", pruned$layers)[[1L]]
  expect_true(is_ptr_literal(geom$data_arg))
  expect_equal(deparse(geom$data_arg$expr), "mtcars")
})

# ---- ptr_resolve_upstream integration -------------------------------------

test_that("ptr_resolve_upstream: disabled head stage strips from upstream", {
  # PLAN-02: a 2-stage chain (filter + head) is required for the lift to
  # fire and produce a `ptr_pipeline` with a disable-able head stage. Use
  # `filter(mpg > 0)` as a no-op upstream so the disable test on head is
  # isolated.
  tree <- ptr_translate("mtcars |> subset(mpg > 0) |> head(ppNum) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  num_id <- find_nodes(data_arg, is_ptr_ph_value)[[1L]]$id
  sid <- "ggplot_3_stage_enabled"  # head stage in the 3-stage pipeline
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(2), num_id),
    eval_env = .test_env(),
    stage_enabled = stats::setNames(list(FALSE), sid)
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), nrow(mtcars))
})

test_that("ptr_resolve_upstream: re-enabled stage restores behavior", {
  tree <- ptr_translate("mtcars |> subset(mpg > 0) |> head(ppNum) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  sid <- "ggplot_3_stage_enabled"
  num_id <- find_nodes(data_arg, is_ptr_ph_value)[[1L]]$id
  r_disabled <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(2), num_id),
    eval_env = .test_env(),
    stage_enabled = stats::setNames(list(FALSE), sid)
  )
  expect_equal(nrow(r_disabled), nrow(mtcars))
  r_enabled <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(2), num_id),
    eval_env = .test_env(),
    stage_enabled = stats::setNames(list(TRUE), sid)
  )
  expect_equal(nrow(r_enabled), 2L)
})

# ---- input-spec -----------------------------------------------------------

test_that("ptr_runtime_input_spec: emits stage_enabled rows", {
  tree <- ptr_translate("mtcars |> filter(ppNum) |> select(mpg) |> ggplot()")
  spec <- ptr_runtime_input_spec(tree)
  stage_rows <- spec[spec$role == "stage_enabled", , drop = FALSE]
  expect_equal(nrow(stage_rows), 1L)
  expect_equal(stage_rows$input_id, "ggplot_2_stage_enabled")
  expect_equal(stage_rows$layer_name, "ggplot")
})

# ---- server state ---------------------------------------------------------

test_that("ptr_init_state: stage_enabled initialized with all-TRUE", {
  state <- ptr_init_state(
    "mtcars |> subset(mpg > 0) |> head(ppNum) |> ggplot()",
    envir = .test_env()
  )
  cur <- shiny::isolate(state$stage_enabled())
  # 3-stage lifted pipeline: source + filter(no placeholder) + head(ppNum).
  # Only head carries a stage_id.
  expect_named(cur, "ggplot_3_stage_enabled")
  expect_true(isTRUE(cur[["ggplot_3_stage_enabled"]]))
})

test_that("ptr_server_internal: toggle input flips state$stage_enabled", {
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "mtcars |> subset(mpg > 0) |> head(ppNum) |> ggplot()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(`ggplot_3_stage_enabled` = FALSE)
    cur <- state$stage_enabled()
    expect_false(isTRUE(cur[["ggplot_3_stage_enabled"]]))
  })
})
