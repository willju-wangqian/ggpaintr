# Stage-disable — gate-2 manual escape. Spec: .claude/specs/stage-disable-checkbox.md

.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- predicate / helpers --------------------------------------------------

test_that("is_data_chain_call: ptr_call with placeholder qualifies", {
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  pipe <- tree$layers[[1L]]$data_arg
  filter_stage <- pipe$stages[[2L]]
  expect_true(is_data_chain_call(filter_stage))
})

test_that("is_data_chain_call: ptr_call without placeholder rejected", {
  tree <- ptr_translate("mtcars |> filter(year > 2000) |> ggplot()")
  pipe <- tree$layers[[1L]]$data_arg
  filter_stage <- pipe$stages[[2L]]
  expect_false(is_data_chain_call(filter_stage))
})

test_that("is_data_chain_call: bare symbol rejected", {
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  mtcars_stage <- tree$layers[[1L]]$data_arg$stages[[1L]]
  expect_false(is_data_chain_call(mtcars_stage))
})

test_that("data_arg_position: named .data wins over first positional", {
  call <- ptr_call(
    rlang::sym("filter"),
    list(.data = ptr_literal(quote(mtcars)),
         ptr_ph_value(keyword = "num", expr = quote(num))),
    expr = quote(filter(.data = mtcars, num))
  )
  expect_equal(data_arg_position(call), 1L)
})

test_that("data_arg_position: first positional when no named", {
  call <- ptr_call(
    rlang::sym("filter"),
    list(ptr_literal(quote(mtcars)),
         ptr_ph_value(keyword = "num", expr = quote(num))),
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
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  pipe <- tree$layers[[1L]]$data_arg
  expect_null(pipe$stages[[1L]]$stage_id)            # mtcars symbol
  expect_equal(pipe$stages[[2L]]$stage_id,
               "ggplot_2_stage_enabled")            # filter(num)
})

test_that("assign_stage_ids: stage without placeholder skipped", {
  tree <- ptr_translate("mtcars |> filter(num) |> select(mpg) |> ggplot()")
  stages <- tree$layers[[1L]]$data_arg$stages
  expect_equal(stages[[2L]]$stage_id, "ggplot_2_stage_enabled")
  expect_null(stages[[3L]]$stage_id)
})

test_that("assign_stage_ids: single-call data_arg gets stage_id at path 0", {
  tree <- ptr_translate("ggplot() + geom_point(data = filter(mtcars, num))")
  geom <- Filter(function(l) l$name == "geom_point", tree$layers)[[1L]]
  expect_equal(geom$data_arg$stage_id, "geom_point_0_stage_enabled")
})

test_that("assign_stage_ids: nested non-pipeline calls each get a stage_id", {
  tree <- ptr_translate(
    "ggplot() + geom_point(data = filter(head(mtcars, num), num))"
  )
  geom <- Filter(function(l) l$name == "geom_point", tree$layers)[[1L]]
  outer <- geom$data_arg
  expect_equal(outer$stage_id, "geom_point_0_stage_enabled")
  inner <- outer$args[[1L]]
  expect_equal(inner$stage_id, "geom_point_1_stage_enabled")
})

test_that("assign_stage_ids: re-translate of identical formula is deterministic", {
  t1 <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  t2 <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  expect_equal(collect_stage_ids(t1), collect_stage_ids(t2))
})

# ---- disable_walk ---------------------------------------------------------

test_that("disable_walk: empty stage_enabled is identity", {
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  expect_identical(disable_walk(tree, list()), tree)
})

test_that("disable_walk: TRUE-valued stage left alone", {
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  sid <- "ggplot_2_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(TRUE), sid))
  pipe <- out$layers[[1L]]$data_arg
  expect_equal(length(pipe$stages), 2L)
})

test_that("disable_walk: FALSE-valued pipeline stage drops from stages list", {
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  sid <- "ggplot_2_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(FALSE), sid))
  pipe <- out$layers[[1L]]$data_arg
  expect_equal(length(pipe$stages), 1L)
  expect_true(is_ptr_literal(pipe$stages[[1L]]))
})

test_that("disable_walk: FALSE single-call data_arg replaces with first arg", {
  tree <- ptr_translate("ggplot() + geom_point(data = filter(mtcars, num))")
  sid <- "geom_point_0_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(FALSE), sid))
  geom <- Filter(function(l) l$name == "geom_point", out$layers)[[1L]]
  expect_true(is_ptr_literal(geom$data_arg))
  expect_equal(deparse(geom$data_arg$expr), "mtcars")
})

test_that("disable_walk: nested calls — outer disabled exposes inner", {
  tree <- ptr_translate(
    "ggplot() + geom_point(data = filter(head(mtcars, num), num))"
  )
  outer_sid <- "geom_point_0_stage_enabled"
  out <- disable_walk(tree, stats::setNames(list(FALSE), outer_sid))
  geom <- Filter(function(l) l$name == "geom_point", out$layers)[[1L]]
  expect_true(is_ptr_call(geom$data_arg))
  expect_equal(bare_call_name(geom$data_arg$fun), "head")
})

test_that("disable_walk: nested calls — both disabled collapses to bare data", {
  tree <- ptr_translate(
    "ggplot() + geom_point(data = filter(head(mtcars, num), num))"
  )
  out <- disable_walk(tree, list(
    "geom_point_0_stage_enabled" = FALSE,
    "geom_point_1_stage_enabled" = FALSE
  ))
  geom <- Filter(function(l) l$name == "geom_point", out$layers)[[1L]]
  expect_true(is_ptr_literal(geom$data_arg))
  expect_equal(deparse(geom$data_arg$expr), "mtcars")
})

# ---- ptr_resolve_upstream integration -------------------------------------

test_that("ptr_resolve_upstream: disabled head stage strips from upstream", {
  tree <- ptr_translate("mtcars |> head(num) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  num_id <- find_nodes(data_arg, is_ptr_ph_value)[[1L]]$id
  sid <- "ggplot_2_stage_enabled"
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
  tree <- ptr_translate("mtcars |> head(num) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  sid <- "ggplot_2_stage_enabled"
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
  tree <- ptr_translate("mtcars |> filter(num) |> ggplot()")
  spec <- ptr_runtime_input_spec(tree)
  stage_rows <- spec[spec$role == "stage_enabled", , drop = FALSE]
  expect_equal(nrow(stage_rows), 1L)
  expect_equal(stage_rows$input_id, "ggplot_2_stage_enabled")
  expect_equal(stage_rows$layer_name, "ggplot")
})

# ---- server state ---------------------------------------------------------

test_that("ptr_server_state: stage_enabled initialized with all-TRUE", {
  state <- ptr_server_state(
    "mtcars |> head(num) |> ggplot()",
    envir = .test_env()
  )
  cur <- shiny::isolate(state$stage_enabled())
  expect_named(cur, "ggplot_2_stage_enabled")
  expect_true(isTRUE(cur[["ggplot_2_stage_enabled"]]))
})

test_that("ptr_server: toggle input flips state$stage_enabled", {
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server(
      input, output, session,
      "mtcars |> head(num) |> ggplot()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(`ggplot_2_stage_enabled` = FALSE)
    cur <- state$stage_enabled()
    expect_false(isTRUE(cur[["ggplot_2_stage_enabled"]]))
  })
})
