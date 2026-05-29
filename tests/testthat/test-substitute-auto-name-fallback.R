# ADR 0025 §5 / PLAN-02 — substitute walker fallback semantics.
# `substitute_walk.ptr_ph_data_source(node, ctx)` on a node with the
# shortcut surface (`node$shortcut_id` set) reads
# `ctx$snapshot[[node$shortcut_id]]`. When that snapshot value is empty
# (NULL or ""), the walker falls through to `as.name(node$auto_name)`
# ONLY WHEN that symbol is actually bound in `ctx$eval_env` (i.e. an
# upload bound a frame under it). When nothing is bound it returns
# `ptr_missing()` so the `data=` arg is pruned and the layer inherits the
# plot's data -- the documented no-arg `ppUpload()` meaning. Emitting the
# symbol unconditionally regressed `ppUpload()`-with-no-upload to
# `object '<auto_name>' not found` at eval-time (ppUpload-bug, 2026-05-29).

# Helper: a ctx the source-dispatch consults for `snapshot` + `eval_env`.
make_ctx <- function(snapshot = list(), eval_env = new.env(parent = emptyenv())) {
  list(snapshot = snapshot, eval_env = eval_env)
}

# Helper: build a ptr_ph_data_source node directly (bypass translate so
# we control exact field values).
mk_source_node <- function(shortcut_id, auto_name = NULL) {
  n <- list(
    keyword = "ppUpload",
    param = NA_character_,
    shared = NULL,
    id = "test_source",
    shortcut_id = shortcut_id,
    auto_name = auto_name,
    default = NULL,
    named_args = list()
  )
  structure(n, class = c("ptr_ph_data_source", "ptr_placeholder"))
}

test_that("empty snapshot + auto_name BOUND in eval_env -> as.name(node$auto_name)", {
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "auto_x")
  env <- new.env(parent = emptyenv())
  assign("auto_x", data.frame(a = 1), envir = env)  # simulate an upload bind
  ctx <- make_ctx(list(x_shortcut = ""), eval_env = env)
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_literal"))
  expect_identical(out$expr, as.name("auto_x"))
})

test_that("empty-string snapshot + auto_name UNBOUND -> ptr_missing()", {
  # No upload happened: nothing is bound under `auto_x`. Emitting the
  # symbol would error at eval-time, so the slot is dropped instead.
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "auto_x")
  ctx <- make_ctx(list(x_shortcut = ""), eval_env = new.env(parent = emptyenv()))
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_missing"))
})

test_that("NULL snapshot + auto_name UNBOUND -> ptr_missing()", {
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "auto_x")
  ctx <- make_ctx(list(), eval_env = new.env(parent = emptyenv()))
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_missing"))
})

test_that("shared upload bound in a PARENT eval_env (ADR 0023) still resolves", {
  # Shared sources bind under the canonical in the coordinator eval_env,
  # which is the parent of the per-instance eval_env. `inherits = TRUE`
  # in the walker must reach it.
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "main_ds")
  coord <- new.env(parent = emptyenv())
  assign("main_ds", data.frame(a = 1), envir = coord)
  instance <- new.env(parent = coord)
  ctx <- make_ctx(list(x_shortcut = ""), eval_env = instance)
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_literal"))
  expect_identical(out$expr, as.name("main_ds"))
})

test_that("empty snapshot + NULL auto_name -> ptr_missing()", {
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = NULL)
  ctx <- make_ctx(list(x_shortcut = ""))
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_missing"))
})

test_that("non-empty snapshot still wins over auto_name", {
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "auto_x")
  ctx <- make_ctx(list(x_shortcut = "user_typed"))
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_literal"))
  # ppUpload's resolve_expr returns `as.name(name_value)` for a valid
  # R name, so we expect the snapshot symbol, not the auto_name.
  expect_identical(out$expr, as.name("user_typed"))
})
