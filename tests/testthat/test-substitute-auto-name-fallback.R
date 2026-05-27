# ADR 0025 §5 / PLAN-02 — substitute walker fallback semantics.
# `substitute_walk.ptr_ph_data_source(node, ctx)` on a node with the
# shortcut surface (`node$shortcut_id` set) reads
# `ctx$snapshot[[node$shortcut_id]]`. When that snapshot value is empty
# (NULL or ""), the walker falls through to `as.name(node$auto_name)`
# when `node$auto_name` is non-NULL; otherwise it returns
# `ptr_missing()` (pre-PLAN-02 behaviour).

# Helper: a minimal ctx the source-dispatch only consults for `snapshot`.
make_ctx <- function(snapshot = list()) list(snapshot = snapshot)

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

test_that("empty-string snapshot + auto_name → as.name(node$auto_name)", {
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "auto_x")
  ctx <- make_ctx(list(x_shortcut = ""))
  out <- ggpaintr:::substitute_walk(node, ctx)
  # The walker wraps its return in `ptr_literal()`; the underlying expr
  # lives at `out$expr` (mirroring how other walker tests assert).
  expect_true(inherits(out, "ptr_literal"))
  expect_identical(out$expr, as.name("auto_x"))
})

test_that("NULL snapshot + auto_name → as.name(node$auto_name)", {
  node <- mk_source_node(shortcut_id = "x_shortcut", auto_name = "auto_x")
  ctx <- make_ctx(list())  # snapshot lookup returns NULL
  out <- ggpaintr:::substitute_walk(node, ctx)
  expect_true(inherits(out, "ptr_literal"))
  expect_identical(out$expr, as.name("auto_x"))
})

test_that("empty snapshot + NULL auto_name → ptr_missing()", {
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
