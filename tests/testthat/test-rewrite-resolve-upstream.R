# `mtcars` lives in `datasets`, `head` lives in `utils`. Build a small env
# whose parent is the search path so eval can find both without depending
# on the test runner's frame.
.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

test_that("NULL subtree returns NULL", {
  expect_null(ptr_resolve_upstream(NULL))
})

test_that("literal subtree evaluates to data frame", {
  tree <- ptr_translate("ggplot(mtcars)")
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(data_arg, eval_env = .test_env())
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), names(mtcars))
})

test_that("pipeline subtree folds and evaluates", {
  # PLAN-02 (ADR 0012 §1): the canonical pipeline shape only emerges when
  # the lift's chain-depth gate fires (>= 2 stages above the source). A
  # 2-stage chain (filter + head) produces the 3-stage pipeline this test
  # exercises.
  tree <- ptr_translate("mtcars |> subset(mpg > 0) |> head(2) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  expect_true(is_ptr_pipeline(data_arg))
  result <- ptr_resolve_upstream(data_arg, eval_env = .test_env())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
})

test_that("placeholder inside pipeline substitutes from snapshot", {
  tree <- ptr_translate("mtcars |> head(ppNum) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  num_id <- find_nodes(data_arg, is_ptr_ph_value)[[1L]]$id
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(3), num_id),
    eval_env = .test_env()
  )
  expect_equal(nrow(result), 3L)
})

test_that("missing positional placeholder drops arg but keeps the call (P12.1)", {
  # Per relaxed P9: empty `num` drops the arg from `head(num)`, leaving
  # `head()` empty. head() at eval uses its default n = 6, so the upstream
  # data has 6 rows.
  tree <- ptr_translate("mtcars |> head(ppNum) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = list(),
    eval_env = .test_env()
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 6L)
})

test_that("entire pipeline pruning to missing returns NULL", {
  # All stages depend on a missing source
  tree <- ptr_translate("ppUpload |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = list(),
    eval_env = .test_env()
  )
  expect_null(result)
})

test_that("eval error returns NULL (not abort)", {
  tree <- ptr_translate("nonexistent_object_xyz |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = list(),
    eval_env = .test_env()
  )
  expect_null(result)
})

test_that("safety check fires on denylisted subtree", {
  bad <- ptr_call(
    rlang::sym("system"),
    list(ptr_literal("id")),
    expr = quote(system("id"))
  )
  expect_error(
    ptr_resolve_upstream(bad, eval_env = .test_env(), expr_check = TRUE),
    "system"
  )
})

test_that("cache returns identical value without re-eval", {
  cache <- new.env(parent = emptyenv())
  call_count <- 0L
  e <- .test_env(list(track_data = function() {
    call_count <<- call_count + 1L
    mtcars
  }))
  bad_subtree <- ptr_call(
    rlang::sym("track_data"),
    list(),
    expr = quote(track_data())
  )

  r1 <- ptr_resolve_upstream(bad_subtree, eval_env = e, cache = cache,
                             expr_check = FALSE)
  r2 <- ptr_resolve_upstream(bad_subtree, eval_env = e, cache = cache,
                             expr_check = FALSE)
  expect_equal(r1, r2)
  expect_equal(call_count, 1L)  # second call hit cache
})

test_that("different snapshot produces fresh eval", {
  cache <- new.env(parent = emptyenv())
  tree <- ptr_translate("mtcars |> head(ppNum) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  num_id <- find_nodes(data_arg, is_ptr_ph_value)[[1L]]$id

  r1 <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(2), num_id),
    eval_env = .test_env(),
    cache = cache
  )
  r2 <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(5), num_id),
    eval_env = .test_env(),
    cache = cache
  )
  expect_equal(nrow(r1), 2L)
  expect_equal(nrow(r2), 5L)
})

test_that("P11.5 reserved-word columns renamed", {
  e <- .test_env(list(reserved_df = data.frame(
    `if` = 1:3, `for` = 4:6, check.names = FALSE
  )))
  result <- ptr_resolve_upstream(
    ptr_literal(rlang::sym("reserved_df")),
    eval_env = e
  )
  expect_true(all(c("if_", "for_") %in% names(result)))
  expect_false(any(c("if", "for") %in% names(result)))
})

test_that("P11.6 disambiguates against pre-existing normalized name", {
  e <- .test_env(list(conflict_df = data.frame(
    `if` = 1:3, if_ = 4:6, check.names = FALSE
  )))
  result <- ptr_resolve_upstream(
    ptr_literal(rlang::sym("conflict_df")),
    eval_env = e
  )
  expect_true("if_" %in% names(result))   # original preserved
  expect_true("if_2" %in% names(result))  # renamed `if`
})

test_that("normalize_columns = FALSE leaves names alone", {
  e <- .test_env(list(reserved_df = data.frame(
    `if` = 1:3, check.names = FALSE
  )))
  result <- ptr_resolve_upstream(
    ptr_literal(rlang::sym("reserved_df")),
    eval_env = e,
    normalize_columns = FALSE
  )
  expect_true("if" %in% names(result))
})

test_that("non-data-frame eval result returns NULL", {
  result <- ptr_resolve_upstream(
    ptr_literal(42),
    eval_env = .test_env()
  )
  expect_null(result)
})

test_that("matrix coerces to data frame", {
  e <- .test_env(list(mat = matrix(
    1:6, nrow = 2L,
    dimnames = list(NULL, c("a", "b", "c"))
  )))
  result <- ptr_resolve_upstream(
    ptr_literal(rlang::sym("mat")),
    eval_env = e
  )
  expect_s3_class(result, "data.frame")
  expect_equal(names(result), c("a", "b", "c"))
})

# --- safe_to_remove must reach the upstream-resolution prune ----------------
# Regression for the prune-policy divergence: the render prune
# (`ptr_complete_expr_safe`) threaded `safe_to_remove`, but the upstream-data
# prune here did not. A deselected consumer inside a non-default-set predicate
# (`!is.na(ppVar(...))`) leaves a broken `is.na()` that errors on eval and
# starves downstream pickers (NULL upstream). When the caller opts `is.na`
# into `safe_to_remove`, the upstream prune must drop the whole `filter()`
# stage just like the render prune does.

test_that("safe_to_remove is honored in upstream resolution (deselected predicate)", {
  # mtcars (32 rows) |> filter(!is.na(ppVar(cyl))) |> head(2)
  # Deselect cyl (empty snapshot) -> ppVar(cyl) is missing. With is.na opted
  # into safe_to_remove, is.na() prunes -> `!` propagates -> filter() drops,
  # leaving `mtcars |> head(2)` = 2 rows.
  tree <- ptr_translate(
    "mtcars |> dplyr::filter(!is.na(ppVar(cyl))) |> head(2) |> ggplot()",
    expr_check = FALSE
  )
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = list(),                 # cyl deselected
    eval_env = .test_env(),
    safe_to_remove = "is.na"
  )
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
  expect_equal(names(result), names(mtcars))
})

test_that("without safe_to_remove the broken predicate still yields NULL upstream", {
  # Default path (caller has NOT opted is.na in): is.na cannot be pruned by
  # the default remove_set, so `filter(!is.na())` survives, errors on eval,
  # and resolution returns NULL. Guards that the fix is opt-in, not a blanket
  # behavior change.
  tree <- ptr_translate(
    "mtcars |> dplyr::filter(!is.na(ppVar(cyl))) |> head(2) |> ggplot()",
    expr_check = FALSE
  )
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = list(),
    eval_env = .test_env()
  )
  expect_null(result)
})
