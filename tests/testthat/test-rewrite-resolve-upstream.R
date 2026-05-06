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
  tree <- ptr_translate("mtcars |> head(2) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  expect_true(is_ptr_pipeline(data_arg))
  result <- ptr_resolve_upstream(data_arg, eval_env = .test_env())
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 2L)
})

test_that("placeholder inside pipeline substitutes from snapshot", {
  tree <- ptr_translate("mtcars |> head(num) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  num_id <- find_nodes(data_arg, is_ptr_ph_value)[[1L]]$id
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = stats::setNames(list(3), num_id),
    eval_env = .test_env()
  )
  expect_equal(nrow(result), 3L)
})

test_that("missing placeholder prunes pipeline stage (G6.1 trim-to-root)", {
  tree <- ptr_translate("mtcars |> head(num) |> ggplot()")
  data_arg <- tree$layers[[1L]]$data_arg
  result <- ptr_resolve_upstream(
    data_arg,
    snapshot = list(),
    eval_env = .test_env()
  )
  expect_s3_class(result, "data.frame")
  # head() default is 6 rows; with num missing, head stage drops, leaving mtcars
  expect_equal(nrow(result), nrow(mtcars))
})

test_that("entire pipeline pruning to missing returns NULL", {
  # All stages depend on a missing source
  tree <- ptr_translate("upload |> ggplot()")
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
  tree <- ptr_translate("mtcars |> head(num) |> ggplot()")
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
