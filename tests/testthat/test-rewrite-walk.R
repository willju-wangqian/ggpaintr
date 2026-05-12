# Cursor-threaded traversal kernel (ptr_cursor / ptr_rewrite_pre /
# ptr_cursor_descend) and the ptr_assert_* tree validators.

# ---- a hand-built typed tree exercising every cursor field ----------------

build_cursor_tree <- function() {
  consumer <- ptr_ph_data_consumer(keyword = "var", expr = quote(x))
  # An `upstream` back-pointer that must NOT be descended into.
  consumer$upstream <- ptr_ph_data_source(keyword = "upload", expr = quote(.data))
  consumer$upstream$marker <- "SHOULD_NOT_BE_VISITED"

  aes_call <- ptr_call(fun = quote(aes), args = list(x = consumer), expr = quote(aes(x = x)))
  filter_call <- ptr_call(
    fun = quote(filter),
    args = list(ptr_ph_value(keyword = "expr", expr = quote(cyl == 4))),
    expr = quote(filter(cyl == 4))
  )
  pipe <- ptr_pipeline(
    stages = list(ptr_literal(expr = quote(mtcars)), filter_call),
    op = "|>", expr = quote(mtcars |> filter(cyl == 4))
  )
  layer <- ptr_layer(
    name = "ggplot", expr = quote(ggplot(mtcars |> filter(cyl == 4), aes(x = x))),
    data_arg = pipe, children = list(mapping = aes_call)
  )
  ptr_root(layers = list(layer), expr = quote(ggplot()))
}

collect_visits <- function(tree) {
  visits <- list()
  ptr_rewrite_pre(tree, function(n, cur) {
    visits[[length(visits) + 1L]] <<- list(class = class(n)[[1L]], cur = cur, node = n)
    n
  })
  visits
}

test_that("ptr_rewrite_pre threads a cursor matching the per-container rules", {
  visits <- collect_visits(build_cursor_tree())
  classes <- vapply(visits, `[[`, character(1), "class")
  expect_equal(classes, c(
    "ptr_root", "ptr_layer", "ptr_pipeline", "ptr_literal", "ptr_call",
    "ptr_ph_value", "ptr_call", "ptr_ph_data_consumer"
  ))

  cur_of <- function(i) visits[[i]]$cur

  # 1: root — default cursor
  expect_equal(cur_of(1)$layer_name, NA_character_)
  expect_equal(cur_of(1)$path, integer())
  expect_true(is.na(cur_of(1)$slot))
  expect_false(cur_of(1)$in_data_position)
  expect_true(is.na(cur_of(1)$pipeline_index))

  # 2: layer via "layers"
  expect_equal(cur_of(2)$layer_name, "ggplot")
  expect_equal(cur_of(2)$path, integer())
  expect_equal(cur_of(2)$slot, "layers")
  expect_false(cur_of(2)$in_data_position)

  # 3: pipeline in data_arg position
  expect_equal(cur_of(3)$slot, "data_arg")
  expect_equal(cur_of(3)$layer_name, "ggplot")
  expect_equal(cur_of(3)$path, integer())
  expect_equal(cur_of(3)$param, "data")
  expect_true(cur_of(3)$in_data_position)

  # 4: stage 1 (the data source) — in_data_position carries; pipeline_index 1
  expect_equal(cur_of(4)$slot, "stages")
  expect_equal(cur_of(4)$path, 1L)
  expect_equal(cur_of(4)$param, "data")
  expect_true(cur_of(4)$in_data_position)
  expect_equal(cur_of(4)$pipeline_index, 1L)

  # 5: stage 2 (filter) — NOT in data position; pipeline_index 2; param passes through
  expect_equal(cur_of(5)$slot, "stages")
  expect_equal(cur_of(5)$path, 2L)
  expect_equal(cur_of(5)$param, "data")
  expect_false(cur_of(5)$in_data_position)
  expect_equal(cur_of(5)$pipeline_index, 2L)

  # 6: the `expr` placeholder inside filter()'s arg 1 — positional => param NA;
  #    not in data position (its enclosing stage isn't either)
  expect_equal(cur_of(6)$slot, "args")
  expect_equal(cur_of(6)$path, c(2L, 1L))
  expect_true(is.na(cur_of(6)$param))
  expect_false(cur_of(6)$in_data_position)
  expect_true(is.na(cur_of(6)$pipeline_index))

  # 7: aes() at children[[1]] named "mapping"
  expect_equal(cur_of(7)$slot, "children")
  expect_equal(cur_of(7)$path, 1L)
  expect_equal(cur_of(7)$param, "mapping")
  expect_false(cur_of(7)$in_data_position)

  # 8: the `var` consumer at aes(x = .) — path c(1,1), param "x"
  expect_equal(cur_of(8)$slot, "args")
  expect_equal(cur_of(8)$path, c(1L, 1L))
  expect_equal(cur_of(8)$param, "x")
  expect_false(cur_of(8)$in_data_position)
})

test_that("ptr_rewrite_pre does not descend into `upstream`", {
  visits <- collect_visits(build_cursor_tree())
  markers <- vapply(visits, function(v) v$node$marker %||% "", character(1))
  expect_false(any(markers == "SHOULD_NOT_BE_VISITED"))
})

test_that("a placeholder sitting directly as a layer keeps layer_name = keyword", {
  ph <- ptr_ph_data_source(keyword = "upload", expr = quote(upload))
  root <- ptr_root(layers = list(ph), expr = quote(upload))
  visits <- collect_visits(root)
  expect_equal(visits[[2L]]$cur$layer_name, "upload")
  expect_equal(visits[[2L]]$cur$path, integer())
})

test_that("ptr_rewrite_pre returns the (possibly stamped) tree and respects fn replacements", {
  root <- build_cursor_tree()
  stamped <- ptr_rewrite_pre(root, function(n, cur) {
    if (is_ptr_placeholder(n)) n$seen_param <- cur$param
    n
  })
  expect_equal(stamped$layers[[1L]]$children[[1L]]$args[[1L]]$seen_param, "x")
  expect_true(is_ptr_root(stamped))
})

# ---- tree validators -------------------------------------------------------

test_that("ptr_assert_ids_assigned aborts on an un-id'd placeholder", {
  root <- ptr_root(
    layers = list(ptr_layer(
      name = "ggplot", expr = quote(ggplot()),
      children = list(mapping = ptr_call(
        fun = quote(aes),
        args = list(x = ptr_ph_data_consumer(keyword = "var", expr = quote(x))),
        expr = quote(aes(x = x))
      ))
    )),
    expr = quote(ggplot())
  )
  expect_error(ptr_assert_ids_assigned(root), "without an id")
  expect_error(ptr_assert_ids_assigned(root), "var")
  # Once ids are assigned it passes.
  root2 <- ptr_assign_ids(root)
  expect_silent(ptr_assert_ids_assigned(root2))
})

test_that("ptr_assert_classified aborts on a consumer with no upstream and on a source with one", {
  consumer <- ptr_ph_data_consumer(keyword = "var", expr = quote(x))  # upstream NULL
  root <- ptr_root(layers = list(ptr_layer(
    name = "ggplot", expr = quote(ggplot()),
    children = list(mapping = ptr_call(fun = quote(aes), args = list(x = consumer), expr = quote(aes(x = x))))
  )), expr = quote(ggplot()))
  expect_error(ptr_assert_classified(root), "upstream")

  src <- ptr_ph_data_source(keyword = "upload", expr = quote(upload))
  src$upstream <- ptr_literal(expr = quote(mtcars))
  root2 <- ptr_root(layers = list(src), expr = quote(upload))
  expect_error(ptr_assert_classified(root2), "non-NULL `upstream`")
})

test_that("ptr_assert_no_placeholders aborts when a placeholder survives", {
  root <- ptr_root(layers = list(ptr_layer(
    name = "ggplot", expr = quote(ggplot()),
    children = list(mapping = ptr_call(
      fun = quote(aes),
      args = list(x = ptr_ph_value(keyword = "text", expr = quote("x"))),
      expr = quote(aes(x = "x"))
    ))
  )), expr = quote(ggplot()))
  expect_error(ptr_assert_no_placeholders(root), "unresolved placeholder")
  expect_error(ptr_assert_no_placeholders(root), "text")
})

test_that("ptr_assert_no_missing aborts when a ptr_missing survives pruning", {
  root <- ptr_root(layers = list(ptr_layer(
    name = "ggplot", expr = quote(ggplot()),
    children = list(ptr_missing())
  )), expr = quote(ggplot()))
  expect_error(ptr_assert_no_missing(root), "ptr_missing")
})

test_that("ptr_assert_acyclic aborts on a self-referential upstream", {
  consumer <- ptr_ph_data_consumer(id = "ggplot_1_1_var_NA", keyword = "var", expr = quote(x))
  consumer$upstream <- consumer
  root <- ptr_root(layers = list(ptr_layer(
    name = "ggplot", expr = quote(ggplot()),
    children = list(mapping = ptr_call(fun = quote(aes), args = list(x = consumer), expr = quote(aes(x = x))))
  )), expr = quote(ggplot()))
  expect_error(ptr_assert_acyclic(root), "visited twice")
})
