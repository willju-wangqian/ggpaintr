# P3 — shared-binding. Groups placeholders by `shared` key; assigns one
# canonical id per group; rewrites every member's `id`.

test_that("P3.1 two var nodes with same shared key share one canonical id", {
  r <- ptr_translate(
    'ggplot(aes(x = var(shared = "axis"), y = var(shared = "axis"))) + geom_point()'
  )
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 2L)
  expect_equal(consumers[[1]]$id, consumers[[2]]$id)
  expect_equal(consumers[[1]]$id, "shared_axis")
})

test_that("P3.2 different shared keys remain distinct", {
  r <- ptr_translate(
    'ggplot(aes(x = var(shared = "x"), y = var(shared = "y"))) + geom_point()'
  )
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  ids <- vapply(consumers, function(c) c$id, character(1))
  expect_equal(length(unique(ids)), 2L)
})

test_that("P3.3 shared metadata surfaces in runtime input spec", {
  r <- ptr_translate(
    'ggplot(mtcars, aes(x = mpg)) + geom_point(size = num(shared = "size_filter"))'
  )
  spec <- ptr_runtime_input_spec(r)
  ph_rows <- spec[spec$role %in% "placeholder", ]
  size_rows <- ph_rows[ph_rows$keyword == "num", ]
  expect_equal(nrow(size_rows), 1L)
  expect_equal(size_rows$shared, "size_filter")
})

test_that("P3.4 upload companion row carries shared", {
  r <- ptr_translate('ggplot(data = upload(shared = "ds"))')
  spec <- ptr_runtime_input_spec(r)
  upload_rows <- spec[spec$keyword %in% "upload", ]
  expect_equal(nrow(upload_rows), 2L)  # placeholder + companion
  expect_true(all(upload_rows$shared == "ds"))
})

test_that("P3.5 bare-symbol placeholder has shared = NA in input spec", {
  r <- ptr_translate("ggplot(aes(x = var)) + geom_point()")
  spec <- ptr_runtime_input_spec(r)
  var_rows <- spec[spec$keyword %in% "var", ]
  expect_equal(nrow(var_rows), 1L)
  expect_true(is.na(var_rows$shared))
})

test_that("P3 bare-symbol placeholders pass through unchanged", {
  r <- ptr_translate("ggplot(mtcars, aes(x = var)) + geom_point()")
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  expect_equal(length(consumers), 1L)
  expect_null(consumers[[1]]$shared)
  # Bare-symbol id is the raw layer-encoded id, NOT prefixed with "shared_".
  expect_false(startsWith(consumers[[1]]$id, "shared_"))
})

# ---- P3.6+ — ptr_validate_shared_bindings + runtime fallback ----

test_that("P3.6 missing shared binding falls back to ptr_missing at substitute", {
  # Formula carries `shared = "axis"` but caller supplies no matching
  # entry in shared_bindings → P8 should treat the placeholder as missing.
  r <- ptr_translate(
    'ggplot(mtcars, aes(x = var(shared = "axis"))) + geom_point()'
  )
  s <- ptr_substitute(r, input_snapshot = list(), shared_bindings = list())
  missings <- find_nodes(s, is_ptr_missing)
  expect_gte(length(missings), 1L)
})

test_that("P3.7 ptr_validate_shared_bindings accepts NULL and empty list", {
  expect_identical(ptr_validate_shared_bindings(NULL), list())
  expect_identical(ptr_validate_shared_bindings(list()), list())
})

test_that("P3.8 ptr_validate_shared_bindings rejects non-list", {
  expect_error(ptr_validate_shared_bindings("not-a-list"), "list")
})

test_that("P3.9 ptr_validate_shared_bindings requires names", {
  unnamed <- list(shiny::reactive(1), shiny::reactive(2))
  expect_error(ptr_validate_shared_bindings(unnamed), "name")
})

test_that("P3.10 ptr_validate_shared_bindings rejects duplicate names", {
  dup <- list(a = shiny::reactive(1), a = shiny::reactive(2))
  expect_error(ptr_validate_shared_bindings(dup), "duplicate|unique")
})

test_that("P3.11 ptr_validate_shared_bindings rejects non-reactive values", {
  bad <- list(a = 1, b = "two")
  expect_error(ptr_validate_shared_bindings(bad), "reactive")
})
