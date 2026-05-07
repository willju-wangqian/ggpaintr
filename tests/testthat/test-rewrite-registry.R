# Registry constructor validation. Phase 1 covers the constructor surface
# (keyword shape, hook arity, copy_defaults shape, role assignment).

test_that("ptr_define_placeholder_value rejects bad keyword", {
  expect_error(ptr_define_placeholder_value(
    keyword = "", build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ))
  expect_error(ptr_define_placeholder_value(
    keyword = NA_character_, build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ))
  expect_error(ptr_define_placeholder_value(
    keyword = c("a", "b"), build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ))
  expect_error(ptr_define_placeholder_value(
    keyword = "if", build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ), "reserved")
  expect_error(ptr_define_placeholder_value(
    keyword = "1bad", build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ), "valid R name")
})

test_that("hook arity validation: required arg missing without dots aborts", {
  expect_error(ptr_define_placeholder_value(
    keyword = "abc",
    build_ui = function(other) NULL,
    resolve_expr = function(value, node) value
  ), "build_ui")
  expect_error(ptr_define_placeholder_value(
    keyword = "abc",
    build_ui = function(node) NULL,
    resolve_expr = function(other) other
  ), "resolve_expr")
})

test_that("hook arity: dots accepted as substitute for required args", {
  expect_silent(ptr_define_placeholder_value(
    keyword = "okwithdots",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ))
  expect_silent(ptr_define_placeholder_value(
    keyword = "okfullargs",
    build_ui = function(node) NULL,
    resolve_expr = function(value, node) value
  ))
  ptr_registry_v2_clear()
})

test_that("hook with only `...` warns about hidden required args", {
  expect_warning(ptr_define_placeholder_value(
    keyword = "dotsonly",
    build_ui = function(...) NULL,
    resolve_expr = function(value, node, ...) value
  ), "node")
  ptr_registry_v2_clear()
})

test_that("copy_defaults validation: rejects non-list", {
  expect_error(ptr_define_placeholder_value(
    keyword = "k1",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value,
    copy_defaults = "label"
  ), "named list")
})

test_that("copy_defaults validation: rejects non-string values", {
  expect_error(ptr_define_placeholder_value(
    keyword = "k2",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value,
    copy_defaults = list(label = 42)
  ), "single non-NA string")
})

test_that("copy_defaults validation: rejects unnamed entries", {
  expect_error(ptr_define_placeholder_value(
    keyword = "k3",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value,
    copy_defaults = list("naked")
  ))
})

test_that("consumer constructor stores role and validate_input", {
  ptr_registry_v2_clear()
  ptr_define_placeholder_consumer(
    keyword = "mycol",
    build_ui = function(node, cols, ...) NULL,
    resolve_expr = function(value, node, ...) rlang::sym(value),
    validate_input = function(value, upstream_cols) value %in% upstream_cols
  )
  e <- ptr_registry_v2_lookup("mycol")
  expect_equal(e$role, "consumer")
  expect_true(e$data_aware)
  expect_true(is.function(e$validate_input))
  ptr_registry_v2_clear()
})

test_that("source constructor defaults resolve_expr to symbol of value", {
  ptr_registry_v2_clear()
  ptr_define_placeholder_source(
    keyword = "mysrc",
    build_ui = function(node, ...) NULL,
    resolve_data = function(value, node, ...) data.frame(x = 1:3),
    companion_id_fn = function(id) paste0(id, "_companion")
  )
  e <- ptr_registry_v2_lookup("mysrc")
  expect_equal(e$role, "source")
  expect_true(e$data_aware)
  expect_true(is.function(e$resolve_expr))
  expect_equal(e$resolve_expr("foo"), rlang::sym("foo"))
  expect_equal(e$companion_id_fn("abc"), "abc_companion")
  ptr_registry_v2_clear()
})

test_that("translate dispatches placeholders to correct node class via role", {
  ptr_registry_v2_clear()
  ptr_register_builtins()
  r <- ptr_translate("ggplot(my_local_df, aes(x = var, y = num)) + geom_point(color = text)")
  values <- find_nodes(r, is_ptr_ph_value)
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  sources <- find_nodes(r, is_ptr_ph_data_source)
  expect_equal(length(consumers), 1L)
  expect_equal(consumers[[1]]$keyword, "var")
  expect_true(any(vapply(values, function(v) v$keyword == "num", logical(1))))
  expect_true(any(vapply(values, function(v) v$keyword == "text", logical(1))))
  expect_equal(length(sources), 0L)
})

test_that("registering same keyword twice warns", {
  ptr_registry_v2_clear()
  ptr_define_placeholder_value(
    keyword = "dup",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  expect_warning(ptr_define_placeholder_value(
    keyword = "dup",
    build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  ), "Overwriting")
  ptr_registry_v2_clear()
})
