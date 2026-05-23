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
  ptr_registry_clear()
})

test_that("hook with only `...` warns about hidden required args", {
  expect_warning(ptr_define_placeholder_value(
    keyword = "dotsonly",
    build_ui = function(...) NULL,
    resolve_expr = function(value, node, ...) value
  ), "node")
  ptr_registry_clear()
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
  ptr_registry_clear()
  ptr_define_placeholder_consumer(
    keyword = "mycol",
    build_ui = function(node, cols, ...) NULL,
    resolve_expr = function(value, node, ...) rlang::sym(value),
    validate_input = function(value, ctx) value %in% ctx$upstream_cols
  )
  e <- ptr_registry_lookup("mycol")
  expect_equal(e$role, "consumer")
  expect_true(e$data_aware)
  expect_true(is.function(e$validate_input))
  ptr_registry_clear()
})

test_that("source constructor defaults resolve_expr to symbol of value", {
  ptr_registry_clear()
  ptr_define_placeholder_source(
    keyword = "mysrc",
    build_ui = function(node, ...) NULL,
    resolve_data = function(value, node, ...) data.frame(x = 1:3),
    companion_id_fn = function(id) paste0(id, "_companion")
  )
  e <- ptr_registry_lookup("mysrc")
  expect_equal(e$role, "source")
  expect_true(e$data_aware)
  expect_true(is.function(e$resolve_expr))
  expect_equal(e$resolve_expr("foo"), rlang::sym("foo"))
  expect_equal(e$companion_id_fn("abc"), "abc_companion")
  ptr_registry_clear()
})

test_that("translate dispatches placeholders to correct node class via role", {
  ptr_registry_clear()
  ptr_register_builtins()
  r <- ptr_translate("ggplot(my_local_df, aes(x = ppVar, y = ppNum)) + geom_point(color = ppText)")
  values <- find_nodes(r, is_ptr_ph_value)
  consumers <- find_nodes(r, is_ptr_ph_data_consumer)
  sources <- find_nodes(r, is_ptr_ph_data_source)
  expect_equal(length(consumers), 1L)
  expect_equal(consumers[[1]]$keyword, "ppVar")
  expect_true(any(vapply(values, function(v) v$keyword == "ppNum", logical(1))))
  expect_true(any(vapply(values, function(v) v$keyword == "ppText", logical(1))))
  expect_equal(length(sources), 0L)
})

test_that("registering same keyword twice warns", {
  ptr_registry_clear()
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
  ptr_registry_clear()
})

# ---- Phase 5.1 — copy-field whitelist at placeholder definition ----

test_that("validate_copy_defaults rejects unknown copy fields", {
  withr::defer({
    if (exists("ptr_registry_clear")) ptr_registry_clear()
    ptr_register_builtins()
  })
  expect_error(
    ptr_define_placeholder_value(
      "kw_bad",
      build_ui = function(node, ...) NULL,
      resolve_expr = function(value, node, ...) value,
      copy_defaults = list(tooltip = "x")
    ),
    "unsupported field"
  )
})

test_that("validate_copy_defaults accepts every supported leaf field", {
  withr::defer({
    if (exists("ptr_registry_clear")) ptr_registry_clear()
    ptr_register_builtins()
  })
  expect_silent(
    ptr_define_placeholder_value(
      "kw_ok",
      build_ui = function(node, ...) NULL,
      resolve_expr = function(value, node, ...) value,
      copy_defaults = list(label = "L", help = "H", placeholder = "P", empty_text = "E")
    )
  )
})

# ---- ptr_clear_placeholder ----

.clean_registry <- function() {
  withr::defer_parent({
    if (exists("ptr_registry_clear")) ptr_registry_clear()
    ptr_register_builtins()
  })
}

test_that("ptr_clear_placeholder(keyword) removes one user placeholder", {
  .clean_registry()
  ptr_define_placeholder_value(
    "kw_one", build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  ptr_define_placeholder_value(
    "kw_two", build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  expect_true("kw_one" %in% ptr_registry_keywords())
  expect_message(ret <- ptr_clear_placeholder("kw_one"), "kw_one")
  expect_equal(ret, "kw_one")
  expect_false("kw_one" %in% ptr_registry_keywords())
  expect_true("kw_two" %in% ptr_registry_keywords())
  expect_true(all(ptr_builtin_keywords() %in% ptr_registry_keywords()))
})

test_that("ptr_clear_placeholder() removes every user placeholder, keeps built-ins", {
  .clean_registry()
  ptr_define_placeholder_value(
    "kw_a", build_ui = function(node, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  ptr_define_placeholder_consumer(
    "kw_b", build_ui = function(node, cols, data, ...) NULL,
    resolve_expr = function(value, node, ...) value
  )
  expect_message(ret <- ptr_clear_placeholder(), "kw_a")
  expect_setequal(ret, c("kw_a", "kw_b"))
  expect_setequal(ptr_registry_keywords(), ptr_builtin_keywords())
})

test_that("ptr_clear_placeholder() errors on an unknown keyword", {
  .clean_registry()
  expect_error(ptr_clear_placeholder("no_such_kw"), "no_such_kw")
})

test_that("ptr_clear_placeholder() refuses to clear a built-in placeholder", {
  .clean_registry()
  expect_error(ptr_clear_placeholder("ppVar"), "built-in")
  expect_true("ppVar" %in% ptr_registry_keywords())
})

test_that("ptr_clear_placeholder() with nothing to clear informs and returns empty", {
  if (exists("ptr_registry_clear")) ptr_registry_clear()
  ptr_register_builtins()
  .clean_registry()
  expect_message(ret <- ptr_clear_placeholder(), "[Nn]o user-registered")
  expect_equal(ret, character())
})
