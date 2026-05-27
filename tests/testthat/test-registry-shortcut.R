# PLAN-01 / ADR 0025 §1 — registry surface rename: `shortcut = TRUE` replaces
# the removed `companion_id_fn` argument on ptr_define_placeholder_source().

test_that("ptr_define_placeholder_source accepts shortcut = TRUE", {
  ptr_registry_clear()
  withr::defer(ptr_registry_clear())
  ptr_define_placeholder_source(
    keyword = "ppShortcutX",
    build_ui = function(node, label = NULL, ...) NULL,
    resolve_data = function(value, node, ...) data.frame(x = 1:3),
    resolve_expr = function(value, node, ...) rlang::sym(value),
    shortcut = TRUE
  )
  e <- ptr_registry_lookup("ppShortcutX")
  expect_identical(e$shortcut, TRUE)
  # The removed slot must not be stamped on the entry.
  expect_null(e$companion_id_fn)
})

test_that("ptr_define_placeholder_source defaults shortcut to FALSE", {
  ptr_registry_clear()
  withr::defer(ptr_registry_clear())
  ptr_define_placeholder_source(
    keyword = "ppNoShortcut",
    build_ui = function(node, label = NULL, ...) NULL,
    resolve_data = function(value, node, ...) data.frame(x = 1:3)
  )
  e <- ptr_registry_lookup("ppNoShortcut")
  expect_identical(e$shortcut, FALSE)
})

test_that("ptr_define_placeholder_source rejects the removed companion_id_fn arg", {
  ptr_registry_clear()
  withr::defer(ptr_registry_clear())
  expect_error(
    ptr_define_placeholder_source(
      keyword = "ppRejectedOldArg",
      build_ui = function(node, label = NULL, ...) NULL,
      resolve_data = function(value, node, ...) data.frame(x = 1:3),
      companion_id_fn = function(id) paste0(id, "_x")
    ),
    regexp = "unused argument|companion_id_fn"
  )
})

test_that("ptr_define_placeholder_source rejects non-logical shortcut", {
  ptr_registry_clear()
  withr::defer(ptr_registry_clear())
  expect_error(
    ptr_define_placeholder_source(
      keyword = "ppBadShortcut",
      build_ui = function(node, label = NULL, ...) NULL,
      resolve_data = function(value, node, ...) data.frame(x = 1:3),
      shortcut = "yes"
    ),
    regexp = "single logical"
  )
})

test_that("ppUpload node carries shortcut_id and no companion_id after translation", {
  ptr_registry_clear()
  ptr_register_builtins()
  withr::defer(ptr_registry_clear())
  tree <- ptr_translate("ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()")
  srcs <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(srcs, 1L)
  node <- srcs[[1L]]
  expect_equal(node$shortcut_id, paste0(node$id, "_shortcut"))
  expect_true(endsWith(node$shortcut_id, "_shortcut"))
  expect_false(endsWith(node$shortcut_id, "_name"))
  expect_null(node$companion_id)
})
