# ppUpload-bug (2026-05-29): a layer-data `ppUpload()` with NO upload and an
# empty shortcut textbox must resolve to an UNRESOLVED data source -- the
# `data=` arg is dropped so the layer inherits the plot's data. Before the fix,
# `substitute_walk.ptr_ph_data_source()` unconditionally emitted the
# translate-stamped auto-name `df_<hash(node$id)>` whenever the shortcut
# snapshot was empty, even when nothing was bound, so eval blew up with
# `object 'df_<hash>' not found`.
#
# Seam: the headless runtime core (`ptr_run_formula` -> substitute -> prune ->
# render -> eval), the same pipeline `ptr_setup_runtime()` drives on the
# Update Plot click. This is the correct seam: it exercises substitute against
# the real default snapshot (shortcut id NULL == empty textbox) and actually
# evaluates the resulting tree, which is exactly where the original error fired.

test_that("layer-data ppUpload() with no upload drops data arg and renders", {
  f <- paste0(
    "ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point() + ",
    "geom_rug(data = ppUpload(), aes(x = mpg, y = wt), inherit.aes = FALSE)"
  )
  res <- ggpaintr:::ptr_run_formula(f, envir = new.env(parent = globalenv()))

  expect_true(isTRUE(res$ok))
  expect_null(res$error)
  # The dangling auto-name must NOT appear in the rendered code...
  expect_false(grepl("df_[0-9a-f]{6,}", res$code_text))
  # ...and the geom_rug layer must carry no `data =` arg (inherits plot data).
  expect_false(grepl("data\\s*=\\s*df_", res$code_text))
  expect_match(res$code_text, "geom_rug\\(")
})

test_that("layer-data ppUpload() resolves the bound frame once an upload binds", {
  # Discrimination arm: when an upload HAS bound a frame under the auto-name
  # in eval_env, the symbol survives and resolves (the fix must not over-drop).
  f <- paste0(
    "ggplot(mtcars, aes(x = mpg, y = wt)) + geom_point() + ",
    "geom_rug(data = ppUpload(), aes(x = mpg, y = wt), inherit.aes = FALSE)"
  )
  tree <- ggpaintr:::ptr_translate(f, expr_check = TRUE)
  # Recover the stamped auto-name for the ppUpload source node.
  srcs <- ggpaintr:::find_nodes(tree, ggpaintr:::is_ptr_ph_data_source)
  auto <- srcs[[1]]$auto_name
  expect_true(is.character(auto) && nzchar(auto))

  env <- new.env(parent = globalenv())
  assign(auto, data.frame(mpg = 1:3, wt = 4:6), envir = env)
  res <- ggpaintr:::ptr_run_formula(f, envir = env)

  expect_true(isTRUE(res$ok))
  expect_match(res$code_text, paste0("data = ", auto))
})
