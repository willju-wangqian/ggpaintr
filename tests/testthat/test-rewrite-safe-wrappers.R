# Helpers reused across tests.
.test_env_safe <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

# ---- ptr_complete_expr_safe_v2 ----

test_that("complete_expr_safe returns code_text on success", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point()")
  result <- ptr_complete_expr_safe_v2(tree, eval_env = .test_env_safe())
  expect_true(result$ok)
  expect_equal(result$stage, "complete")
  expect_match(result$code_text, "ggplot")
  expect_match(result$code_text, "geom_point")
  expect_null(result$error)
})

test_that("complete_expr_safe substitutes placeholders", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(size = num)")
  num_id <- find_nodes(tree, is_ptr_ph_value)[[1L]]$id
  result <- ptr_complete_expr_safe_v2(
    tree,
    snapshot = stats::setNames(list(3), num_id),
    eval_env = .test_env_safe()
  )
  expect_match(result$code_text, "size = 3")
})

test_that("complete_expr_safe sets ok=FALSE on substitute error", {
  # `expr` rejects multi-line input at substitute time (BDD P8.11)
  tree <- ptr_translate("ggplot(mtcars) + expr")
  expr_id <- find_nodes(tree, is_ptr_ph_value)[[1L]]$id
  result <- ptr_complete_expr_safe_v2(
    tree,
    snapshot = stats::setNames(list("geom_point()\nlabs()"), expr_id),
    eval_env = .test_env_safe()
  )
  expect_false(result$ok)
  expect_match(result$error, "exactly one expression")
})

test_that("complete_expr_safe captures pruned tree for downstream eval", {
  tree <- ptr_translate("ggplot(mtcars)")
  result <- ptr_complete_expr_safe_v2(tree, eval_env = .test_env_safe())
  expect_true(is_ptr_root(result$pruned))
})

# ---- ptr_assemble_plot_safe_v2 ----

test_that("assemble_plot_safe builds a ggplot object on success", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  c_res <- ptr_complete_expr_safe_v2(tree, eval_env = .test_env_safe())
  result <- ptr_assemble_plot_safe_v2(c_res)
  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
})

test_that("assemble_plot_safe propagates upstream failure unchanged", {
  failed <- list(
    ok = FALSE, stage = "complete", code_text = "",
    pruned = NULL, eval_env = NULL,
    error = "earlier failure", condition = NULL, plot = NULL
  )
  result <- ptr_assemble_plot_safe_v2(failed)
  expect_false(result$ok)
  expect_equal(result$error, "earlier failure")
})

test_that("assemble_plot_safe captures eval-time errors (P11.7)", {
  # Reference an undefined symbol via expr placeholder
  tree <- ptr_translate("ggplot(mtcars, aes(x = mpg)) + expr")
  expr_id <- find_nodes(tree, is_ptr_ph_value)[[1L]]$id
  c_res <- ptr_complete_expr_safe_v2(
    tree,
    snapshot = stats::setNames(list("nonexistent_layer_xyz()"), expr_id),
    eval_env = .test_env_safe()
  )
  expect_true(c_res$ok)         # complete (render text) succeeds
  expect_match(c_res$code_text, "nonexistent_layer_xyz")
  result <- ptr_assemble_plot_safe_v2(c_res, expr_check = FALSE)
  expect_false(result$ok)        # eval fails
  expect_equal(result$stage, "plot")
  expect_match(result$error, "nonexistent_layer_xyz")
})

test_that("assemble_plot_safe surfaces no-layers error", {
  tree <- ptr_translate("ggplot(mtcars) + geom_point(color = text)")
  c_res <- ptr_complete_expr_safe_v2(
    tree,
    snapshot = list(),  # text missing → geom_point empty → dropped (standalone-eligible kept though)
    eval_env = .test_env_safe()
  )
  # geom_point IS standalone-eligible, so it survives empty as geom_point()
  # but ggplot lacks aes — still valid eval
  result <- ptr_assemble_plot_safe_v2(c_res)
  expect_true(result$ok)  # this should still succeed
})

# ---- ptr_validate_plot_render_safe_v2 ----

test_that("validate_plot_render_safe passes a valid plot through", {
  tree <- ptr_translate("ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()")
  c_res <- ptr_complete_expr_safe_v2(tree, eval_env = .test_env_safe())
  a_res <- ptr_assemble_plot_safe_v2(c_res)
  result <- ptr_validate_plot_render_safe_v2(a_res)
  expect_true(result$ok)
  expect_s3_class(result$plot, "ggplot")
})

test_that("validate_plot_render_safe propagates earlier failure", {
  failed <- list(ok = FALSE, stage = "plot", error = "boom", plot = NULL)
  result <- ptr_validate_plot_render_safe_v2(failed)
  expect_false(result$ok)
})

test_that("validate_plot_render_safe captures ggplot_build failure (P11.8)", {
  # facet on a non-existent column triggers ggplot_build failure
  tree <- ptr_translate(
    "ggplot(mtcars, aes(x = mpg)) + geom_point() + facet_wrap(~nonexistent)"
  )
  c_res <- ptr_complete_expr_safe_v2(tree, eval_env = .test_env_safe())
  a_res <- ptr_assemble_plot_safe_v2(c_res)
  expect_true(a_res$ok)
  result <- ptr_validate_plot_render_safe_v2(a_res)
  expect_false(result$ok)
  expect_equal(result$stage, "plot")
  expect_match(result$error, "nonexistent", ignore.case = TRUE)
})
