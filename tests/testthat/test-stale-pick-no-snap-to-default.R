# Tier-3 regression test (X1): stale-pick on upstream data swap must
# drop cleanly without snapping back to the placeholder default.
#
# Scenario: the formula declares `ggplot(dataset, aes(x = ppVar(mpg)))`
# with a custom source `dataset` that switches between mtcars (has
# "mpg") and iris (does not). The user can pick "mpg" while on mtcars;
# after switching to iris, "mpg" is no longer a valid column and the
# picker must reflect that -- empty, not the default.
#
# This used to be hidden by the same bug as the deselect case: any
# re-render where `extra$selected` came out empty would fall back to
# `node$default`. With the closure-flag fix, `has_rendered = TRUE` on
# the post-swap re-render so `current = "mpg"` passes through verbatim;
# ptr_builtin_var_build_ui's `intersect("mpg", iris_cols) = character(0)`
# then drops it at the hook layer. No default re-injection anywhere.

empty_picker <- function(v) {
  is.null(v) || (is.character(v) && length(v) == 0L)
}

test_that("X1: upstream data swap drops the stale pick without snapping to default", {
  app <- boot_vignette_app("stale-pick-no-snap-to-default")
  ds_id <- "ggplot_0_dataset_NA"
  x_id  <- "ggplot_1_1_ppVar_NA"

  # Boot: dataset = mtcars, x picker seeded with default "mpg".
  expect_equal(app$get_value(input = ds_id), "mtcars",
               label = "boot dataset is mtcars")
  expect_equal(app$get_value(input = x_id), "mpg",
               label = "x picker boots seeded to ppVar(mpg)")

  # Swap dataset to iris (which has no "mpg" column) and Update Plot.
  set_input(app, ds_id, "iris")
  draw(app, "ptr_update_plot")

  # Stale-pick must drop, NOT snap back to default.
  v <- app$get_value(input = x_id)
  expect_true(empty_picker(v),
              label = paste0("x picker empties after dataset swap (got: ",
                             deparse(v), ")"))

  # Defense in depth: pick a column that IS in iris, then swap BACK to
  # mtcars. Sepal.Length isn't in mtcars, so this is the same shape in
  # the other direction.
  set_input(app, x_id, "Sepal.Length")
  draw(app, "ptr_update_plot")
  expect_equal(app$get_value(input = x_id), "Sepal.Length",
               label = "iris pick survives Update Plot")

  set_input(app, ds_id, "mtcars")
  draw(app, "ptr_update_plot")
  v <- app$get_value(input = x_id)
  expect_true(empty_picker(v),
              label = paste0("x picker empties after swap back to mtcars (got: ",
                             deparse(v), ")"))
})
