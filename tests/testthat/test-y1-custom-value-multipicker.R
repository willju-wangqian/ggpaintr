# Y1 regression test. A custom *value* placeholder backed by
# pickerInput(multiple = TRUE) -- the same widget shape that exposed
# the deselect bug on the consumer path. The built-in value
# placeholders (ppText/ppNum) never surfaced the bug in practice
# because their widgets emit "" / NA on clear (length 1, not the
# length-0 / NULL the legacy clobber required). This test confirms the
# value-path closure-flag at R/paintr-server.R:1475 covers a custom
# placeholder that DOES emit NULL on clear.

empty_picker <- function(v) {
  is.null(v) || (is.character(v) && length(v) == 0L)
}

test_that("Y1: custom value placeholder with pickerInput(multiple=TRUE) — deselect stays empty", {
  app <- boot_vignette_app("y1-custom-value-multipicker")
  id <- "geom_smooth_1_ppMultiPick_NA"

  # Boot: positional default ("a") seeds the picker.
  expect_equal(app$get_value(input = id), "a",
               label = "boot seed from ppMultiPick(\"a\")")

  # Pick a different option -> survives Update Plot.
  set_input(app, id, "b")
  draw(app, "ptr_update_plot")
  expect_equal(app$get_value(input = id), "b",
               label = "user pick survives Update Plot")

  # Deselect (pickerInput multi=TRUE -> Shiny server sees NULL after the
  # transport collapses []/null). Pre-closure-flag fix this snapped back
  # to "a" because the value-path call site couldn't distinguish first-
  # render NULL from cleared NULL. Post-fix: stays empty.
  set_input(app, id, character(0))
  draw(app, "ptr_update_plot")
  v <- app$get_value(input = id)
  expect_true(empty_picker(v),
              label = paste0("custom value picker stays empty after deselect (got: ",
                             deparse(v), ")"))
})
