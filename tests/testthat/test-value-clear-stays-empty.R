# End-to-end regression tests for the value-path call sites of the
# default-fallback fix. Mirrors test-consumer-deselect-stays-empty.R
# but exercises the two value renderUIs:
#   - ptr_setup_value_uis (non-shared value loop, R/paintr-server.R:1475)
#   - ptr_setup_value_uis (shared-value loop,    R/paintr-server.R:1561)
#
# These widgets emit "" (textInput on clear) or NA_real_ (numericInput on
# clear) -- both have length 1, so the original `length == 0L` clobber
# in invoke_build_ui never fired here. The post-fix code is symmetric
# across all four call sites; this file pins the value-path behavior so
# a regression at either value call site shows up here.

empty_string <- function(v) is.character(v) && length(v) == 1L && !nzchar(v)

test_that("non-shared value: clear ppText + Update Plot stays empty (does not snap to default)", {
  app <- boot_vignette_app("value-clear-stays-empty")
  txt_id <- "geom_smooth_1_ppText_NA"

  # Boot: positional default seeds the textInput.
  expect_equal(app$get_value(input = txt_id), "lm",
               label = "boot seed from ppText(\"lm\")")

  # Edit -> persist.
  set_input(app, txt_id, "loess")
  draw(app, "ptr_update_plot")
  expect_equal(app$get_value(input = txt_id), "loess",
               label = "user edit survives Update Plot")

  # Clear -> stays empty. Pre-fix: branch B in invoke_build_ui would
  # require length == 0; "" has length 1 so no snap-back. Post-fix:
  # has_rendered + verbatim passthrough still keeps "" stable.
  set_input(app, txt_id, "")
  draw(app, "ptr_update_plot")
  v <- app$get_value(input = txt_id)
  expect_true(empty_string(v),
              label = paste0("ppText stays empty after clear (got: ", deparse(v), ")"))
})

test_that("shared value: clear ppNum + Update Plot stays NA (does not snap to first-occurrence default)", {
  app <- boot_vignette_app("value-clear-stays-empty")
  num_id <- "shared_lw"

  # Boot: shared widget seeds from first occurrence's default ("2" for
  # geom_smooth(linewidth = ppNum(2, shared = "lw"))).
  expect_equal(app$get_value(input = num_id), 2L,
               label = "shared ppNum boot-seeds from first occurrence")

  set_input(app, num_id, 5)
  draw(app, "ptr_update_plot")
  expect_equal(app$get_value(input = num_id), 5L,
               label = "shared ppNum user edit survives Update Plot")

  # Clear (numericInput emits NA_real_).
  set_input(app, num_id, NA_real_)
  draw(app, "ptr_update_plot")
  v <- app$get_value(input = num_id)
  expect_true(
    is.na(v) || (is.numeric(v) && length(v) == 1L && is.na(v)),
    label = paste0("shared ppNum stays NA after clear (got: ", deparse(v), ")")
  )
})
