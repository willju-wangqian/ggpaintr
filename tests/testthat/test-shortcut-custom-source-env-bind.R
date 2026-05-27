# test-shortcut-custom-source-env-bind.R -- shinytest2 driver for ADR 0025
# worked example #1 (PLAN-03). A custom `ppDataset` source opts in to the
# env-shortcut path with `shortcut = TRUE`; the fixture renders both
# selectInput(node$id) and textInput(node$shortcut_id), and exposes
# `mtcars` in caller env. Typing "mtcars" into the shortcut binds the
# caller-env frame; the downstream ppVar picker populates with
# names(mtcars); the plot renders; the code panel reads
# `mtcars |> ggplot(aes(x = mpg)) + geom_point()`.

test_that("custom ppDataset(shortcut=TRUE): typed env name binds caller-env frame end-to-end", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("shortcut-custom-source-env-bind")

  shortcut_id <- "ggplot_0_ppDataset_NA_shortcut"
  var_id      <- "ggplot_1_1_ppVar_NA"
  expect_dom_id(app, shortcut_id)

  # Type the caller-env frame name into the shortcut textbox.
  set_input(app, shortcut_id, "mtcars")
  app$wait_for_idle(timeout = 15 * 1000)

  # Switch to the layer's Controls subtab so the suspended ppVar
  # renderUI binds (the var picker for an in-aes ppVar is suspended
  # under the Data subtab -- see project memory
  # `shinytest2-appdir-pkgload`).
  set_input(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)

  draw(app, "ptr_update_plot")

  # Then 1: the downstream ppVar picker is populated with mtcars columns
  # (mpg, cyl, disp, hp, ...).  Spot-check three; "mpg" is the default
  # selection from the formula.
  expect_picker_populated(app, var_id, "mpg")
  # AND the positional default `ppVar(mpg)` is the SELECTED value, not
  # merely an option (the `_populated` proxy fired green on the
  # `aes(y = ppVar(adj))` derived-column regression at 2026-05-27).
  expect_picker_selected(app, var_id, "mpg")
  for (col in c("cyl", "disp", "hp")) {
    html <- app$get_html(paste0("#", var_id)) %||% ""
    expect_true(
      grepl(col, html, fixed = TRUE),
      label = paste0("ppVar picker offers names(mtcars) col '", col, "'")
    )
  }

  # Then 2: the rendered plot is non-empty (an <img> from renderPlot).
  expect_rendered(app, "#ptr_plot", "ggplot")

  # Then 3: the code panel mentions `mtcars` as the source symbol and the
  # picked column `mpg` in the aes.
  code <- app$get_value(output = "ptr_code")
  expect_true(
    is.character(code) && length(code) == 1L && nzchar(code),
    label = "code panel output is non-empty"
  )
  expect_match(code, "mtcars", fixed = TRUE)
  expect_match(code, "mpg", fixed = TRUE)
})
