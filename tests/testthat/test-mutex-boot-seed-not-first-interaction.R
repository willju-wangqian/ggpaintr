# test-mutex-boot-seed-not-first-interaction.R -- shinytest2 driver for
# ADR 0025 §2 boot-time exception (PLAN-03). `ppUpload(penguins)` seeds the
# shortcut textInput with "penguins" at boot via ADR 0010's default-arg
# seeding path; that seed write must NOT count as a first interaction and
# must NOT shiny::reset() the already-empty sibling fileInput. The two new
# mutex observers gate on `ignoreInit = TRUE` precisely to enforce this.
#
# Then we drive a real user file pick after boot and assert it silently
# wipes the seeded textbox (the boot seed loses to the explicit upload).

test_that("ppUpload(penguins) boot seed does not trip mutex; later upload still wipes the seeded textbox", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  app <- boot_vignette_app("mutex-boot-seed-not-first-interaction")

  src_id      <- "ggplot_0_ppUpload_NA"
  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"
  expect_dom_id(app, src_id)
  expect_dom_id(app, shortcut_id)

  # At boot: shortcut shows the seeded bareword "penguins" AND the
  # fileInput is empty + live (no shiny::reset fired against it, no
  # greyed state).
  expect_equal(
    app$get_value(input = shortcut_id), "penguins",
    label = "shortcut textInput seeded with bareword 'penguins' from ppUpload(penguins)"
  )
  expect_null(
    app$get_value(input = src_id),
    label = "fileInput is still NULL/live at boot (boot seed did NOT trip mutex)"
  )
  # The fileInput container should not carry any disabled / greyed marker;
  # the rendered HTML is non-empty and does not contain a `disabled` attr.
  src_html <- app$get_html(paste0("#", src_id)) %||% ""
  expect_true(
    nzchar(src_html),
    label = "fileInput rendered HTML is non-empty (interactable at boot)"
  )
  expect_false(
    grepl("disabled", src_html, fixed = TRUE),
    label = "fileInput is not in a disabled/greyed state at boot"
  )

  # Now exercise the real first interaction: pick a file. The file-pick
  # observer fires once (this IS the first non-ignoreInit event) and wipes
  # the seeded textbox.
  csv_path <- testthat::test_path(
    "fixtures", "vignette-apps", "mutex-boot-seed-not-first-interaction",
    "penguins.csv"
  )
  upload_file(app, ggplot_0_ppUpload_NA = csv_path)
  app$wait_for_idle(timeout = 15 * 1000)

  expect_equal(
    app$get_value(input = shortcut_id), "",
    label = "seeded textbox cleared by a real (post-boot) file pick"
  )
})
