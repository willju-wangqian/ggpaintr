# PLAN-06 / ADR 0025 §6 -- ADR worked example #3 end-to-end. Two
# shinytest2 boots in one test:
#
#   boot-1: `ppUpload |> ggplot(...)` with `penguins.csv` uploaded and
#           the shortcut textbox left at "". Capture the spec-mode code
#           panel; parse out the `ptr_spec <- list(...)` block; persist
#           the parsed spec to disk for boot-2.
#   boot-2: a *fresh* AppDriver process whose `app.R` reads the persisted
#           spec, `assign()`s `read.csv(penguins)` into globalenv() under
#           the auto-name from boot-1, then calls
#           `ptr_app(formula, spec = spec)`. The shortcut textbox at
#           boot-2 must read the auto-name, AND the rendered host output
#           must contain a `<img>` (ggplot rendered) -- proving the
#           env-shortcut path resolved the caller-env frame.

extract_ptr_spec <- function(code_txt) {
  if (!is.character(code_txt) || length(code_txt) != 1L) return(NULL)
  if (!grepl("ptr_spec <- list", code_txt, fixed = TRUE)) return(NULL)
  exprs <- parse(text = code_txt, keep.source = FALSE)
  for (i in seq_along(exprs)) {
    ex <- exprs[[i]]
    if (is.call(ex) && identical(ex[[1L]], as.name("<-")) &&
        identical(ex[[2L]], as.name("ptr_spec"))) {
      return(eval(ex[[3L]]))
    }
  }
  NULL
}

test_that("ADR worked example #3: spec round-trip reproduces an uploaded session (two-boot)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  # -- boot-1: upload + spec capture ----------------------------------------
  app1 <- boot_vignette_app("plan06-roundtrip-boot1")

  src_id      <- "ggplot_0_ppUpload_NA"
  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"
  expect_dom_id(app1, src_id)
  expect_dom_id(app1, shortcut_id)

  csv_path <- normalizePath(
    testthat::test_path("fixtures", "vignette-apps",
                        "plan06-roundtrip-boot1", "penguins.csv"),
    mustWork = TRUE
  )
  # Upload the file; leave the shortcut textbox at default "".
  args <- stats::setNames(list(csv_path), src_id)
  do.call(app1$upload_file, args)
  app1$wait_for_idle(timeout = 15 * 1000)

  # Click Update and wait until rendered (the host output is the
  # standard ggplot renderPlot panel; id is the engine's host id).
  draw(app1, "ptr_update_plot")

  # Switch the code panel to spec mode and read its contents.
  app1$set_inputs(ptr_code_mode = "spec", wait_ = FALSE)
  app1$wait_for_idle(timeout = 15 * 1000)
  code_txt <- app1$get_value(output = "ptr_code")
  spec <- extract_ptr_spec(code_txt)
  expect_true(!is.null(spec), label = "boot-1 spec mode emits ptr_spec block")

  # The fallback target: the shortcut id must appear in the spec, with
  # a non-empty character value (the auto-name Plan 02 stamped). This
  # is the closed-loop CORE ASSERTION for boot-1.
  expect_true(
    shortcut_id %in% names(spec),
    label = sprintf("spec contains entry for %s", shortcut_id)
  )
  auto_name <- spec[[shortcut_id]]
  expect_true(
    is.character(auto_name) && length(auto_name) == 1L && nzchar(auto_name),
    label = "auto-name in spec is a non-empty single character string"
  )

  app1$stop()

  # -- boot-2: reproduce from spec + env binding ----------------------------
  spec_rds <- withr::local_tempfile(fileext = ".rds")
  saveRDS(spec, spec_rds)

  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  app_dir <- testthat::test_path("fixtures", "vignette-apps",
                                 "plan06-roundtrip-boot2")
  prune_dead_ggpaintr_resource_paths()
  withr::local_envvar(
    GGP_PKG          = pkg,
    PLAN06_SPEC_RDS  = spec_rds,
    PLAN06_AUTO_NAME = auto_name,
    PLAN06_CSV_PATH  = csv_path
  )
  app2 <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "e2e-plan06-roundtrip-boot2",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app2$stop())
  app2$wait_for_idle(timeout = 15 * 1000)

  # Then-1: the shortcut textbox at boot-2 reads the auto-name from
  # boot-1's spec dump (apply_spec_at_boot seeds it via the
  # source_companion path).
  expect_equal(
    app2$get_value(input = shortcut_id), auto_name,
    label = "boot-2 shortcut textbox seeded with auto-name from spec"
  )

  # Then-2: the env-shortcut path resolves the caller-env binding (we
  # `assign(auto_name, read.csv(csv_path), envir = globalenv())` in
  # boot-2's app.R) and the plot renders -- the host output contains a
  # base64 `<img>` (renderPlot output).
  draw(app2, "ptr_update_plot")
  expect_no_inline_error(app2, "ptr_error")
  expect_rendered(app2, "#ptr_plot", "ggplot")
})
