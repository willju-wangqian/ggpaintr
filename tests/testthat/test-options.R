restore_options <- function(envir = parent.frame()) {
  withr::local_options(
    list(
      ggpaintr.verbose = NULL,
      ggpaintr.gate_draw = NULL,
      ggpaintr.suppress_warnings = NULL
    ),
    .local_envir = envir
  )
}

# ---------------------------------------------------------------------------
# ptr_options() — getter
# ---------------------------------------------------------------------------

test_that("ptr_options() with no args returns current settings as named list", {
  restore_options()
  out <- ptr_options()
  expect_type(out, "list")
  expect_setequal(names(out), c("verbose", "gate_draw", "suppress_warnings"))
  expect_false(out$verbose)
})

# ---------------------------------------------------------------------------
# gate_draw — default + UI/runtime contract
# ---------------------------------------------------------------------------

test_that("gate_draw defaults to TRUE", {
  restore_options()
  expect_true(ptr_options()$gate_draw)
  expect_true(ptr_get_setting(ptr_settings$gate_draw))
})

test_that("ptr_options(gate_draw = FALSE) sets the underlying option", {
  restore_options()
  ptr_options(gate_draw = FALSE)
  expect_false(getOption("ggpaintr.gate_draw"))
  expect_false(ptr_get_setting(ptr_settings$gate_draw))
})

# ---------------------------------------------------------------------------
# suppress_warnings — default + the plot-draw suppression contract
# ---------------------------------------------------------------------------

test_that("suppress_warnings defaults to FALSE", {
  restore_options()
  expect_false(ptr_options()$suppress_warnings)
  expect_false(ptr_get_setting(ptr_settings$suppress_warnings))
})

test_that("ptr_render_plot_value suppresses draw warnings only when TRUE", {
  # A plot that genuinely warns when DRAWN: loess on 2 points per group
  # (cyl splits into singleton-ish groups) -> the same loess warnings the
  # user reported. The warning fires at print/build time, not construction.
  df <- data.frame(x = c(1, 2, 1, 2), y = c(1, 2, 3, 4), g = c("a", "a", "b", "b"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, y, colour = g)) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x)

  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())

  # The plot really warns when drawn -- proves the test has teeth (loess
  # emits several; capture_warnings collects all so none leak as test noise).
  expect_true(length(testthat::capture_warnings(print(p))) > 0L)

  # FALSE path: returns the object unchanged, deferring the print (and thus
  # the warning) to shiny -- behavior is byte-identical to pre-option.
  expect_identical(ptr_render_plot_value(p, FALSE), p)

  # TRUE path: draws here under suppressWarnings -> nothing escapes, and it
  # returns NULL so shiny uses the device drawing rather than re-printing.
  expect_no_warning(out <- ptr_render_plot_value(p, TRUE))
  expect_null(out)
})

test_that("the Update plot button is rendered iff gate_draw is TRUE", {
  restore_options()
  f <- "ggplot(mtcars, aes(x = ppVar(mpg))) + geom_histogram()"

  ptr_options(gate_draw = TRUE)
  ui_on <- as.character(ptr_ui_controls(f, id = "g"))
  expect_match(ui_on, "g-ptr_update_plot", fixed = TRUE)

  ptr_options(gate_draw = FALSE)
  ui_off <- as.character(ptr_ui_controls(f, id = "g"))
  expect_no_match(ui_off, "ptr_update_plot")
})

test_that("ptr_options() reflects values set via base options()", {
  withr::local_options(
    ggpaintr.verbose = FALSE
  )
  out <- ptr_options()
  expect_false(out$verbose)
})

# ---------------------------------------------------------------------------
# ptr_options() — setter
# ---------------------------------------------------------------------------

test_that("ptr_options(name = value) sets the underlying option", {
  restore_options()
  ptr_options(verbose = FALSE)
  expect_false(getOption("ggpaintr.verbose"))
  expect_false(ptr_options()$verbose)
})

test_that("ptr_options() setting returns previous values invisibly", {
  restore_options()
  prev <- withVisible(ptr_options(verbose = TRUE))
  expect_false(prev$visible)
  expect_identical(prev$value$verbose, FALSE)
})

test_that("ptr_options() round-trip restores prior state", {
  restore_options()
  ptr_options(verbose = TRUE)
  old <- ptr_options(
    verbose = FALSE
  )
  expect_false(getOption("ggpaintr.verbose"))
  do.call(ptr_options, old)
  expect_true(getOption("ggpaintr.verbose"))
})

# ---------------------------------------------------------------------------
# ptr_options() — validation
# ---------------------------------------------------------------------------

test_that("ptr_options() errors on unknown setting name", {
  restore_options()
  expect_error(ptr_options(verbose = TRUE, nonsense = FALSE), "nonsense")
})

test_that("ptr_options() errors on non-logical value", {
  restore_options()
  expect_error(ptr_options(verbose = "off"), "logical")
  expect_error(ptr_options(verbose = NA), "NA|logical")
  expect_error(ptr_options(verbose = c(TRUE, FALSE)), "single")
})

# ---------------------------------------------------------------------------
# ADR 0020 / Plan 04: the deprecated `checkbox_default_all_other_layer`
# option was removed straight out — `ptr_options()` must reject the name.
# ---------------------------------------------------------------------------

test_that("ptr_options() errors on the removed checkbox_default_all_other_layer option", {
  restore_options()
  expect_error(
    ptr_options(checkbox_default_all_other_layer = FALSE),
    "checkbox_default_all_other_layer"
  )
  expect_false(
    "checkbox_default_all_other_layer" %in% names(ptr_options())
  )
})
