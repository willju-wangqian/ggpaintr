# helper-vignette-apps.R — boot + assertion helpers for the e2e browser suite
# (test-e2e-vignette-examples-shinytest2.R).
#
# Each covered vignette example is a self-contained Shiny app under
#   tests/testthat/fixtures/vignette-apps/<slug>/app.R
# whose body is verbatim-equivalent to the named vignette chunk (the >>> marker
# block in each app.R is reviewable by `diff` against the .Rmd). The app.R
# header loads the in-development package into the child app process via
# pkgload::load_all() so the browser test exercises *dev* source, not any
# stale system install of ggpaintr (a real hazard: a pre-redesign 0.9.1 is
# installed system-wide and would otherwise shadow the dev tree in the child).
#
# get_values() is deliberately NOT used: it snapshots *every* output, and the
# custom-renderer examples (l3-plotly / plotly-paintr / ggiraph-paintr) keep
# their host output in a shiny.silent.error state until the first draw, which
# makes the values endpoint emit invalid JSON. All assertions are targeted
# DOM (get_html) / single-output (get_value) reads instead.

# Boot one vignette-app fixture in a real headless-Chromium browser.
boot_vignette_app <- function(slug) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  withr::local_envvar(GGP_PKG = normalizePath(testthat::test_path("..", "..")))
  app_dir <- testthat::test_path("fixtures", "vignette-apps", slug)
  # The "Failed to locate globals" / htmlDependency-prefix warnings on
  # AppDriver$new() are benign and documented in project memory; scope the
  # suppression to construction only (assertion output is never suppressed).
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = paste0("e2e-", slug),
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop(), envir = parent.frame())
  app
}

# Assert a DOM element with the given Shiny id is present in the page.
expect_dom_id <- function(app, id) {
  html <- app$get_html(paste0("#", id))
  testthat::expect_true(
    !is.null(html) && nzchar(html),
    label = paste0("DOM id #", id, " present")
  )
}

# Set one input WITHOUT waiting for an output change. ggpaintr only re-renders
# on an explicit Update/Draw click, so setting a placeholder widget never
# updates an output by itself — wait_ = TRUE would (correctly) time out.
set_input <- function(app, id, value) {
  args <- stats::setNames(list(value), id)
  do.call(app$set_inputs, c(args, list(wait_ = FALSE)))
}

# Click a draw/update button and wait for the app to go idle.
draw <- function(app, button_id) {
  app$click(button_id)
  app$wait_for_idle(timeout = 25 * 1000)
}

# Assert the plot/host output at `selector` rendered the expected node kind.
#   kind = "ggplot"  -> a base64 <img> (renderPlot)
#          "plotly"  -> a plotly.js node
#          "ggiraph" -> a girafe / <svg> node
expect_rendered <- function(app, selector, kind = "ggplot") {
  html <- app$get_html(selector)
  pat <- switch(kind,
    ggplot  = "<img",
    plotly  = "plotly|js-plotly-plot",
    ggiraph = "girafe|<svg|ggiraph",
    stop("unknown kind")
  )
  testthat::expect_match(html %||% "", pat,
    info = paste0(selector, " rendered a ", kind, " node"))
}

# Assert the generated-code output is a non-empty string.
expect_code_nonempty <- function(app, output_id) {
  val <- app$get_value(output = output_id)
  testthat::expect_true(
    is.character(val) && length(val) == 1L && nzchar(val),
    label = paste0("generated-code output ", output_id, " is non-empty")
  )
}

# Assert no inline error banner is shown on the happy path. The error pane is
# a uiOutput; an active error renders the `.ptr-alert--error` block.
expect_no_inline_error <- function(app, error_output_id) {
  html <- app$get_html(paste0("#", error_output_id))
  testthat::expect_false(
    grepl("ptr-alert--error", html %||% "", fixed = TRUE),
    label = paste0("no inline error in #", error_output_id, " on happy path")
  )
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
