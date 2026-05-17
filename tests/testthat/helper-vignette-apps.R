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
  # Source-root guard. The fixture app.R boots via
  # pkgload::load_all(Sys.getenv("GGP_PKG")); that needs the package SOURCE
  # tree (a DESCRIPTION at GGP_PKG). `skip_on_cran()` above does NOT cover
  # `devtools::check()`: devtools runs the check subprocess with
  # NOT_CRAN=true, so skip_on_cran() does not fire even under --as-cran, and
  # the fixtures would otherwise run inside the .Rcheck sandbox where
  # test_path("..","..") has no DESCRIPTION -> pkgload::load_all() ERRORs.
  # Skipping on absent DESCRIPTION turns that into a clean SKIP under check
  # while the NOT_CRAN=true test_dir authoritative gate (source tree present)
  # still RUNs every browser test. See CLAUDE.md authoritative-gate block.
  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  testthat::skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "e2e vignette-app boot needs the package source root (pkgload::load_all); absent under the R CMD check .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
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

# Assert a rendered consumer/shared picker is *populated* — its element holds
# a real column choice, not an empty uiOutput. A blank uiOutput still has its
# container id present (so plain expect_dom_id passes), which is precisely how
# bug B1 (formula-local var(shared=) never bound in the embed path) survived
# every prior test. This asserts an actual rendered choice for a known column.
expect_picker_populated <- function(app, input_id, choice) {
  html <- app$get_html(paste0("#", input_id))
  testthat::expect_true(
    !is.null(html) && nzchar(html) && grepl(choice, html, fixed = TRUE),
    label = paste0("picker #", input_id, " populated (offers \"",
                   choice, "\"), not an empty uiOutput")
  )
}

# Assert no DOM element with the given Shiny id exists (the inverse of
# expect_dom_id). Used to prove a panel key is NOT double-rendered inline.
expect_no_dom_id <- function(app, id) {
  html <- app$get_html(paste0("#", id))
  testthat::expect_true(
    is.null(html) || !nzchar(html),
    label = paste0("DOM id #", id, " absent")
  )
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
