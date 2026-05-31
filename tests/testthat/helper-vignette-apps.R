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
  # Close the resourcePath-leak isolation asymmetry: an earlier `css =` test
  # can leave a process-global `ggpaintr-user-*` resource path pointing at a
  # torn-down withr tempdir; AppDriver$new() replays parent resourcePaths()
  # into the child and a dead entry aborts the child boot ("Couldn't
  # normalize path"), order-dependently. test-shared-lockstep-shinytest2.R
  # already prunes before its AppDriver boots; boot_vignette_app() did not —
  # the asymmetry tracked in .scratch/shared-section-fix/issues/02. Pruning
  # dead paths is a semantic no-op for live ones (see helper-shinytest2.R).
  prune_dead_ggpaintr_resource_paths()
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
  # AppDriver$new returns once the initial Connect handshake completes; the
  # JS side's bindAll() and the server's first flush may still be in flight.
  # Calling wait_for_idle here makes every test_that body start from a
  # quiet, observable state, so the first set_input / get_html / get_value
  # doesn't race the boot tail. Cheap (idle in single-digit ms on most
  # fixtures); only matters for the small subset that have a heavier first
  # flush.
  app$wait_for_idle(timeout = 15 * 1000)
  withr::defer(app$stop(), envir = parent.frame())
  app
}

# Poll `app$get_html(selector)` until `pred(html)` holds or `timeout_ms`
# elapses, returning the last html seen (so the caller asserts on it).
#
# Why this exists: `wait_for_idle()` returns on the FIRST moment the reactive
# graph goes quiet, but a `renderUI`-injected widget can land in a LATER flush
# (the "momentary busy-flag dip" race documented on `expect_host_settled`).
# A single `get_html()` therefore samples too early and intermittently misses
# late content. Under parallel test execution several Chrome+Shiny processes
# contend for CPU, which widens that window from milliseconds to seconds and
# turns the race into reliable flakes. Polling removes it by construction:
# late content is caught when it arrives; genuinely-absent content polls to
# the deadline and the caller's assertion fails exactly as a single-shot read
# would have — so polling only ADDS tolerance, never masks a real failure.
poll_html <- function(app, selector, pred, timeout_ms = 15000, poll_ms = 50) {
  deadline <- Sys.time() + timeout_ms / 1000
  html <- NULL
  repeat {
    html <- tryCatch(app$get_html(selector), error = function(e) NULL)
    if (isTRUE(pred(html))) return(html)
    if (Sys.time() > deadline) return(html)
    Sys.sleep(poll_ms / 1000)
  }
}

# Assert a DOM element with the given Shiny id is present in the page.
expect_dom_id <- function(app, id, timeout_ms = 15000) {
  html <- poll_html(
    app, paste0("#", id),
    function(h) !is.null(h) && nzchar(h), timeout_ms
  )
  testthat::expect_true(
    !is.null(html) && nzchar(html),
    label = paste0("DOM id #", id, " present")
  )
}

# Assert an input reaches `expected` value within `timeout_ms`, polling the
# reactive registry. Use for a value seeded by the server's first flush (e.g.
# a spec `=` boot override) that a single immediate `get_value()` would race —
# the seed can arrive a flush after the Connect handshake completes, and under
# parallel-execution CPU contention that lag is seconds, not milliseconds.
expect_input_eventually <- function(app, id, expected,
                                    timeout_ms = 15000, poll_ms = 50) {
  deadline <- Sys.time() + timeout_ms / 1000
  val <- NULL
  repeat {
    val <- tryCatch(app$get_value(input = id), error = function(e) NULL)
    if (identical(val, expected)) break
    if (Sys.time() > deadline) break
    Sys.sleep(poll_ms / 1000)
  }
  testthat::expect_equal(val, expected)
}

# Set one input WITHOUT waiting for an output change. ggpaintr only re-renders
# on an explicit Update/Draw click, so setting a placeholder widget never
# updates an output by itself — wait_ = TRUE would (correctly) time out.
#
# Waits for Shiny's input binding to register on the target element BEFORE
# dispatching `set_inputs`. shinytest2 emits "Unable to find input binding
# for element with id <id>" when the JS-side binding hasn't been wired by
# the time set_inputs lands — a known race on uiOutput-rendered inputs.
# `wait_for_input_binding()` is empirically fast (max ~6 ms observed
# across 30 calibration boots on a quiet system, p95 4 ms); the 5000 ms
# timeout is the deadline for surfacing a real "binding never appeared"
# regression vs the flake. Pass `bind_timeout_ms = 0` to opt out (e.g.
# when intentionally probing for the absence of a binding).
set_input <- function(app, id, value, bind_timeout_ms = 15000) {
  if (bind_timeout_ms > 0) wait_for_input_binding(app, id, bind_timeout_ms)
  args <- stats::setNames(list(value), id)
  do.call(app$set_inputs, c(args, list(wait_ = FALSE)))
}

# Upload a file to a ppUpload fileInput, waiting for its binding first.
#
# Mirrors `set_input`: `app$upload_file()` resolves the target through
# `app_find_node_id()`, which requires the fileInput to already carry the
# `.shiny-bound-input` class. A ppUpload fileInput is renderUI-injected, so it
# has the same `renderUI -> bindAll()` lag as any other uiOutput input;
# uploading before the binding registers throws "Cannot find HTML element with
# selector #<id>.shiny-bound-input". On a quiet system the lag is sub-frame and
# the bare `app$upload_file()` usually wins the race, but under the CPU
# contention of parallel test execution it loses reliably. Waiting for the
# binding closes the race by construction; a genuine "never binds" regression
# still surfaces at the timeout.
upload_file <- function(app, ..., bind_timeout_ms = 15000) {
  args <- list(...)
  id <- names(args)[1]
  if (!is.null(id) && nzchar(id) && bind_timeout_ms > 0) {
    wait_for_input_binding(app, id, bind_timeout_ms)
  }
  do.call(app$upload_file, args)
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

# Wait until the app is provably in the terminal SUCCESS state, then assert
# it: the host output shows its post-render marker AND the inline error pane
# has cleared, *both simultaneously*. Replaces the fragile
# `wait_for_idle(timeout); expect_no_inline_error()` pattern — wait_for_idle
# can return on a momentary busy-flag dip while the error pane still
# transiently holds a pre-quiescence shiny.silent.error value, which the
# immediate sample then catches (issues/02 — a sampling race, not a product
# bug; ~2.5%, and the heuristic-timeout mitigation proved insufficient,
# clustering across §5.1/§5.2/l3-gg-extra). Polling for the combined
# condition removes the race by construction: a transient error keeps the
# predicate false (keep waiting), a *real* error never satisfies it
# (wait_for_js times out -> a genuine test failure, not masked). Markers are
# post-render-only tokens that appear in the host's *innerHTML* (child
# content), verified empirically against each fixture. NB: `js-plotly-plot`
# and `girafe`/`plotly` live on the output *container's own class*
# (outerHTML), not its children, so they would never satisfy an innerHTML
# check; the rendered figure's content markers are: plotly ->
# `plot-container` (plotly.js builds `<div class="plot-container plotly">`
# only on render), ggiraph -> `<svg` (the rendered graphic, a child),
# ggplot -> `<img` (the base64 renderPlot image, a child).
expect_host_settled <- function(app, host_id, kind, error_output_id,
                                timeout = 25 * 1000) {
  marker <- switch(kind,
    ggplot  = "<img",
    plotly  = "plot-container",
    ggiraph = "<svg",
    stop("unknown kind")
  )
  js <- sprintf(
    paste0("(function(){var h=document.getElementById('%s');",
           "var e=document.getElementById('%s');",
           "return !!h && h.innerHTML.indexOf('%s')!==-1 && ",
           "!!e && e.innerHTML.indexOf('ptr-alert--error')===-1;})()"),
    host_id, error_output_id, marker
  )
  app$wait_for_js(js, timeout = timeout)
  expect_rendered(app, paste0("#", host_id), kind)
  expect_no_inline_error(app, error_output_id)
}

# Assert a rendered consumer/shared picker is *populated* — its element holds
# a real column choice, not an empty uiOutput. A blank uiOutput still has its
# container id present (so plain expect_dom_id passes), which is precisely how
# bug B1 (formula-local ppVar(shared=) never bound in the embed path) survived
# every prior test. This asserts an actual rendered choice for a known column.
expect_picker_populated <- function(app, input_id, choice, timeout_ms = 15000) {
  html <- poll_html(
    app, paste0("#", input_id),
    function(h) !is.null(h) && nzchar(h) && grepl(choice, h, fixed = TRUE),
    timeout_ms
  )
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

# Poll the DOM until the input element `#<id>` is registered with Shiny's
# input binding (i.e. carries the `shiny-bound-input` class that Shiny
# attaches once `Shiny.bindAll()` has wired the element to the input
# registry). Returns the elapsed wait in seconds (useful for calibration).
# Aborts with a structured error on timeout so the failure cleanly
# distinguishes "we waited and the binding never appeared" (a real
# regression) from "set_inputs raced the binding" (the flaky shape).
#
# Why this is needed: every uiOutput-rendered input has a small window
# between renderUI firing and Shiny's `bindAll()` reaching the new DOM
# subtree. shinytest2's `app$set_inputs(id = ..., wait_ = FALSE)` short-
# circuits the wait, so it can land in that window and emit "Unable to
# find input binding for element with id <id>". Calling this helper
# right before `app$set_inputs(...)` closes the race. Empirically
# calibrated default `timeout_ms = 5000` covers the worst-case observed
# binding latency by ~50x (see commit log for calibration data).
# Wait until the output element `#<id>` is settled — the reactive graph is
# idle AND the element exists in the DOM AND it isn't currently flagged
# `shiny-busy`. Use this before reading an output via `app$get_html()` /
# `app$get_value(output = id)` when a re-render might be in flight (after
# a click, a `set_input` cascade, or any reactive invalidation). Returns
# elapsed seconds (useful for calibration). Aborts on timeout with a
# structured class so the failure is distinguishable from "binding never
# registered".
#
# Why this is needed: `app$wait_for_idle()` returns when the reactive
# graph is quiet, but the JS-side output binding's update may still be
# in flight (`shiny-busy` class set on the output node until the binding
# finishes redrawing). Reading `app$get_html(...)` in that window returns
# stale content. This helper polls until the busy-class clears.
#
# Separate from `wait_for_input_binding`: that one guards the INPUT-side
# race (renderUI -> bindAll), this one guards the OUTPUT-side race
# (server emit -> binding update). Use the one that matches the direction
# of the next observable action.
wait_for_output <- function(app, id, timeout_ms = 15000, poll_ms = 25) {
  start <- Sys.time()
  deadline <- start + timeout_ms / 1000
  # First: drain the reactive queue. If a re-render is queued but not yet
  # firing, this is the cheapest way to let it land.
  app$wait_for_idle(timeout = timeout_ms)
  selector <- paste0("#", id)
  repeat {
    html <- tryCatch(app$get_html(selector), error = function(e) NULL)
    if (!is.null(html) && nzchar(html) &&
        !grepl("shiny-busy", html, fixed = TRUE)) {
      return(as.numeric(Sys.time() - start, units = "secs"))
    }
    if (Sys.time() > deadline) {
      rlang::abort(
        sprintf(
          "wait_for_output(\"%s\") timed out after %d ms (element missing, empty, or stuck in `shiny-busy`).",
          id, timeout_ms
        ),
        class = "ggpaintr_wait_for_output_timeout"
      )
    }
    Sys.sleep(poll_ms / 1000)
  }
}

wait_for_input_binding <- function(app, id, timeout_ms = 15000,
                                   poll_ms = 25) {
  start <- Sys.time()
  deadline <- start + timeout_ms / 1000
  # Use `app$get_value(input = id)` as the binding-presence probe — it
  # succeeds iff Shiny has registered the input with its reactive registry
  # (regardless of the binding kind: renderUI'd .shiny-bound-input,
  # bslib tabsetPanel, navset, custom binding, etc.). A DOM-class check
  # like `#<id>.shiny-bound-input` would false-negative on tab and nav
  # inputs which use a different binding shape.
  repeat {
    bound <- tryCatch({
      app$get_value(input = id)
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(bound)) {
      return(as.numeric(Sys.time() - start, units = "secs"))
    }
    if (Sys.time() > deadline) {
      rlang::abort(
        sprintf(
          "wait_for_input_binding(\"%s\") timed out after %d ms. Shiny's input registry has no entry for this id. Either the renderUI for the input's container never fired, or Shiny's bindAll() never reached it.",
          id, timeout_ms
        ),
        class = "ggpaintr_wait_for_input_binding_timeout"
      )
    }
    Sys.sleep(poll_ms / 1000)
  }
}
