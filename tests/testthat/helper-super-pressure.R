# helper-super-pressure.R — sentinel-propagation primitives for the
# super-app pressure suite (test-super-pressure.R / ADR 0013).
#
# Why this exists, in one line: ADR 0013 binds the suite to
# **propagation assertions**, not presence proxies. The B3 regression
# (toggle wiring, fixed in 2c504da) survived a full cycle of presence-style
# fixture assertions (expect_dom_id, expect_code_nonempty) because those
# helpers proved only that *some* code text reached the panel. Project
# memory `e2e-assertion-weakness-lens` distills the lesson:
# presence proves the surface; propagation proves the integration.
#
# Every helper here takes an *exact sentinel string* and asserts that
# sentinel appears at an *exact rendered position* under an *explicitly
# named mode* (final or preserve). The helpers refuse to fall back to
# "code panel non-empty": that fallback is the failure shape we're
# defending against.
#
# Independence: this helper deliberately duplicates small fragments
# (set_input shape, draw shape, AppDriver boot) from
# `helper-vignette-apps.R` rather than sharing. Sharing across two e2e
# suites couples them; the new helpers stay independent so the
# super-pressure suite can evolve without disturbing the vignette-app
# suite (PLAN-01 constraint).
#
# get_values() is deliberately NOT used (project memory
# `shinytest2-appdir-pkgload`); all reads are targeted single-output
# (get_value) / targeted DOM (get_html).

# Boot one super-app fixture in a real headless-Chromium browser.
# Mirrors the boot_vignette_app() scaffolding (skip_on_cran +
# skip_if_not_installed + source-root guard + GGP_PKG env +
# prune_dead_ggpaintr_resource_paths + suppressWarnings(AppDriver$new) +
# withr::defer(app$stop())). Distinct name so the two boots can evolve
# independently.
boot_super_app <- function(slug, app_file = "app.R") {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  # Source-root guard. The fixture app.R boots via
  # pkgload::load_all(Sys.getenv("GGP_PKG")); that needs the package
  # SOURCE tree (a DESCRIPTION at GGP_PKG). skip_on_cran() above does
  # NOT cover devtools::check() (devtools runs the check subprocess with
  # NOT_CRAN=true). Skipping on absent DESCRIPTION turns that into a
  # clean SKIP under check while NOT_CRAN=true test_dir (source tree
  # present) runs every browser test. See CLAUDE.md authoritative-gate.
  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  testthat::skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "e2e super-app boot needs the package source root (pkgload::load_all); absent under the R CMD check .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  # Resource-path leak isolation (a prior `css =` test can leave a
  # process-global ggpaintr-user-* resource path pointing at a torn-down
  # tempdir; AppDriver$new() replays parent resourcePaths() into the
  # child and a dead entry aborts boot). Pruning dead paths is a
  # semantic no-op for live ones.
  prune_dead_ggpaintr_resource_paths()
  app_dir <- testthat::test_path("fixtures", "vignette-apps", slug)
  # `app_file` lets a caller boot a non-default file in the same fixture
  # dir (per ADR-0016: every super-app has a peer `app-no-default.R`
  # alongside its `app.R`). shinytest2's AppDriver resolves the directory
  # to `app.R` by name; to point it elsewhere we materialise a temp dir
  # copy with the requested file renamed to `app.R`, preserving every
  # sibling (CSVs, user.css, etc.).
  if (!identical(app_file, "app.R")) {
    src_path <- file.path(app_dir, app_file)
    if (!file.exists(src_path)) {
      stop(sprintf("boot_super_app: app_file '%s' not found in '%s'",
                   app_file, app_dir))
    }
    staging <- withr::local_tempdir(.local_envir = parent.frame())
    siblings <- list.files(app_dir, full.names = TRUE, no.. = TRUE)
    file.copy(siblings, staging, recursive = TRUE)
    # Drop the canonical app.R from the staging dir if present so the
    # rename doesn't collide; then rename the requested file in place.
    canonical_in_staging <- file.path(staging, "app.R")
    if (file.exists(canonical_in_staging)) file.remove(canonical_in_staging)
    file.rename(file.path(staging, app_file),
                file.path(staging, "app.R"))
    app_dir <- staging
  }
  # "Failed to locate globals" / htmlDependency-prefix warnings on
  # AppDriver$new() are benign (project memory); suppression is scoped
  # to construction only, never to assertion output.
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = paste0("super-", slug,
                    if (identical(app_file, "app.R")) "" else "-no-default"),
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop(), envir = parent.frame())
  app
}

# Set one input WITHOUT waiting for an output change. Identical semantics
# to set_input() in helper-vignette-apps.R but renamed so the
# sentinel-shaped intent is visible at the call site. ggpaintr only
# re-renders on an explicit Update/Draw click, so setting a placeholder
# widget never updates an output by itself — wait_ = TRUE would
# (correctly) time out.
#
# Pre-check the input is rendered in the DOM. Without this guard,
# shinytest2's `set_inputs(..., wait_ = FALSE)` swallows "Unable to find
# input binding for element with id X" as a printed console warning —
# the input never gets set, but the test reports no error. That swallow
# concealed an ADR-0016 diagnosis once: consumer pickers in no-default
# formulas don't render until upstream column scope is resolved, so
# `set_sentinel` calls against them were no-ops while the test thought
# it had driven the picker. Surface that failure mode here, at the call
# site, so the test stops with an actionable message instead of cascading
# into a downstream assertion mismatch (project memory
# `e2e-assertion-weakness-lens`).
set_sentinel <- function(app, input_id, sentinel) {
  page_html <- app$get_html(paste0("#", input_id))
  if (is.null(page_html) || !nzchar(page_html)) {
    stop(
      "set_sentinel: input '", input_id, "' is not rendered in the DOM. ",
      "Consumer pickers gate on upstream column scope — provide an ",
      "upload / source resolution / Controls-subtab activation before ",
      "driving this widget."
    )
  }
  args <- stats::setNames(list(sentinel), input_id)
  do.call(app$set_inputs, c(args, list(wait_ = FALSE)))
}

# Click the draw/update button and wait for the app to go idle.
# Default button_id matches ptr_app()'s convention (see
# helper-vignette-apps.R::draw()).
draw_and_wait <- function(app, button_id = "draw") {
  app$click(button_id)
  app$wait_for_idle(timeout = 25 * 1000)
}

# Flip the B3 mode radio. Input id `ptr_code_mode` matches what
# ptr_register_code() registers (verified against R/paintr-server.R and
# R/paintr-app.R). Refuses any other mode string up-front so callers cannot
# smuggle a typo into a test label. Post-ADR-0022 only "final" and "spec"
# are valid radio values; the legacy "preserve" choice was retired and its
# render-walker shape is unit-tested directly in test-render-preserve.R.
toggle_code_mode <- function(app, mode) {
  if (!isTRUE(mode %in% c("final", "spec"))) {
    stop("mode must be 'final' or 'spec'")
  }
  set_sentinel(app, "ptr_code_mode", mode)
  app$wait_for_idle(timeout = 25 * 1000)
}

# Assert: the literal `sentinel` appears in the code-panel output at a
# position consistent with `position_regex`, under the named `mode`.
#
# Contract (refuses to weaken):
#   1. code_text <- app$get_value(output = code_output_id)
#   2. regexpr(position_regex, code_text, perl = TRUE) must match
#   3. The matched substring (NOT the whole code_text) must contain the
#      literal `sentinel` (fixed-string check).
#
# Any of: code_text empty, regex doesn't match, regex matches but the
# matched substring lacks the literal sentinel → expectation fails,
# label names all four inputs (code_output_id, sentinel,
# position_regex, mode).
expect_sentinel_in_code <- function(app, code_output_id, sentinel,
                                    position_regex, mode) {
  label_inputs <- sprintf(
    "code_output_id=%s, sentinel=%s, position_regex=%s, mode=%s",
    code_output_id, sentinel, position_regex, mode
  )
  code_text <- app$get_value(output = code_output_id)
  if (!is.character(code_text) || length(code_text) != 1L ||
        !nzchar(code_text)) {
    testthat::expect_true(
      FALSE,
      label = paste0(
        "expect_sentinel_in_code: code panel value is empty/non-scalar; ",
        label_inputs
      )
    )
    return(invisible(NULL))
  }
  m <- regexpr(position_regex, code_text, perl = TRUE)
  if (m[[1L]] < 0L) {
    testthat::expect_true(
      FALSE,
      label = paste0(
        "expect_sentinel_in_code: position_regex did NOT match in code text; ",
        label_inputs,
        "; actual code_text=", code_text
      )
    )
    return(invisible(NULL))
  }
  match_len <- attr(m, "match.length")
  matched_substr <- substr(code_text, m[[1L]],
                           m[[1L]] + match_len[[1L]] - 1L)
  testthat::expect_true(
    grepl(sentinel, matched_substr, fixed = TRUE),
    label = paste0(
      "expect_sentinel_in_code: position_regex matched but its capture did ",
      "NOT contain the literal sentinel; ",
      label_inputs,
      "; actual_capture=", matched_substr
    )
  )
  invisible(NULL)
}

# Assert: the literal `sentinel` does NOT appear anywhere in the rendered
# code-panel output. Used for the inverse claim in final mode when only
# preserve mode should reveal a placeholder wrapper. Fixed-string check
# (no regex semantics — the caller passed a literal token).
expect_sentinel_nowhere <- function(app, code_output_id, sentinel) {
  code_text <- app$get_value(output = code_output_id) %||% ""
  testthat::expect_false(
    grepl(sentinel, code_text, fixed = TRUE),
    label = paste0(
      "expect_sentinel_nowhere: sentinel ", sentinel,
      " appeared in #", code_output_id, "; actual code_text=", code_text
    )
  )
}

# Assert: the host output `host_id` shows no Shiny error class. ggpaintr
# emits the standard `shiny-output-error` class on a server-side render
# error and the project's own `ptr-alert--error` block on a inline-error
# pane. Default host_id matches ptr_app()'s default plot host.
expect_no_plot_error <- function(app, host_id = "ptr_plot") {
  html <- app$get_html(paste0("#", host_id)) %||% ""
  testthat::expect_false(
    grepl("shiny-output-error", html, fixed = TRUE) ||
      grepl("ptr-alert--error", html, fixed = TRUE),
    label = paste0(
      "expect_no_plot_error: host #", host_id,
      " contained a shiny-output-error or ptr-alert--error class"
    )
  )
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
