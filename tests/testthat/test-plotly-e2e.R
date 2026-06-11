# test-plotly-e2e.R — PLAN-03 (boot) + PLAN-04 (round-trip) contract tests
# (red-first, implementation-blind).
#
# Source of every expectation: the BDD scenarios in
# dev/plans/0028-plotly-linked-selection-helpers/03-plotly-e2e-boot.html and
# 04-plotly-e2e-roundtrip.html (/implementable PASS 2026-06-10). Written by
# /plan-to-test BEFORE the fixtures exist; no implementation source was read.
#
# FIXTURE INTERFACE (pinned here; the fixture app.R files conform to it):
#
# fixtures/vignette-apps/adr28-plotly-linked/app.R — the ADR 0028 headline
# composition (PLAN-03): ptr_options(gate_draw = FALSE) live mode; explicit
# source = "ptr_e2e".
#   - instance 1, id "p1": formula over mtcars with x = ppVar(wt),
#     y = ppVar(mpg) pickers; rendered via
#     plotly::renderPlotly(ptr_ggplotly(state1, source = "ptr_e2e")) into
#     plotly::plotlyOutput("main_plotly"); error pane ptr_ui_error("p1")
#     (#p1-ptr_error). Pickers: #p1-ggplot_1_1_ppVar_NA / _1_2_ (sibling
#     fixture id convention, test-runtime-gate.R).
#   - sel <- ptr_plotly_selection(state1, mode = "flag", source = "ptr_e2e");
#     instance 2, id "p2": formula head sel() with x = ppVar(hp),
#     y = ppVar(qsec), color = .ptr_selected; default host ptr_ui_plot("p2")
#     (#p2-ptr_plot) + ptr_ui_error("p2") (#p2-ptr_error).
#   - rows-mode table: tableOutput("sel_table") off
#     ptr_plotly_selection(state1, mode = "rows", source = "ptr_e2e").
#
# fixtures/vignette-apps/adr28-plotly-trigger-misuse/app.R (PLAN-04): DEFAULT
# gate_draw = TRUE; same instance-1 wiring (Update button
# #p1-ptr_update_plot, widget #main_plotly, source "ptr_e2e");
# state2 <- ptr_server(f2, "p2", draw_trigger = sel) — the selection reactive
# itself as trigger (ADR rejected-by-design #3); host #p2-ptr_plot.
#
# EVENT INJECTION (PLAN-04, probe-verified 2026-06-10 against the installed
# plotly 4.12.0): plotly:::event_data builds
# `eventID <- paste(event, source, sep = "-")` and reads
# `session$rootScope()$input[[eventID]]` (the `.clientValue-` prefix is
# client-side only) as a JSON string. So the injected ids are
# `plotly_selected-ptr_e2e` / `plotly_deselect-ptr_e2e` at the ROOT scope,
# set via app$set_inputs(..., allow_no_input_binding_ = TRUE). Built trace
# keys serialize as character => the faithful payload carries string keys.
# If this internal contract breaks across plotly versions, the ADR's
# sanctioned fallback (drive the selection reactive directly) must be
# RECORDED in the audit report, never silent.

sel_payload_25 <- '[{"curveNumber":0,"pointNumber":1,"key":"2"},{"curveNumber":0,"pointNumber":4,"key":"5"}]'

inject_event <- function(app, event_id, payload) {
  args <- stats::setNames(list(payload), event_id)
  do.call(
    app$set_inputs,
    c(args, list(allow_no_input_binding_ = TRUE, wait_ = FALSE))
  )
  # Deliberately NO app$wait_for_idle() here. Injecting a selection makes the
  # live-mode instance 2 re-render; under full-suite CPU contention that render
  # flush can transit a momentary shiny.silent.error (a sub-frame window where
  # ggplot reads NULL x/y before the seeded picker snapshot is re-read), which
  # wait_for_idle treats as "not stable" and ABORTS on — a boot-tail/flush
  # race, not a final-state error (project memory
  # shinytest2-boot-tail-race-wait-for-idle). Every caller re-asserts the
  # SETTLED observable via its own bounded poll_* / poll_html predicate, which
  # round-trips to the browser (pumping the event loop so the injected input
  # flushes), both waiting for the event to land AND tolerating the transient.
  invisible(app)
}

# Count data rows in a rendered table's HTML: <tr> blocks that contain <td>
# cells (the header row holds <th> only).
table_data_rows <- function(html) {
  if (is.null(html) || !nzchar(html)) return(NA_integer_)
  chunks <- strsplit(html, "<tr", fixed = TRUE)[[1]]
  sum(grepl("<td", chunks, fixed = TRUE))
}

poll_table_rows <- function(app, n, timeout_ms = 15000) {
  html <- poll_html(
    app, "#sel_table",
    function(h) identical(table_data_rows(h), as.integer(n)),
    timeout_ms
  )
  testthat::expect_equal(table_data_rows(html), as.integer(n))
}

boot_linked <- function() {
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("adr28-plotly-linked")
  # boot_vignette_app() registers `withr::defer(app$stop(), parent.frame())`,
  # and parent.frame() there is THIS wrapper's frame — so the app would be
  # stopped the instant boot_linked() returns, leaving the test body to read a
  # dead session (empty get_html() -> NA tables, dead input bindings). The
  # PLAN-03 boot tests don't hit this because they call boot_vignette_app()
  # directly (its parent.frame() is the test body). Re-scope the cleanup to
  # boot_linked()'s caller (the test body) so the app survives the wrapper
  # return: drop the wrapper-frame defer, re-register on the caller.
  withr::deferred_clear(environment())
  withr::defer(app$stop(), envir = parent.frame())
  # Start every scenario from the settled boot state: instance 1's widget
  # rendered (plotly.js "plot-container" marker) with no inline error, and
  # instance 2 drawn off the all-FALSE flag projection.
  expect_host_settled(app, "main_plotly", "plotly", "p1-ptr_error")
  expect_host_settled(app, "p2-ptr_plot", "ggplot", "p2-ptr_error")
  # Boot-tail race guard (project memory shinytest2-boot-tail-race-wait-for-
  # idle): the round-trip injects a selection that makes instance 2 re-render
  # LIVE. If injection lands before instance 2's renderUI ppVar pickers have
  # re-bound and seeded, the live render reads NULL x/y and ggplot raises
  # "geom_point() requires the following missing aesthetics: x and y".
  # Awaiting the seeded picker values here closes that race by construction.
  expect_input_eventually(app, "p2-ggplot_1_1_ppVar_NA", "hp")
  expect_input_eventually(app, "p2-ggplot_1_2_ppVar_NA", "qsec")
  # Also wait for the rows-mode table to render its (header-only, zero-data-
  # row) boot state before any scenario reads it. renderTable(sel_rows())
  # depends on instance 1's per-draw snapshot being recorded inside
  # ptr_ggplotly; under contention that lands a flush or two after the plotly
  # widget's plot-container marker appears, so expect_host_settled on
  # main_plotly is not by itself a guarantee the table is wired. Polling the
  # rendered <table> here makes the zero-row precondition deterministic.
  poll_html(
    app, "#sel_table",
    function(h) !is.null(h) && nzchar(h) && grepl("<table", h, fixed = TRUE)
  )
  app
}

# --- PLAN-03: boot assertions (no interaction) -------------------------------

test_that("the two-instance app boots with a rendered plotly widget", {
  # PLAN-03 BDD "the two-instance app boots with a rendered plotly widget":
  # instance-1 output holds a rendered plotly htmlwidget, no inline error.
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("adr28-plotly-linked")
  expect_host_settled(app, "main_plotly", "plotly", "p1-ptr_error")
  expect_rendered(app, "#main_plotly", "plotly")  # js-plotly-plot marker
})

test_that("the selection-fed instance draws its all-FALSE flag state on boot", {
  # PLAN-03 BDD "the selection-fed instance draws its all-FALSE flag state on
  # boot": instance 2 settles as a drawn ggplot with an empty error pane, and
  # its ppVar pickers seed the formula defaults (a selection-fed head seeds
  # like any reactive head).
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("adr28-plotly-linked")
  expect_host_settled(app, "p2-ptr_plot", "ggplot", "p2-ptr_error")
  expect_input_eventually(app, "p2-ggplot_1_1_ppVar_NA", "hp")
  expect_input_eventually(app, "p2-ggplot_1_2_ppVar_NA", "qsec")
})

test_that("the rows table renders unconditionally with zero data rows", {
  # PLAN-03 BDD "ADR worked example (becomes possible #2, empty state)": the
  # zero-row-same-columns contract means the renderTable consumer needs no
  # req() dance — a rendered (header-only) table, no error placeholder.
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("adr28-plotly-linked")
  expect_host_settled(app, "p2-ptr_plot", "ggplot", "p2-ptr_error")
  html <- poll_html(
    app, "#sel_table",
    function(h) !is.null(h) && nzchar(h) && grepl("<table", h, fixed = TRUE)
  )
  expect_match(html %||% "", "<table", fixed = TRUE)
  expect_false(grepl("shiny-output-error", html %||% "", fixed = TRUE))
  expect_equal(table_data_rows(html), 0L)
})

# --- PLAN-04: selection round-trip via input-layer injection -----------------

test_that("selected rows appear in the rows-mode table", {
  # PLAN-04 BDD "ADR worked example (becomes possible #2, table fill)":
  # injecting keys c(2, 5) fills the table with exactly 2 data rows.
  app <- boot_linked()
  poll_table_rows(app, 0L)
  inject_event(app, "plotly_selected-ptr_e2e", sel_payload_25)
  poll_table_rows(app, 2L)
})

test_that("the brush drives a flag-mode redraw of the selection-fed instance", {
  # PLAN-04 BDD "ADR worked example (becomes possible #1)": the same injected
  # selection redraws instance 2 (sel() is an ordinary live-mode reactive
  # dependency — zero trigger wiring), with no inline error. This is also the
  # POSITIVE CONTROL for the trigger-misuse no-change assertion below.
  app <- boot_linked()
  before <- app$get_html("#p2-ptr_plot")
  expect_true(nzchar(before %||% ""))
  inject_event(app, "plotly_selected-ptr_e2e", sel_payload_25)
  after <- poll_html(
    app, "#p2-ptr_plot",
    function(h) !is.null(h) && nzchar(h) && !identical(h, before)
  )
  expect_false(identical(after, before))
  expect_host_settled(app, "p2-ptr_plot", "ggplot", "p2-ptr_error")
})

test_that("deselect clears the selection", {
  # PLAN-04 BDD "deselect clears the selection": plotly_deselect returns the
  # table to zero data rows.
  app <- boot_linked()
  inject_event(app, "plotly_selected-ptr_e2e", sel_payload_25)
  poll_table_rows(app, 2L)
  inject_event(app, "plotly_deselect-ptr_e2e", "{}")
  poll_table_rows(app, 0L)
})

test_that("a redraw resets the selection (ADR rejected-by-design #2)", {
  # PLAN-04 BDD "a redraw resets the selection": changing an instance-1 ppVar
  # picker in live mode is a new draw -> new keys -> the old selection is NOT
  # carried over.
  app <- boot_linked()
  inject_event(app, "plotly_selected-ptr_e2e", sel_payload_25)
  poll_table_rows(app, 2L)
  # A live-mode picker change is a new draw -> the per-draw reset observer
  # (state$runtime() in ptr_plotly_selection) clears the keys. Don't gate on
  # wait_for_idle (the redraw flush can transit a transient render error under
  # contention); poll_table_rows below both waits for and asserts the reset.
  set_input(app, "p1-ggplot_1_1_ppVar_NA", "disp")
  poll_table_rows(app, 0L)
})

test_that("the selection reactive as draw_trigger silently never redraws (ADR rejected-by-design #3)", {
  # PLAN-04 BDD "the selection reactive as draw_trigger silently never
  # redraws": under the default gate, draw_trigger = sel (a data-frame
  # reactive, not a click counter) must NOT fire a draw — instance 2's plot
  # HTML stays identical to its pre-injection snapshot. The identical-HTML
  # assertion has teeth only alongside the live-mode flag-redraw scenario
  # above (the discriminating pair, cf. test-runtime-gate.R).
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("adr28-plotly-trigger-misuse")
  draw(app, "p1-ptr_update_plot")
  expect_host_settled(app, "main_plotly", "plotly", "p1-ptr_error")

  before <- app$get_html("#p2-ptr_plot")
  inject_event(app, "plotly_selected-ptr_e2e", sel_payload_25)
  # Bounded no-change window: poll for any difference, expect none to appear.
  last <- poll_html(
    app, "#p2-ptr_plot",
    function(h) !identical(h, before),
    timeout_ms = 4000
  )
  expect_identical(last, before)
})
