# test-plotly-selection.R — PLAN-02 contract tests (red-first, implementation-blind)
#
# Source of every expectation: the frozen Interface Contract + BDD scenarios in
# dev/plans/0028-plotly-linked-selection-helpers/02-plotly-selection.html
# (/implementable PASS 2026-06-10). Written by /plan-to-test BEFORE the
# ptr_plotly_selection implementation exists; the implementation source was
# never read.
#
# Event-injection contract (probe-verified 2026-06-10 against the installed
# plotly 4.12.0, NOT against ggpaintr code): plotly::event_data() builds
# `eventID <- paste(event, source, sep = "-")` and reads
# `session$rootScope()$input[[eventID]]` as a JSON string parsed with
# `jsonlite::parse_json(simplifyVector = TRUE)`; built trace keys serialize as
# character, so a faithful browser payload carries string keys. Shell-level
# scenarios therefore inject `session$setInputs("plotly_selected-<source>" =
# <json>)` per the plan's sanctioned execution path; if event_data proves
# undrivable under the mock session, the plan's sanctioned fallback (drive the
# PLAN-01 store directly) must be recorded, never silent.
#
# Scope note (per plan + project memory): testServer here unit-tests the
# helper shell's own contract; it is never evidence about ptr_app() behavior.

strip_rownames <- function(d) {
  rownames(d) <- NULL
  d
}

sel_payload_25 <- '[{"curveNumber":0,"pointNumber":1,"key":"2"},{"curveNumber":0,"pointNumber":4,"key":"5"}]'
keyless_payload <- '[{"curveNumber":0,"pointNumber":3,"x":3.2,"y":21.1}]'

set_root_input <- function(session, id, value) {
  do.call(session$setInputs, stats::setNames(list(value), id))
}

# --- pure projection cores (PLAN-02 Interface Contract seam) -----------------

test_that("rows projection returns the selected slice", {
  # PLAN-02 BDD "rows projection returns the selected slice" + worked example:
  # snapshot mtcars[1:10, ], keys c(2, 5) -> the 2-row frame, snapshot
  # columns, no .ptr_row.
  snap <- mtcars[1:10, ]
  out <- ggpaintr:::plotly_project_rows(snap, c(2L, 5L))
  expect_equal(nrow(out), 2L)
  expect_identical(names(out), names(snap))
  expect_equal(strip_rownames(out), strip_rownames(snap[c(2L, 5L), ]))
  expect_false(".ptr_row" %in% names(out))
})

test_that("empty selection is zero rows, same columns", {
  # PLAN-02 BDD "empty selection is zero rows, same columns" (the no-req()
  # renderTable contract, ADR worked example #2's enabling empty state).
  snap <- mtcars[1:10, ]
  out <- ggpaintr:::plotly_project_rows(snap, integer(0))
  expect_equal(nrow(out), 0L)
  expect_identical(names(out), names(snap))
})

test_that("flag projection is the full drawn data plus a logical .ptr_selected", {
  # PLAN-02 BDD "flag projection ..." + Interface Contract worked example:
  # 10 rows, .ptr_selected TRUE exactly at rows 2 and 5, no .ptr_row.
  snap <- mtcars[1:10, ]
  out <- ggpaintr:::plotly_project_flag(snap, c(2L, 5L))
  expect_equal(nrow(out), 10L)
  expect_true(is.logical(out$.ptr_selected))
  expect_identical(
    out$.ptr_selected,
    c(FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE)
  )
  expect_equal(strip_rownames(out[names(snap)]), strip_rownames(snap))
  expect_false(".ptr_row" %in% names(out))
})

test_that("a stale .ptr_selected column is silently overwritten", {
  # PLAN-02 SC "Collision overwrite" / BDD "a stale .ptr_selected column is
  # silently overwritten" + plotly_project_flag worked example (ADR Decision
  # 5: silent, because chained selection-fed instances ride stale flags in).
  snap <- transform(mtcars[1:10, ], .ptr_selected = TRUE)
  expect_no_warning(out <- ggpaintr:::plotly_project_flag(snap, 1L))
  expect_identical(out$.ptr_selected, c(TRUE, rep(FALSE, 9)))
  expect_equal(sum(names(out) == ".ptr_selected"), 1L)
})

test_that("stale out-of-range keys are dropped, never select wrong rows", {
  # PLAN-02 Interface Contract input domain: "keys outside seq_len(nrow) are
  # dropped (a key can only be stale after a redraw, and stale keys must not
  # select wrong rows)".
  snap <- mtcars[1:10, ]
  out_rows <- ggpaintr:::plotly_project_rows(snap, c(2L, 99L))
  expect_equal(nrow(out_rows), 1L)
  expect_equal(strip_rownames(out_rows), strip_rownames(snap[2L, ]))
  out_flag <- ggpaintr:::plotly_project_flag(snap, c(2L, 99L))
  expect_identical(which(out_flag$.ptr_selected), 2L)
})

test_that(".ptr_row never appears in either projection, even riding in on the snapshot", {
  # PLAN-02 SC "Neither projection exposes .ptr_row" + Interface Contract
  # output spec: "columns identical to snapshot's (minus any .ptr_row, which
  # never appears)".
  snap <- mtcars[1:10, ]
  snap$.ptr_row <- seq_len(10)
  expect_false(".ptr_row" %in% names(ggpaintr:::plotly_project_rows(snap, 2L)))
  expect_false(".ptr_row" %in% names(ggpaintr:::plotly_project_flag(snap, 2L)))
})

test_that("projection cores reject a non-data-frame snapshot", {
  # PLAN-02 Interface Contract input domain (outside): "a non-data-frame
  # snapshot: abort". (Message text unfrozen — error-presence only; flagged
  # in the plan-to-test report.) The "not found" exclusion keeps these red
  # while the entry points don't exist — a bare expect_error() would
  # green-pass on the absent functions.
  err_rows <- tryCatch(
    ggpaintr:::plotly_project_rows(list(a = 1), 1L),
    error = function(e) e
  )
  expect_s3_class(err_rows, "error")
  expect_no_match(conditionMessage(err_rows), "not found|could not find")
  err_flag <- tryCatch(
    ggpaintr:::plotly_project_flag(list(a = 1), 1L),
    error = function(e) e
  )
  expect_s3_class(err_flag, "error")
  expect_no_match(conditionMessage(err_flag), "not found|could not find")
})

# --- exported shell: validation + the two negative-example pins --------------

test_that("unknown mode aborts naming the allowed modes", {
  # PLAN-02 SC "mode is validated" / BDD "unknown mode aborts": message names
  # both allowed projections (match.arg-style), per the contract's rejection
  # of a bare-indices projection.
  skip_if_not_installed("plotly")
  server <- function(input, output, session) {
    session$userData$state1 <- ptr_server(
      rlang::expr(ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()),
      "p1",
      envir = globalenv()
    )
  }
  shiny::testServer(server, {
    state1 <- session$userData$state1
    err <- tryCatch(
      ptr_plotly_selection(state1, mode = "indices"),
      error = function(e) e
    )
    expect_s3_class(err, "error")
    expect_match(conditionMessage(err), "rows")
    expect_match(conditionMessage(err), "flag")
  })
})

test_that("a bare reactive as the pipeline head is an error (ADR rejected-by-design #4)", {
  # PLAN-02 BDD "a bare reactive as the pipeline head is an error": piping
  # `sel` (the function) instead of `sel()` into ggplot() raises ggplot2's
  # literal error. Expected value probe-verified against the installed
  # ggplot2 on 2026-06-10 — this pins a dependency contract, so it is GREEN
  # at the red-first stage by design (flagged in the plan-to-test report).
  sel <- shiny::reactive(mtcars)
  err <- tryCatch(
    ggplot2::ggplot(sel, ggplot2::aes(x = wt, y = mpg)),
    error = function(e) e
  )
  expect_s3_class(err, "error")
  expect_match(
    conditionMessage(err), "`data` cannot be a function",
    fixed = TRUE
  )
})

# --- shell-level selection lifecycle (testServer + input-layer injection) ----

# Live-mode harness: instance "p1" over a reactiveVal pipeline head so a test
# can force draw N+1; both projections wired with the explicit source "ut1"
# (same id ptr_ggplotly receives, mirroring the fixture's explicit-source
# pattern). The widget build (= per-draw snapshot publication) is triggered
# from the test body via ptr_ggplotly(), standing in for renderPlotly's
# re-execution.
selection_server <- function(input, output, session) {
  rx <- shiny::reactiveVal(mtcars)
  session$userData$rx <- rx
  f_rx <- rlang::expr(
    rx() |> ggplot(aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
  )
  st <- ptr_server(f_rx, "p1")
  session$userData$state1 <- st
  session$userData$sel_rows <- ptr_plotly_selection(st, mode = "rows", source = "ut1")
  session$userData$sel_flag <- ptr_plotly_selection(st, mode = "flag", source = "ut1")
}

test_that("a selection lands, then a new draw resets it to empty", {
  # PLAN-02 SC "Reset on draw" / BDD "a new draw resets the selection to
  # empty" (ADR Decision 3 LOCKED: keys are per-draw and meaningless across
  # draws; ADR rejected-by-design #2 at the unit tier).
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  shiny::testServer(selection_server, {
    state1 <- session$userData$state1
    sel_rows <- session$userData$sel_rows
    sel_flag <- session$userData$sel_flag

    session$flushReact()
    invisible(ptr_ggplotly(state1, source = "ut1"))  # draw N snapshot lands

    set_root_input(session, "plotly_selected-ut1", sel_payload_25)
    expect_equal(nrow(sel_rows()), 2L)

    # Draw N+1: new pipeline-head data, live-mode redraw, new snapshot.
    session$userData$rx(mtcars[1:20, ])
    session$flushReact()
    invisible(ptr_ggplotly(state1, source = "ut1"))

    out <- sel_rows()
    expect_equal(nrow(out), 0L)
    expect_identical(names(out), names(mtcars))
    flg <- sel_flag()
    expect_equal(nrow(flg), 20L)
    expect_false(any(flg$.ptr_selected))
  })
})

test_that("plotly_deselect clears the selection", {
  # PLAN-02 SC "Reset on draw" (second half) / BDD "plotly_deselect clears
  # the selection": back to the zero-row, same-columns empty state.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  shiny::testServer(selection_server, {
    state1 <- session$userData$state1
    sel_rows <- session$userData$sel_rows

    session$flushReact()
    invisible(ptr_ggplotly(state1, source = "ut1"))

    set_root_input(session, "plotly_selected-ut1", sel_payload_25)
    expect_equal(nrow(sel_rows()), 2L)

    set_root_input(session, "plotly_deselect-ut1", "{}")
    out <- sel_rows()
    expect_equal(nrow(out), 0L)
    expect_identical(names(out), names(mtcars))
  })
})

test_that("a keyless event leaves the selection at its empty state (ADR rejected-by-design #1)", {
  # PLAN-02 SC "Keyless events stay empty" / BDD "selecting on a
  # stat-transformed layer yields nothing": a payload with no key field —
  # what brushing a geom_smooth() trace produces — is not an error and not a
  # selection.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  smooth_server <- function(input, output, session) {
    f_sm <- rlang::expr(
      ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_smooth()
    )
    st <- ptr_server(f_sm, "p1", envir = globalenv())
    session$userData$state1 <- st
    session$userData$sel_rows <- ptr_plotly_selection(st, mode = "rows", source = "ut1")
    session$userData$sel_flag <- ptr_plotly_selection(st, mode = "flag", source = "ut1")
  }
  shiny::testServer(smooth_server, {
    state1 <- session$userData$state1
    sel_rows <- session$userData$sel_rows
    sel_flag <- session$userData$sel_flag

    session$flushReact()
    invisible(ptr_ggplotly(state1, source = "ut1"))

    set_root_input(session, "plotly_selected-ut1", keyless_payload)
    out <- sel_rows()
    expect_equal(nrow(out), 0L)
    expect_identical(names(out), names(mtcars))
    flg <- sel_flag()
    expect_equal(nrow(flg), 32L)
    expect_false(any(flg$.ptr_selected))
  })
})

test_that("pre-draw, the selection reactive req()s the first draw", {
  # PLAN-02 Interface Contract output spec: "Pre-draw (no snapshot yet): the
  # reactive req()s the first draw (silent condition)" — the documented
  # pre-draw window, a silent condition rather than an error.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = TRUE)
  shiny::testServer(selection_server, {
    sel_rows <- session$userData$sel_rows
    expect_error(sel_rows(), class = "shiny.silent.error")
  })
})
