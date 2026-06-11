# test-plotly-helpers.R — PLAN-01 contract tests (red-first, implementation-blind)
#
# Source of every expectation: the frozen Interface Contract + BDD scenarios in
# dev/plans/0028-plotly-linked-selection-helpers/01-plotly-ggplotly-core.html
# (/implementable PASS 2026-06-10). Written by /plan-to-test BEFORE
# R/paintr-plotly.R exists; the implementation source was never read.
#
# Third-party observables probe-verified 2026-06-10 against the installed
# plotly 4.12.0 / ggplot2 (NOT against ggpaintr code):
#   - a built widget exposes $x$layout$dragmode, $x$shinyEvents, $x$source and
#     per-trace $key (serialized as character by plotly_build);
#   - geom_smooth's built traces carry no key entries (length-0);
#   - ggplotly(tooltip=) changes the built traces' hover $text.
#
# Harness conventions copied from sibling tests (test-module-server-shared.R,
# test-rewrite-pipeline-data-source.R): public ptr_server() inside a plain
# server function under shiny::testServer(); the state handle is stashed on
# session$userData; session$flushReact() runs the live-mode boot draw;
# gate_draw is toggled per-test via withr::local_options(ggpaintr.gate_draw=).

f_p1 <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
)
f_p2 <- rlang::expr(
  ggplot(mtcars, aes(x = ppVar(hp), y = ppVar(qsec))) + geom_point()
)

# Outer server hosting one ptr_server instance; state on session$userData.
one_instance_server <- function(input, output, session) {
  session$userData$state1 <- ptr_server(f_p1, "p1", envir = globalenv())
}

two_instance_server <- function(input, output, session) {
  session$userData$state1 <- ptr_server(f_p1, "p1", envir = globalenv())
  session$userData$state2 <- ptr_server(f_p2, "p2", envir = globalenv())
}

# --- plotly_mint_keys: pure core (PLAN-01 Interface Contract seam) ----------

test_that("plotly_mint_keys mints per-draw keys on the widget copy only", {
  # PLAN-01 SC "Key minting" / BDD "keys are minted per draw on the widget
  # copy only": .ptr_row = seq_len(32), key aesthetic mapped, input unmutated.
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_point()
  r <- ggpaintr:::plotly_mint_keys(p)
  expect_equal(r$plot$data$.ptr_row, seq_len(32))
  expect_identical(rlang::quo_get_expr(r$plot$mapping$key), quote(.ptr_row))
  expect_false(".ptr_row" %in% names(p$data))
  expect_false(r$key_overridden)
})

test_that("plotly_mint_keys overrides a user aes(key=) with a warn-once contract", {
  # PLAN-01 SC "Key-channel ownership" / BDD "a user formula's own aes(key=)
  # is overridden with a one-time warning" (ADR Decision 5).
  p <- ggplot(mtcars, aes(wt, mpg, key = gear)) + geom_point()
  expect_warning(r <- ggpaintr:::plotly_mint_keys(p), "key")
  expect_true(r$key_overridden)
  expect_identical(rlang::quo_get_expr(r$plot$mapping$key), quote(.ptr_row))
  expect_equal(r$plot$data$.ptr_row, seq_len(32))
  # Second build for the same instance: shell passes key_warned = TRUE.
  expect_no_warning(r2 <- ggpaintr:::plotly_mint_keys(p, key_warned = TRUE))
  expect_true(r2$key_overridden)
  expect_identical(rlang::quo_get_expr(r2$plot$mapping$key), quote(.ptr_row))
})

test_that("plotly_mint_keys silently overwrites a stale .ptr_row column", {
  # PLAN-01 SC "Collision overwrite" / BDD "a stale .ptr_row column is
  # silently overwritten": reserved name, no warning, keys re-minted.
  df <- mtcars
  df$.ptr_row <- rev(seq_len(32))
  p <- ggplot(df, aes(wt, mpg)) + geom_point()
  expect_no_warning(r <- ggpaintr:::plotly_mint_keys(p))
  expect_equal(r$plot$data$.ptr_row, seq_len(32))
})

test_that("plotly_mint_keys rejects a non-ggplot", {
  # PLAN-01 Interface Contract, plotly_mint_keys input domain: "a non-ggplot:
  # abort". (Message text unfrozen by the contract — error-presence only;
  # flagged as a thin clause in the plan-to-test report.) The "not found"
  # exclusion keeps this red while the entry point doesn't exist — a bare
  # expect_error() would green-pass on the absent function.
  err <- tryCatch(ggpaintr:::plotly_mint_keys(mtcars), error = function(e) e)
  expect_s3_class(err, "error")
  expect_no_match(conditionMessage(err), "not found|could not find")
})

test_that("non-identity-stat layers carry no minted keys", {
  # PLAN-01 SC "Stat-layer limitation" / BDD "non-identity-stat layers carry
  # no keys" (ADR Decision 3, accepted limitation).
  skip_if_not_installed("plotly")
  p <- ggplot(mtcars, aes(wt, mpg)) + geom_smooth()
  r <- ggpaintr:::plotly_mint_keys(p)
  b <- plotly::plotly_build(suppressWarnings(plotly::ggplotly(r$plot)))
  keyed <- vapply(
    b$x$data,
    function(tr) length(tr$key) > 0 && !all(is.na(tr$key)),
    logical(1)
  )
  expect_false(any(keyed))
})

# --- ptr_ggplotly: guard, source/store, widget spec --------------------------

test_that("ptr_ggplotly aborts when plotly is not installed", {
  # PLAN-01 SC "Guard" / BDD "call-time guard when plotly is not installed":
  # plotly_installed is the contract-named mock seam.
  testthat::local_mocked_bindings(
    plotly_installed = function(...) FALSE,
    .package = "ggpaintr"
  )
  shiny::testServer(one_instance_server, {
    state1 <- session$userData$state1
    expect_error(ptr_ggplotly(state1), "plotly")
  })
})

test_that("ptr_ggplotly rejects a non-ptr_state with a hard error, not a silent condition", {
  # PLAN-01 Interface Contract error modes: "non-ptr_state state -> abort"
  # (vs the pre-draw req(), which is a *silent* condition — asserting the
  # distinction keeps this from passing on the wrong error path).
  skip_if_not_installed("plotly")
  shiny::testServer(one_instance_server, {
    err <- tryCatch(ptr_ggplotly(mtcars), error = function(e) e)
    expect_s3_class(err, "error")
    expect_false(inherits(err, "shiny.silent.error"))
    # Red-stage guard: must not green-pass on "could not find function".
    expect_no_match(conditionMessage(err), "not found|could not find")
  })
})

test_that("ptr_ggplotly pre-draw raises the silent req() condition", {
  # PLAN-01 Interface Contract input domain (outside): "a pre-draw state ...
  # raises Shiny's silent pre-draw condition, exactly like the demo's req(p)".
  # Default gate_draw = TRUE and no Update click => state stays pre-draw.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = TRUE)
  shiny::testServer(one_instance_server, {
    state1 <- session$userData$state1
    expect_error(ptr_ggplotly(state1), class = "shiny.silent.error")
  })
})

test_that("derived source ids are distinct per instance and recorded server-side", {
  # PLAN-01 SC "Source id + store" / BDD "derived source ids are distinct per
  # instance and recorded server-side" (ADR Decision 6). plotly_source_id and
  # plotly_store are the contract-named seams.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  shiny::testServer(two_instance_server, {
    state1 <- session$userData$state1
    state2 <- session$userData$state2

    s1 <- ggpaintr:::plotly_source_id(state1)
    s2 <- ggpaintr:::plotly_source_id(state2)
    expect_true(is.character(s1) && length(s1) == 1L && nzchar(s1))
    expect_true(is.character(s2) && length(s2) == 1L && nzchar(s2))
    expect_false(identical(s1, s2))

    st <- ggpaintr:::plotly_store(session)
    expect_true(is.environment(st))
    # Idempotent: first call creates, later calls reuse the same environment.
    expect_identical(st, ggpaintr:::plotly_store(session))

    # Live-mode boot draw, then each instance builds its widget (source =
    # NULL => derived id). Entries land in the store keyed by instance id.
    session$flushReact()
    w1 <- ptr_ggplotly(state1)
    w2 <- ptr_ggplotly(state2)
    expect_true(all(c("p1", "p2") %in% ls(st)))
    # NOTE: entry *field names* (source string / snapshot slots) are not
    # frozen by the contract — existence-only assertion here; recorded as a
    # contract gap in the plan-to-test report.
  })
})

test_that("ptr_ggplotly returns a plain plotly widget configured for selection", {
  # PLAN-01 SC 1 + "Widget configuration" + Interface Contract output spec:
  # plain "plotly" class, dragmode = "select", plotly_selected/_deselect
  # registered, identity-stat trace carries keys 1..nrow(drawn data).
  # Also the ADR worked example "becomes possible #3" / BDD "composing extra
  # plotly layout after the helper": the plotly::layout() pipe keeps working.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  shiny::testServer(one_instance_server, {
    state1 <- session$userData$state1
    session$flushReact()

    w <- ptr_ggplotly(state1, tooltip = "all")
    expect_s3_class(w, "plotly")
    expect_false(any(grepl("ptr|ggpaintr", class(w))))

    b <- plotly::plotly_build(w)
    expect_identical(b$x$layout$dragmode, "select")
    expect_true(all(c("plotly_selected", "plotly_deselect") %in% b$x$shinyEvents))
    expect_equal(as.integer(b$x$data[[1]]$key), seq_len(32))

    # The user's plotly verbs stay composable after the helper.
    w2 <- plotly::layout(w, legend = list(orientation = "h"))
    expect_s3_class(w2, "plotly")
  })
})

test_that("ptr_ggplotly forwards ... to plotly::ggplotly", {
  # PLAN-01 SC "Widget configuration": "... is forwarded to plotly::ggplotly()
  # (e.g. tooltip = 'all' reaches it)". Observable (probe-verified): tooltip
  # selection changes the built traces' hover text.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  shiny::testServer(one_instance_server, {
    state1 <- session$userData$state1
    session$flushReact()
    t_all <- plotly::plotly_build(ptr_ggplotly(state1, tooltip = "all"))$x$data[[1]]$text
    t_y <- plotly::plotly_build(ptr_ggplotly(state1, tooltip = "y"))$x$data[[1]]$text
    expect_false(identical(t_all, t_y))
  })
})

test_that("ptr_ggplotly never mutates the state's drawn data", {
  # PLAN-01 SC "Key minting" (second half) + Interface Contract side effects:
  # "Never mutates state$runtime()$plot$data" — no .ptr_row on the runtime's
  # data after the widget builds.
  skip_if_not_installed("plotly")
  withr::local_options(ggpaintr.gate_draw = FALSE)
  shiny::testServer(one_instance_server, {
    state1 <- session$userData$state1
    session$flushReact()
    w <- ptr_ggplotly(state1)
    expect_false(".ptr_row" %in% names(state1$runtime()$plot$data))
  })
})
