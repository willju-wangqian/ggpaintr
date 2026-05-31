# ADR 0012 / PLAN-01 (Bug B) — SC-1 negative-control unit pin.
#
# The positive-case block ("custom-keyword spec entry applies via
# state$spec_seed at boot") was migrated to the J12 browser journey on
# 2026-05-27 (see dev/audit/audit-test-fidelity-v8-j12-browser-
# faithfulness-2026-05-27-2337.html). The negative control below stays
# as a unit pin because its contract — `state$spec_seed` is an EMPTY
# environment when no `spec=` is passed — has no DOM analog: no widget
# changes its rendered HTML when the seed map is empty vs absent.
# Recommendation locked in v8 §7.1.

.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

test_that("ADR 0012 / PLAN-01 (Bug B): state$spec_seed is empty when no spec= is passed", {
  # Negative control: without a spec= argument the seed map stays empty
  # — invariant of `ptr_init_state()`'s default. Guards against future
  # refactors that might inadvertently populate spec_seed from defaults.
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(ppText, mpg))",
      envir = e
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state
    expect_true(is.environment(state$spec_seed))
    expect_equal(length(ls(state$spec_seed, all.names = TRUE)), 0L)
  })
})
