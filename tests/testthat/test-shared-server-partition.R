# Step 02 (#P2) -- UI<->server partition consistency. `ptr_shared_server()`
# must derive the SAME partition `ptr_shared()` computed: it owns ONLY the
# cross-formula (panel) keys; formula-local keys are bound by each module
# itself. These are correctness-invariant tests, not feature tests.
#
# The lockstep scenarios wire `ptr_shared_server()` + two `ptr_module_server`
# modules by hand (NOT via `ptr_app_grid`, whose coordinator routing is a
# later step) and drive them through an app-level `shiny::testServer()`.
# Per project memory a bare `ptr_server` testServer is not a faithful proxy
# for the real wiring, so the host server function below mirrors what an
# embedder writes. Each module's returned `ptr_state` is captured by
# shimming `ptr_module_server` in the package namespace (same technique as
# test-shared-stage-checkbox.R's grid end-to-end test).

f1 <- 'ggplot(mtcars) + geom_point(aes(x = var(shared = "B"), y = mpg), size = num(shared = "A"))'
f2 <- 'ggplot(mtcars) + geom_point(aes(x = var(shared = "B"), y = hp))'

test_that("S-P2.3 server partition matches UI partition: bundle holds panel keys only", {
  # BDD Scenario: Server partition matches UI partition.
  obj <- ptr_shared(formulas = list(f1, f2))
  expect_equal(obj$panel_keys, "B")

  ss <- NULL
  server <- function(input, output, session) {
    ss <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    # Exactly the cross-formula key gets a shared reactive.
    expect_equal(names(ss$shared), "B")
    expect_true(shiny::is.reactive(ss$shared$B))
    # No shared reactive for the formula-local key "A" or an absent "C".
    expect_null(ss$shared$A)
    expect_null(ss$shared$C)
    expect_false("A" %in% names(ss$shared))
    # Bundle shape is unchanged (Step 02 constraint).
    expect_s3_class(ss, "ptr_shared_state")
    expect_true(is.list(ss$shared_resolutions))
    expect_true(is.list(ss$shared_stage_enabled))
  })
})

test_that("S-P2.1 cross-formula key drives both plots in lockstep, one source", {
  # BDD Scenario: Cross-formula key drives both plots in lockstep.
  obj <- ptr_shared(formulas = list(f1, f2))

  module_states <- new.env(parent = emptyenv())
  orig_ms <- getFromNamespace("ptr_module_server", "ggpaintr")
  capt_ms <- function(id, formula, ...) {
    res <- orig_ms(id, formula, ...)
    module_states[[id]] <- res
    res
  }
  assignInNamespace("ptr_module_server", capt_ms, "ggpaintr")
  withr::defer(assignInNamespace("ptr_module_server", orig_ms, "ggpaintr"))

  server <- function(input, output, session) {
    ss <- ggpaintr::ptr_shared_server(obj, envir = globalenv())
    session$userData$ss <- ss
    ggpaintr::ptr_module_server("p1", f1, envir = globalenv(), shared_state = ss)
    ggpaintr::ptr_module_server("p2", f2, envir = globalenv(), shared_state = ss)
  }

  shiny::testServer(server, {
    session$flushReact()
    ss <- session$userData$ss
    p1 <- module_states[["p1"]]
    p2 <- module_states[["p2"]]

    # Exactly ONE reactive source for "B": both modules received the
    # identical reactive object from the single top-level bundle.
    expect_true(identical(p1$shared_bindings$B, p2$shared_bindings$B))
    expect_true(identical(p1$shared_bindings$B, ss$shared$B))

    # Drive the single panel widget; click the shared draw-all (the
    # 2-formula path gates every module's render on it).
    session$setInputs(shared_B = "wt", ptr_shared_draw_all = 1L)
    session$flushReact()
    expect_match(ptr_extract_code(p1), "x = wt", fixed = TRUE)
    expect_match(ptr_extract_code(p2), "x = wt", fixed = TRUE)

    # Change it again -> BOTH re-render against the new value.
    session$setInputs(shared_B = "drat", ptr_shared_draw_all = 2L)
    session$flushReact()
    expect_match(ptr_extract_code(p1), "x = drat", fixed = TRUE)
    expect_match(ptr_extract_code(p2), "x = drat", fixed = TRUE)
  })
})

test_that("S-P2.2 formula-local key is module-scoped, not panel-owned", {
  # BDD Scenario: Formula-local key is module-scoped, not in the panel.
  obj <- ptr_shared(formulas = list(f1, f2))

  module_states <- new.env(parent = emptyenv())
  orig_ms <- getFromNamespace("ptr_module_server", "ggpaintr")
  capt_ms <- function(id, formula, ...) {
    res <- orig_ms(id, formula, ...)
    module_states[[id]] <- res
    res
  }
  assignInNamespace("ptr_module_server", capt_ms, "ggpaintr")
  withr::defer(assignInNamespace("ptr_module_server", orig_ms, "ggpaintr"))

  server <- function(input, output, session) {
    ss <- ggpaintr::ptr_shared_server(obj, envir = globalenv())
    session$userData$ss <- ss
    ggpaintr::ptr_module_server("p1", f1, envir = globalenv(), shared_state = ss)
    ggpaintr::ptr_module_server("p2", f2, envir = globalenv(), shared_state = ss)
  }

  shiny::testServer(server, {
    session$flushReact()
    ss <- session$userData$ss
    p1 <- module_states[["p1"]]
    p2 <- module_states[["p2"]]

    # "A" is formula-local: NOT panel-owned (absent from the bundle), but
    # p1 self-binds it as a MODULE-scoped reactive (distinct from any
    # top-level panel source).
    expect_false("A" %in% names(ss$shared))
    expect_true(shiny::is.reactive(p1$shared_bindings$A))
    expect_false(identical(p1$shared_bindings$A, ss$shared$B))
    # p2 never declared "A" at all.
    expect_false("A" %in% names(p2$shared_bindings))

    # Changing p1's module-local "A" widget re-renders ONLY p1. The
    # widget is the canonical `shared_A` id under p1's namespace.
    session$setInputs(
      shared_B = "wt", ptr_shared_draw_all = 1L,
      `p1-shared_A` = 5
    )
    session$flushReact()
    p2_code_before <- ptr_extract_code(p2)
    expect_match(ptr_extract_code(p1), "size = 5", fixed = TRUE)

    session$setInputs(`p1-shared_A` = 9, ptr_shared_draw_all = 2L)
    session$flushReact()
    expect_match(ptr_extract_code(p1), "size = 9", fixed = TRUE)
    # p2 has no "A"; its code is unaffected by the p1-local change.
    expect_equal(ptr_extract_code(p2), p2_code_before)
  })
})
