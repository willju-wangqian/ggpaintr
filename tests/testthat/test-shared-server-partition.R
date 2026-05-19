# Step 02 (#P2) -- UI<->server partition consistency. `ptr_shared_server()`
# must derive the SAME partition `ptr_shared()` computed: it owns ONLY the
# cross-formula (panel) keys; formula-local keys are bound by each module
# itself. These are correctness-invariant tests, not feature tests.
#
# The lockstep scenarios wire `ptr_shared_server()` + two `ptr_server`
# modules by hand (NOT via `ptr_app_grid`, whose coordinator routing is a
# later step) and drive them through an app-level `shiny::testServer()`.
# Per project memory a bare `ptr_server_internal` testServer is not a faithful proxy
# for the real wiring, so the host server function below mirrors what an
# embedder writes. Each module's returned `ptr_state` is captured by
# shimming `ptr_server` in the package namespace (same technique as
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
  orig_ms <- getFromNamespace("ptr_server", "ggpaintr")
  capt_ms <- function(formula, id = NULL, ...) {
    res <- orig_ms(formula, id, ...)
    module_states[[id]] <- res
    res
  }
  assignInNamespace("ptr_server", capt_ms, "ggpaintr")
  withr::defer(assignInNamespace("ptr_server", orig_ms, "ggpaintr"))

  server <- function(input, output, session) {
    ss <- ggpaintr::ptr_shared_server(obj, envir = globalenv())
    session$userData$ss <- ss
    ggpaintr::ptr_server(f1, "p1", envir = globalenv(), shared_state = ss)
    ggpaintr::ptr_server(f2, "p2", envir = globalenv(), shared_state = ss)
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
  orig_ms <- getFromNamespace("ptr_server", "ggpaintr")
  capt_ms <- function(formula, id = NULL, ...) {
    res <- orig_ms(formula, id, ...)
    module_states[[id]] <- res
    res
  }
  assignInNamespace("ptr_server", capt_ms, "ggpaintr")
  withr::defer(assignInNamespace("ptr_server", orig_ms, "ggpaintr"))

  server <- function(input, output, session) {
    ss <- ggpaintr::ptr_shared_server(obj, envir = globalenv())
    session$userData$ss <- ss
    ggpaintr::ptr_server(f1, "p1", envir = globalenv(), shared_state = ss)
    ggpaintr::ptr_server(f2, "p2", envir = globalenv(), shared_state = ss)
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

test_that("S-P2.4 canonical f1=A+A+B / f2=C+C+B end-to-end via the coordinator", {
  # Step 10 gate scenario (ADR 0005 / api-audit global gate): A twice-local
  # to f1, C twice-local to f2, B cross-formula. Driven through the REAL
  # ptr_app_grid coordinator -- the module-server call is internal, so it
  # must be intercepted with local_mocked_bindings (assignInNamespace cannot
  # reach an internal call under devtools; see test-shared-stage-checkbox.R).
  cf1 <- paste0('ggplot(mtcars) + geom_point(aes(x = var(shared = "B"), ',
                'y = mpg), size = num(shared = "A"), ',
                'alpha = num(shared = "A"))')
  cf2 <- paste0('ggplot(mtcars) + geom_point(aes(x = var(shared = "B"), ',
                'y = hp), size = num(shared = "C"), ',
                'alpha = num(shared = "C"))')

  # (a) Partition is authoritative: B -> panel, A -> f1-local, C -> f2-local.
  obj <- ptr_shared(formulas = list(cf1, cf2))
  expect_equal(obj$panel_keys, "B")
  expect_equal(obj$local_keys_by_formula[[1]], "A")
  expect_equal(obj$local_keys_by_formula[[2]], "C")

  # (b) The standalone panel owns exactly the cross-formula key.
  panel_html <- paste(as.character(ptr_ui_shared_panel(obj)), collapse = "")
  expect_match(panel_html, "shared_B", fixed = TRUE)
  expect_no_match(panel_html, "shared_A", fixed = TRUE)
  expect_no_match(panel_html, "shared_C", fixed = TRUE)

  # (c) Formula-local keys render under their own module only.
  grid_html <- paste(
    as.character(ptr_app_grid_components(
      list(cf1, cf2), expr_check = FALSE,
      envir = new.env(parent = baseenv())
    )$ui),
    collapse = ""
  )
  expect_match(grid_html, "plot_1-shared_A", fixed = TRUE)
  expect_match(grid_html, "plot_2-shared_C", fixed = TRUE)
  expect_no_match(grid_html, "plot_1-shared_C", fixed = TRUE)
  expect_no_match(grid_html, "plot_2-shared_A", fixed = TRUE)

  # (d) The single panel B widget drives BOTH plots in lockstep.
  parts <- ptr_app_grid_components(
    list(cf1, cf2), expr_check = FALSE,
    envir = new.env(parent = baseenv())
  )
  ms <- new.env(parent = emptyenv())
  orig_ms <- ggpaintr:::ptr_server
  testthat::local_mocked_bindings(
    ptr_server = function(formula, id = NULL, ...) {
      r <- orig_ms(formula, id, ...)
      ms[[id]] <- r
      r
    },
    .package = "ggpaintr"
  )
  shiny::testServer(parts$server, {
    session$flushReact()
    p1 <- ms[["plot_1"]]
    p2 <- ms[["plot_2"]]

    session$setInputs(shared_B = "wt", ptr_shared_draw_all = 1L)
    session$flushReact()
    expect_match(ptr_extract_code(p1), "x = wt", fixed = TRUE)
    expect_match(ptr_extract_code(p2), "x = wt", fixed = TRUE)

    session$setInputs(shared_B = "drat", ptr_shared_draw_all = 2L)
    session$flushReact()
    expect_match(ptr_extract_code(p1), "x = drat", fixed = TRUE)
    expect_match(ptr_extract_code(p2), "x = drat", fixed = TRUE)
  })
})
