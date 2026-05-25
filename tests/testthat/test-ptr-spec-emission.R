# PLAN-05 (ADR 0012 §3.5) — `ptr_spec` emission in preserve-mode code panel.
#
# Tests the three new internal helpers + the preserve-mode panel
# integration:
#   * ptr_spec_from_snapshot(snapshot, defaults)  — pure sparse diff
#   * ptr_spec_combine(specs)                     — pure union with
#                                                   collision-abort
#   * format_spec_for_panel(spec)                 — deparse + light indent;
#                                                   "" on empty spec
#   * state$spec reactive                         — pull over runtime
#                                                   snapshot
#   * ptr_register_code preserve branch           — appends ptr_spec block

# ---- ptr_spec_from_snapshot ------------------------------------------------

test_that("snapshot with every entry at default produces an empty spec", {
  snapshot <- list(ggplot_2_stage_enabled = TRUE,
                   geom_point_1_active   = TRUE)
  defaults <- list(ggplot_2_stage_enabled = TRUE,
                   geom_point_1_active   = TRUE)
  out <- ggpaintr:::ptr_spec_from_snapshot(snapshot, defaults)
  expect_equal(length(out), 0L)
  expect_named(out, character())
})

test_that("snapshot with one default + one non-default produces a one-entry spec", {
  snapshot <- list(ggplot_2_stage_enabled = FALSE,  # non-default
                   geom_point_1_active   = TRUE)   # default
  defaults <- list(ggplot_2_stage_enabled = TRUE,
                   geom_point_1_active   = TRUE)
  out <- ggpaintr:::ptr_spec_from_snapshot(snapshot, defaults)
  expect_equal(length(out), 1L)
  expect_equal(names(out), "ggplot_2_stage_enabled")
  expect_false(out[["ggplot_2_stage_enabled"]])
})

test_that("snapshot ids missing from defaults are emitted (never invented as default)", {
  # Per the plan: registry entry with no documented default -> treat
  # snapshot value as non-default. Same logic applies to bare ids that
  # never appear in `defaults`.
  snapshot <- list(ggplot_1_1_ppVar_NA = "mpg")
  defaults <- list()  # nothing known
  out <- ggpaintr:::ptr_spec_from_snapshot(snapshot, defaults)
  expect_equal(length(out), 1L)
  expect_equal(out[["ggplot_1_1_ppVar_NA"]], "mpg")
})

test_that("identical-only default comparison: TRUE vs \"TRUE\" both emit", {
  # No type coercion: a "TRUE" string and a TRUE logical compare unequal
  # via `identical()`. Shiny preserves type at the wire so we never see
  # this pair in practice — the test is here to lock the contract.
  snapshot <- list(a = "TRUE")
  defaults <- list(a = TRUE)
  out <- ggpaintr:::ptr_spec_from_snapshot(snapshot, defaults)
  expect_equal(length(out), 1L)
  expect_equal(out[["a"]], "TRUE")
})

# ---- format_spec_for_panel -------------------------------------------------

test_that("format_spec_for_panel returns \"\" on an empty spec", {
  expect_identical(ggpaintr:::format_spec_for_panel(list()), "")
  expect_identical(
    ggpaintr:::format_spec_for_panel(stats::setNames(list(), character())),
    ""
  )
})

test_that("format_spec_for_panel produces parseable R for a non-empty spec", {
  spec <- list(`ggplot_2_stage_enabled` = FALSE,
               `ggplot_1_1_ppVar_NA`    = "mpg")
  txt <- ggpaintr:::format_spec_for_panel(spec)
  # The expected shape from the plan's Scenario / Success Criteria:
  expect_match(txt, "^ptr_spec <- list\\(")
  expect_match(txt, "`ggplot_2_stage_enabled` = FALSE", fixed = TRUE)
  expect_match(txt, "`ggplot_1_1_ppVar_NA` = \"mpg\"", fixed = TRUE)
  parsed <- rlang::parse_expr(txt)
  # The first element is `<-`, second is the LHS symbol, third is the
  # `list(...)` call.
  expect_identical(parsed[[1L]], as.name("<-"))
  expect_identical(parsed[[2L]], as.name("ptr_spec"))
  expect_identical(parsed[[3L]][[1L]], as.name("list"))
  evald <- eval(parsed[[3L]])
  expect_equal(evald, spec)
})

# ---- ptr_spec_combine ------------------------------------------------------

test_that("ptr_spec_combine unions per-plot specs", {
  spec_p1 <- list(`p1-ggplot_1_1_ppVar_NA` = "mpg")
  spec_p2 <- list(`p2-ggplot_2_stage_enabled` = FALSE)
  out <- ggpaintr:::ptr_spec_combine(list(spec_p1, spec_p2))
  expect_equal(length(out), 2L)
  expect_equal(
    sort(names(out)),
    sort(c("p1-ggplot_1_1_ppVar_NA", "p2-ggplot_2_stage_enabled"))
  )
  expect_equal(out[["p1-ggplot_1_1_ppVar_NA"]], "mpg")
  expect_false(out[["p2-ggplot_2_stage_enabled"]])
})

test_that("ptr_spec_combine aborts on colliding fully-qualified ids", {
  spec_a <- list(`p1-foo` = 1L)
  spec_b <- list(`p1-foo` = 2L)
  expect_error(
    ggpaintr:::ptr_spec_combine(list(spec_a, spec_b)),
    "p1-foo"
  )
})

test_that("ptr_spec_combine on empty input returns empty list", {
  expect_equal(length(ggpaintr:::ptr_spec_combine(list())), 0L)
  expect_equal(
    length(ggpaintr:::ptr_spec_combine(list(list(), list()))), 0L
  )
})

# ---- state$spec reactive ---------------------------------------------------

test_that("state$spec is an empty named list before the first runtime fires", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  state <- ptr_init_state(
    "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()",
    envir = e
  )
  shiny::isolate({
    out <- state$spec()
    expect_equal(length(out), 0L)
    expect_equal(names(out), character())
  })
})

test_that("state$spec is sparse when only defaults are picked (testServer)", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(ptr_update_plot = 1L)  # gates the runtime
    res <- state$runtime()
    expect_true(isTRUE(res$ok))
    # No placeholders, no checkboxes off, no stages off -> empty spec.
    expect_equal(length(state$spec()), 0L)
  })
})

# ---- spec-mode code panel integration --------------------------------------
# ADR 0022: the second code-panel radio choice was renamed from "preserve"
# (formula + spec) to "spec" (spec only). The pre-ADR-0022 tests in this
# section asserted the formula-text half of the preserve emission; those
# assertions are now covered as direct render-walker unit tests in
# test-render-preserve.R. The tests below cover the spec-emission half
# end-to-end through the panel reactive.

test_that("spec-mode panel emits ptr_spec block when a non-default pick exists", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(x = ppVar, y = ppVar)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    # Pick both vars; first click freezes the snapshot on res$snapshot.
    session$setInputs(
      ggplot_1_1_ppVar_NA = "mpg",
      ggplot_1_2_ppVar_NA = "hp",
      ptr_update_plot     = 1L
    )
    session$flushReact()
    res <- state$runtime()
    expect_true(isTRUE(res$ok))

    # Spec-mode rendering: switch the mode toggle and read the code output.
    # ADR 0022: the panel emits only the `ptr_spec <- list(...)` block;
    # no formula text precedes it.
    session$setInputs(ptr_code_mode = "spec")
    session$flushReact()
    code_txt <- output$ptr_code

    # ptr_spec block present.
    expect_match(code_txt, "ptr_spec <- list(", fixed = TRUE)
    expect_match(code_txt, "`ggplot_1_1_ppVar_NA` = \"mpg\"", fixed = TRUE)
    expect_match(code_txt, "`ggplot_1_2_ppVar_NA` = \"hp\"", fixed = TRUE)
    # No formula text — the panel is no longer a reproducer for non-owners
    # (audience-split rationale in ADR 0022). Formula source is owner-side.
    expect_false(grepl("ggplot(", code_txt, fixed = TRUE))
    expect_false(grepl("ppVar(", code_txt, fixed = TRUE))
    expect_false(grepl("ptr_app(", code_txt, fixed = TRUE))
  })
})

test_that("spec-mode panel shows the empty-spec placeholder line when nothing is overridden", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(x = mpg, y = hp)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    session$setInputs(ptr_update_plot = 1L)
    session$setInputs(ptr_code_mode = "spec")
    session$flushReact()
    code_txt <- output$ptr_code

    # Empty spec → ADR 0022 placeholder line surfaces (so the panel is
    # never confusingly blank pre-Update for first-time users).
    expect_match(code_txt, "No overrides yet", fixed = TRUE)
    # And no `ptr_spec` block (there's nothing to list).
    expect_false(grepl("ptr_spec", code_txt, fixed = TRUE))
  })
})
