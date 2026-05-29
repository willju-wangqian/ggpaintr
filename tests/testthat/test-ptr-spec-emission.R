# PLAN-05 (ADR 0012 §3.5; ADR 0022) — `ptr_spec` emission in spec-mode code panel.
#
# Tests the three new internal helpers + the spec-mode panel
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

# spec-mode code panel integration tests previously here (state$spec
# sparse on defaults, ptr_spec block on non-default pick, empty-spec
# placeholder line) were migrated to the J12 browser journey on
# 2026-05-27. See test-j12-spec-and-renderui-emission-journey.R and
# dev/audit/audit-test-fidelity-v8-j12-browser-faithfulness-2026-05-27-
# 2337.html for routing rationale.
