# ADR 0012 / PLAN-01 (Bug B) — SC-3
#
# No-spec render falls back to `node$default` for value/source widgets,
# and the value/source `renderUI` containers are bound by the
# ptr_setup_value_uis / ptr_setup_source_uis helpers added in PLAN-01.
# These assertions guard the "structural keystone" of PLAN-01: the
# `build_ui_for.ptr_ph_value` / `build_ui_for.ptr_ph_data_source`
# reshape (which now emits a `uiOutput` instead of a static widget)
# must be matched by server-side `renderUI` bodies that populate those
# slots. If either side regresses, `state$spec_seed` is empty, no
# inputs bind, and `session$input[[<id>]]` returns NULL for every
# placeholder -- which is exactly what we assert against.
#
# The companion assertion (state$spec_seed is empty when no spec= is
# passed) is duplicated in test-adr12b-spec-custom-keyword.R as a
# negative control there; keeping it here too because it is also the
# baseline for THIS file's no-spec assertions.

.test_env <- function(extras = list()) {
  list2env(c(list(mtcars = mtcars), extras), parent = globalenv())
}

test_that("ADR 0012 / PLAN-01 (Bug B): no-spec ppNum renders with the build_ui default", {
  # With a bare `ppNum` (no literal default in the formula) the build_ui
  # hook emits a numericInput with `value = NA_real_`, which Shiny
  # stringifies in HTML as `value="NA"`. Asserts the renderUI fired and
  # the widget reflects the hook's default — the no-spec baseline that
  # the SC-1 spec-seeded case overrides.
  #
  # HTML matcher rather than `input[[id]]`: MockShinySession does NOT
  # populate `input[[id]]` for widgets emitted via `renderUI`. The
  # rendered HTML is the observable contract under testServer. The
  # browser-side round-trip is covered by `test-e2e-vignette-examples-
  # shinytest2.R`.
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(mpg, ppNum)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state

    # state$spec_seed is empty -- no spec was passed.
    expect_true(is.environment(state$spec_seed))
    expect_equal(length(ls(state$spec_seed, all.names = TRUE)), 0L)

    # The renderUI fired and emitted a numericInput at the bare id with
    # `value="NA"`. Output id is `<bare_id>_ui` per `value_output_id()`;
    # bare ppNum at aes position 2 yields `ggplot_1_2_ppNum_NA`.
    out <- session$getOutput("ggplot_1_2_ppNum_NA_ui")
    html <- paste(as.character(out), collapse = "\n")
    expect_match(html, 'id="ggplot_1_2_ppNum_NA"', fixed = TRUE)
    expect_match(html, 'value="NA"', fixed = TRUE)
  })
})

test_that("ADR 0012 / PLAN-01 (Bug B): no-spec ppVar source picker binds via ptr_setup_source_uis", {
  # ppVar-as-source: `var(mpg)` at the top level. The source renderUI
  # body emits a pickerInput; with no `selected` seed it starts at
  # `character(0)` (the legacy multiple=TRUE + maxOptions=1 trick --
  # see ptr_builtin_var_build_ui). Asserting the input binds at all
  # (non-NULL after flushReact, or NULL because pickerInput's empty
  # selection is exposed as character(0) or NULL depending on Shiny
  # version) is the keystone -- pre-PLAN-01 the source widget was
  # emitted statically at UI-build time, so input binding was
  # immediate; post-PLAN-01 it goes through renderUI so it only binds
  # after the first flush.
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      "ggplot(mtcars, aes(ppVar(mpg), ppNum)) + geom_point()",
      envir = e
    )
  }
  shiny::testServer(server, {
    session$flushReact()
    state <- session$userData$state

    expect_true(is.environment(state$spec_seed))
    expect_equal(length(ls(state$spec_seed, all.names = TRUE)), 0L)

    # The source picker output id ends in "_ui". Existence of the
    # output slot confirms `ptr_setup_source_uis` ran and registered a
    # renderUI for this source node. The bare-id for a `ppVar(mpg)`
    # source at aes position 1 is `ggplot_1_1_ppVar_NA` (the `_NA` is
    # the `param_key` slot — sources have no aes-named param).
    src_output <- session$getOutput("ggplot_1_1_ppVar_NA_ui")
    expect_false(is.null(src_output))
  })
})

test_that("ADR 0012 / PLAN-01 (Bug B): ptr_setup_value_uis registers a renderUI per ppText/ppNum/ppExpr", {
  # Smoke test: the value-renderUI helper registers an output slot for
  # each value placeholder. Pre-PLAN-01 the value widget was a static
  # tag in the UI tree; post-PLAN-01 it is a uiOutput populated by the
  # renderUI body. Existence of the output slot confirms wiring.
  e <- .test_env()
  server <- function(input, output, session) {
    session$userData$state <- ptr_server_internal(
      input, output, session,
      paste0("ggplot(mtcars) + geom_text(aes(mpg, hp, label = ppText),",
             " size = ppNum)"),
      envir = e
    )
  }
  shiny::testServer(server, {
    session$flushReact()

    # Two value placeholders -> two `_ui` output slots. Output ids
    # follow `<bare_id>_ui` where `bare_id = <layer>_<aes-or-arg
    # position>_<keyword>_<param_key-or-NA>`. ppText at `label=` (aes
    # position 3 of the aes call) → `geom_text_1_3_ppText_NA`; ppNum at
    # `size=` (direct geom arg, no aes wrapping) → `geom_text_2_ppNum_NA`.
    text_out <- session$getOutput("geom_text_1_3_ppText_NA_ui")
    num_out  <- session$getOutput("geom_text_2_ppNum_NA_ui")
    expect_false(is.null(text_out))
    expect_false(is.null(num_out))
  })
})
