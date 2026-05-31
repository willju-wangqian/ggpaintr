# J12 journey -- spec emission + renderUI emission.
#
# Browser-driven coverage for the two ADR 0012 wires:
#   (1) PLAN-05 spec-mode code panel: `state$spec()` reactive -> `output$ptr_code`
#       via `format_spec_for_panel` (placeholder-line on empty, ptr_spec block
#       on non-default picks).
#   (2) PLAN-01 renderUI emission: `ptr_setup_value_uis` /
#       `ptr_setup_source_uis` populate the `_ui` slots emitted by
#       `build_ui_for.ptr_ph_value` / `build_ui_for.ptr_ph_data_source`.
#
# Routing decisions (dev/audit/audit-test-fidelity-v8-j12-browser-
# faithfulness-2026-05-27-2337.html):
#   MERGED (deleted L2 -> covered here):
#     test-adr12b-renderui-emission.R   (whole file -- 3 blocks at :23/:61/:97)
#     test-adr12b-spec-custom-keyword.R :33 (the positive-case block only)
#     test-ptr-spec-emission.R          :133, :160, :200
#   RETAINED as unit pins (NOT covered here -- internal-shape contracts):
#     test-adr12b-spec-custom-keyword.R :100  (state$spec_seed empty on no
#                                              spec=; no DOM analog -- guards
#                                              against future refactors that
#                                              populate spec_seed from defaults)
#     test-ptr-spec-emission.R          :16 .. :116  (pure-helper unit blocks
#                                              for ptr_spec_from_snapshot /
#                                              format_spec_for_panel /
#                                              ptr_spec_combine -- not
#                                              absorbed)
#
# Standard browser-e2e scaffolding (skip_on_cran / skip_if_not_installed /
# source-root guard) lives in boot_vignette_app() -- helper-vignette-apps.R.

test_that("J12 stage 1 (j12-spec-emission-empty): empty-spec placeholder line on no override", {
  # Covers test-ptr-spec-emission.R:200 (DOM-explicit) AND :133 (DOM-equivalent).
  # Placeholder-free formula (mtcars + aes(x=mpg, y=hp), no ppXxx) -- runtime
  # fires on Update click with no overrides, so state$spec() is empty,
  # format_spec_for_panel emits the ADR 0022 placeholder line. (Note:
  # `app-basic` is NOT placeholder-free -- it has labs(title=ppText) whose
  # empty default lands in state$spec(); see j12-spec-emission-empty fixture
  # header for the rationale.)
  app <- boot_vignette_app("j12-spec-emission-empty")

  draw(app, "ptr_update_plot")
  wait_for_input_binding(app, "ptr_code_mode")
  app$set_inputs(ptr_code_mode = "spec", wait_ = TRUE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5 * 1000)

  code_txt <- app$get_value(output = "ptr_code")
  expect_match(
    code_txt, "No overrides yet", fixed = TRUE,
    label = "empty-spec placeholder line surfaces when state$spec() is empty"
  )
  expect_false(
    grepl("ptr_spec", code_txt, fixed = TRUE),
    label = "no `ptr_spec` literal when state$spec() is empty"
  )
})

test_that("J12 stage 2 (j12-spec-emission-vars): non-default picks emit ptr_spec block", {
  # Covers test-ptr-spec-emission.R:160. Pick both vars, click Update, switch
  # the code-mode radio to "spec"; assert the ADR-0022 spec block is present
  # with the two backtick-quoted bare ids AND the formula text is NOT
  # emitted (audience-split contract).
  app <- boot_vignette_app("j12-spec-emission-vars")

  set_input(app, "ggplot_1_1_ppVar_NA", "mpg")
  set_input(app, "ggplot_1_2_ppVar_NA", "hp")
  app$wait_for_idle(timeout = 5 * 1000)

  draw(app, "ptr_update_plot")
  wait_for_input_binding(app, "ptr_code_mode")
  app$set_inputs(ptr_code_mode = "spec", wait_ = TRUE, timeout_ = 10000)
  app$wait_for_idle(timeout = 5 * 1000)

  code_txt <- app$get_value(output = "ptr_code")
  expect_match(code_txt, "ptr_spec <- list(", fixed = TRUE)
  expect_match(code_txt, "`ggplot_1_1_ppVar_NA` = \"mpg\"", fixed = TRUE)
  expect_match(code_txt, "`ggplot_1_2_ppVar_NA` = \"hp\"", fixed = TRUE)
  # No formula text -- the panel emits ONLY the spec block (ADR 0022).
  expect_false(grepl("ggplot(", code_txt, fixed = TRUE))
  expect_false(grepl("ppVar(", code_txt, fixed = TRUE))
  expect_false(grepl("ptr_app(", code_txt, fixed = TRUE))
})

test_that("J12 stage 3 (j12-spec-custom-keyword): custom-keyword spec value applies at boot", {
  # Covers test-adr12b-spec-custom-keyword.R:33. The fixture registers
  # `ppCustomChoice` (selectInput, choices a/b/c, honors `selected` formal)
  # and boots with spec=list(ggplot_1_1_ppCustomChoice_NA = "b"). Assert
  # the rendered widget carries the seeded selection.
  #
  # Assertion shape (per v8 §7.3): regex `value="b"[^>]*selected` --
  # forward-compatible if Shiny ever changes attribute serialization.
  app <- boot_vignette_app("j12-spec-custom-keyword")

  html <- app$get_html("#ggplot_1_1_ppCustomChoice_NA") %||% ""
  expect_match(
    html, 'value="b"[^>]*selected', perl = TRUE,
    label = "selectInput's `b` option carries the seeded `selected` attribute"
  )
})

test_that("J12 stage 4 (j12-renderui-emission): bare ppNum renders the build_ui default value=\"NA\"", {
  # Covers test-adr12b-renderui-emission.R:23. The fixture's formula has
  # ppNum at ggplot aes position 2 with no literal default -- build_ui
  # emits `numericInput(value = NA_real_)`, stringified in HTML as
  # value="NA".
  app <- boot_vignette_app("j12-renderui-emission")

  html <- app$get_html("#ggplot_1_2_ppNum_NA") %||% ""
  expect_match(
    html, 'value="NA"', fixed = TRUE,
    label = "bare ppNum widget HTML carries build_ui's NA_real_ default"
  )
})

test_that("J12 stage 5 (j12-renderui-emission): ppVar source picker binds via ptr_setup_source_uis", {
  # Covers test-adr12b-renderui-emission.R:61. `ppVar(mpg)` at aes pos 1 is
  # a SOURCE; post-PLAN-01 the source widget goes through renderUI so it
  # only binds after the first flush. wait_for_input_binding succeeds iff
  # Shiny registered the input -- proving ptr_setup_source_uis ran.
  app <- boot_vignette_app("j12-renderui-emission")

  # _ui slot existence -- the uiOutput container is in the static UI tree.
  expect_dom_id(app, "ggplot_1_1_ppVar_NA_ui")
  # Bound input -- the renderUI body populated it and bindAll wired it.
  wait_for_input_binding(app, "ggplot_1_1_ppVar_NA")
})

test_that("J12 stage 6 (j12-renderui-emission): ptr_setup_value_uis registers a renderUI per ppText/ppNum", {
  # Covers test-adr12b-renderui-emission.R:97. geom_text's `label=ppText`
  # (the only positional arg of the inner aes() in this fixture -> pos 1)
  # and `size=ppNum` (direct geom arg, no aes wrapping -> position 2 of
  # the outer geom_text call) each get a `_ui` uiOutput slot from
  # ptr_setup_value_uis. bare_ids verified by tree probe against
  # ptr_translate() of the fixture's formula.
  app <- boot_vignette_app("j12-renderui-emission")

  expect_dom_id(app, "geom_text_1_1_ppText_NA_ui")
  expect_dom_id(app, "geom_text_2_ppNum_NA_ui")
})
