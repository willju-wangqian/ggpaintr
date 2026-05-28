# J1 journey -- adr10-ppupload-name + adr12-spec-roundtrip + adr24-companion-typed.
#
# Browser-driven coverage for one upload-naming contract that absorbed
# 2 STRONG L2 blocks (test-spec-roundtrip-bound-names-fallback.R :25 +
# :94). 3 more L2 deletes ride a pre-existing L3 sibling; 5 audit-listed
# L2 blocks were already absorbed in a prior round and no longer exist
# at the audit's cited line numbers; 2 audit-listed L2 blocks were
# empirically demoted to RETAIN during the v10 routing pass when the
# probe showed their contract surface is not DOM-observable (see
# "Routing" below).
#
# Routing decisions (dev/audit/audit-test-fidelity-v10-j1-browser-
# faithfulness-2026-05-28-0235.html):
#   MERGED via NEW stages here:
#     test-spec-roundtrip-bound-names-fallback.R:25 (empty shortcut +
#                                                    bound_names => spec
#                                                    carries auto-name)
#     test-spec-roundtrip-bound-names-fallback.R:94 (typed shortcut wins
#                                                    over bound_names fallback)
#       -> stage 1 below (via new fixture j1-ppupload-spec-roundtrip)
#   MERGED via PRE-EXISTING L3 sibling (no new stage needed):
#     test-ppupload-name-e2e.R:46 (bareword default + uiOutput companion slot)
#     test-ppupload-name-e2e.R:75 (ptr_substitute emits bare symbol)
#     test-ppupload-name-e2e.R:98 (live-eval via eval_env parent chain)
#       -> all covered by test-adr10-ppupload-name-browser.R:17 (companion
#          textInput is seeded with "penguins" at boot AND the plot renders
#          via caller-env auto-resolve WITHOUT any file upload -- a chain
#          that is only reachable if (a) translate captured the bareword
#          default, (b) renderUI populated the uiOutput, (c) ptr_substitute
#          emitted the bare symbol, AND (d) live-eval found `penguins` via
#          eval_env's parent chain).
#   STALE -- already absorbed in a prior round:
#     test-ppupload-preserve-mode.R:26/:35/:70/:82/:91 -- clusters audit's
#       citation pre-dates the prior absorption round; the file's own
#       header (test-ppupload-preserve-mode.R:24-32) documents these were
#       absorbed into test-adr10-ppupload-name-browser.R:84/:127 on
#       2026-05-27. Current file contents are unrelated RETAIN-class
#       unit pins at :36 / :44; not touched by J1.
#   RETAIN (empirical demotion during v10 probe; see audit §7):
#     test-auto-name-shared-canonical.R:11 (shared auto_name = "main_ds")
#     test-auto-name-shared-canonical.R:74 (ptr_setup_panel_sources stamps)
#       -> probe of a candidate `j1-shared-ppupload-main` fixture
#          (ptr_shared(plots, id = "main") with two ppUpload(shared='ds')
#          formulas) confirmed: after upload via `main-shared_ds`, the
#          auto_name literal `main_ds` NEVER appears in the rendered DOM.
#          Per-plot `substitute_walk` does not consume shared-source
#          bound_names; per-plot `state$spec()` does not include shared
#          shortcut ids. The contract is genuinely R-internal-only for
#          shared apps -- RETAIN as unit pins.
#
# Standard browser-e2e scaffolding (skip_on_cran / skip_if_not_installed /
# source-root guard) lives in boot_vignette_app() -- helper-vignette-apps.R.

test_that("J1 stage 1 (j1-ppupload-spec-roundtrip): empty shortcut + upload => spec carries auto-name; typed shortcut wins", {
  # Covers test-spec-roundtrip-bound-names-fallback.R:25 AND :94 in one
  # stage. Both assert that the snapshot-write loop in `ptr_setup_runtime()`
  # falls back to `state$bound_names[[key]]()` when the shortcut textbox
  # is empty (:25), and that a typed value wins over that fallback (:94).
  # The L2 unit tests reach this with manual `state$bound_names[[key]](...)`
  # writes via `shiny::testServer`; the DOM-faithful equivalent uses a real
  # `app$upload_file()` (the upload observer writes bound_names itself)
  # and observes the spec-mode panel text, which renders the spec list
  # via `format_spec_for_panel()` (paintr-server.R:456) as
  #   ptr_spec <- list(
  #     `<key>` = <value>,
  #     ...
  #   )
  #
  # The auto-name for this formula (`ppUpload |> ggplot(...)`) is the
  # source id itself `ggplot_0_ppUpload_NA` (no `default=` in the
  # placeholder; the bareword fallback uses `node$id`); the shortcut id
  # is `ggplot_0_ppUpload_NA_shortcut`. Probe-verified this session.
  app <- boot_vignette_app("j1-ppupload-spec-roundtrip")

  shortcut_id <- "ggplot_0_ppUpload_NA_shortcut"
  source_id   <- "ggplot_0_ppUpload_NA"

  # ---- :25 -- empty shortcut + upload => spec at shortcut id == auto-name
  app$upload_file(ggplot_0_ppUpload_NA = testthat::test_path(
    "fixtures", "mtcars.csv"
  ))
  app$wait_for_idle(timeout = 15 * 1000)

  draw(app, "ptr_update_plot")
  app$set_inputs(ptr_code_mode = "spec", wait_ = FALSE)
  app$wait_for_idle(timeout = 15 * 1000)

  spec_text_empty <- app$get_value(output = "ptr_code")
  # ptr_spec format: `<key>` = <deparsed value>.  Assert the shortcut id
  # entry is present AND its value is the auto-name (the source id).
  expect_match(
    spec_text_empty,
    paste0("`", shortcut_id, "` = \"", source_id, "\""),
    fixed = TRUE,
    label = "spec-mode panel emits `<shortcut_id>` = \"<auto-name>\" when shortcut is empty"
  )

  # ---- :94 -- type into shortcut; spec at shortcut id == typed value
  set_input(app, shortcut_id, "mtcars")
  draw(app, "ptr_update_plot")
  app$wait_for_idle(timeout = 15 * 1000)

  spec_text_typed <- app$get_value(output = "ptr_code")
  expect_match(
    spec_text_typed,
    paste0("`", shortcut_id, "` = \"mtcars\""),
    fixed = TRUE,
    label = "typed shortcut wins -- spec entry value becomes the typed `mtcars`"
  )
  expect_false(
    grepl(
      paste0("`", shortcut_id, "` = \"", source_id, "\""),
      spec_text_typed, fixed = TRUE
    ),
    label = "auto-name no longer appears at the shortcut id once the user types"
  )
})
