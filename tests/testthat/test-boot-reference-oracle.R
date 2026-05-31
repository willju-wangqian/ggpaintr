# test-boot-reference-oracle.R -- boot-state reference oracle (handoff
# deliverable (a); closes gap #2 "the reference.R oracle is never consulted").
#
# Each super-app fixture ships a sibling reference.R: the Path-B expression in
# which every `pp*(default)` collapses to its positional default. The
# shared-consumer boot-default discard fixed at cddc46e lived ONLY in the
# untouched boot state -- every super-pressure scenario drove the shared
# widget before asserting, so the suite never compared first-render code to
# the reference. This file does exactly that comparison, at the deliberately
# narrow consumer-default granularity (see expect_boot_matches_reference /
# ggp_reference_consumer_mappings in helper-super-pressure.R for the rationale
# and the upload-scope exclusion).
#
# Red-first provenance (handoff success-criterion #1): with cddc46e's seed fix
# reverted (R/paintr-server.R checked out to cddc46e^), the SHARED mappings
# below -- super-1 color/facet = cyl, super-2b color/facet = Species, super-3
# color = cyl -- boot to the first column instead and these assertions FAIL.
# With the fix restored they PASS. A guard added green is unverified; this one
# was proven to bite.
#
# These are e2e browser tests: skip_on_cran() + skip_if_not_installed via
# boot_super_app(); only NOT_CRAN=true / devtools::test() runs them.

test_that("boot oracle: super-1 kitchen-sink consumer defaults match reference.R at first render", {
  app <- boot_super_app("super-1-kitchen-sink")
  app$wait_for_idle(timeout = 25 * 1000)
  mappings <- expect_boot_matches_reference(app, "super-1-kitchen-sink",
                                            formula_name = "formula1")
  # Sanity: the reference declares x, y, color(shared grp), facet(shared grp).
  testthat::expect_equal(length(mappings), 4L)
})

test_that("boot oracle: super-2a source-shortcut defaults match reference.R (all-upload consumer exclusion)", {
  # super-2a is all-upload: root data is `ppUpload(df_main) |> ...` and
  # geom_smooth reads `data = ppUpload(df_aux)`, so every CONSUMER slot is
  # upload-scoped and reference.R declares ZERO non-upload consumer mappings.
  # A naive `boot == reference` string diff would false-positive here.
  ref_path <- testthat::test_path("fixtures", "vignette-apps",
                                  "super-2a-upload-registry", "reference.R")
  mappings <- ggp_reference_consumer_mappings(ref_path, formula_name = NULL)
  testthat::expect_equal(length(mappings), 0L)

  # SOURCE side. Because the consumer oracle is empty here, super-2a's app.R
  # had NO boot-oracle coverage at all -- which is exactly how commit 332f7b7's
  # `ppUpload(df_main)/(df_aux)` -> `ppUpload()` strip rode through this file
  # (caught only by test-source-shortcut-preserves). Boot the app and assert
  # each ppUpload's shortcut textbox seeds to reference.R's positional default
  # (df_main / df_aux). A re-strip to bare ppUpload() boots "" and FAILS here,
  # so the oracle now bites the source side too.
  app <- boot_super_app("super-2a-upload-registry")
  app$wait_for_idle(timeout = 25 * 1000)
  expect_boot_source_defaults_match_reference(
    app, "super-2a-upload-registry", formula_name = NULL
  )
})

test_that("boot oracle: super-2b customsource-splice consumer defaults match reference.R at first render", {
  app <- boot_super_app("super-2b-customsource-splice")
  app$wait_for_idle(timeout = 25 * 1000)
  mappings <- expect_boot_matches_reference(app, "super-2b-customsource-splice",
                                            formula_name = "formula")
  # x, y, color(shared fac), facet(shared fac); geom_rug aes excluded (upload).
  testthat::expect_equal(length(mappings), 4L)
})

test_that("boot oracle: super-2b boot PLOT (built data) equals reference.R", {
  # Stronger than the consumer-default code-substring oracle above: this
  # asserts the actual plot. (1) `expect_no_plot_error` proves the LIVE
  # server render succeeded at boot -- this is what catches the unbound
  # `ppUpload(df_rug)` shortcut regression (df_rug absent => "object
  # 'df_rug' not found" => the consumer-default text oracle stayed green
  # while the plot was broken). (2) the app's emitted final-mode code,
  # eval'd in reference.R's sandbox, must `ggplot_build()$data`-match
  # reference.R's own plot -- catching a semantically-wrong-but-evaluable
  # emitted expression. Object identity is NOT used (every ggplot carries a
  # distinct plot_env); built layer data is the robust equality.
  ref <- ggp_reference_plot("super-2b-customsource-splice")
  app <- boot_super_app("super-2b-customsource-splice")
  app$wait_for_idle(timeout = 25 * 1000)
  expect_boot_plot_matches_reference(app, ref)
})

test_that("boot oracle: super-3 L3 cells' consumer defaults match reference.R at first render", {
  testthat::skip_if_not_installed("plotly")
  # Previously PINNED xfail: the shared consumer `linked` booted to the first
  # column ("mpg") instead of its formula default ("cyl"). Root cause was NOT
  # the host-scope binder (as first hypothesised) but the now-removed
  # `shared_ui` per-key override: the fixture supplied a static
  # `selectInput(names(mtcars))` (no `selected=`) for `linked`, bypassing the
  # `ppVar` default-injection path. `shared_ui` is gone (see ?ptr_shared); the
  # fixture's `linked` now auto-renders from `ppVar`'s `build_ui` and seeds
  # `cyl` at boot. The slot + picker assertions are now ACTIVE (no xfail).
  app <- boot_super_app("super-3-l3-multi-shared-plotly")
  app$wait_for_idle(timeout = 25 * 1000)
  # Cell A (plain ggplot output): plot1-* ids.
  ma <- expect_boot_matches_reference(
    app, "super-3-l3-multi-shared-plotly", formula_name = "formula_a",
    code_output_id = "plot1-ptr_code",
    draw_button_id = "plot1-ptr_update_plot",
    code_mode_id   = "plot1-ptr_code_mode"
  )
  testthat::expect_equal(length(ma), 3L)
  # Cell B (plotly-wrapped output): plot2-* ids. The shared "linked" widget is
  # host-owned (shared_linked), so shared_prefix stays "".
  mb <- expect_boot_matches_reference(
    app, "super-3-l3-multi-shared-plotly", formula_name = "formula_b",
    code_output_id = "plot2-ptr_code",
    draw_button_id = "plot2-ptr_update_plot",
    code_mode_id   = "plot2-ptr_code_mode"
  )
  testthat::expect_equal(length(mb), 3L)
})

test_that("boot oracle: super-4 user-css consumer defaults match reference.R at first render", {
  app <- boot_super_app("super-4-user-css-safety-adversarial")
  app$wait_for_idle(timeout = 25 * 1000)
  # reference.R assembles its formula as a string via paste0 -> formula_text.
  # All three consumers (x, y, color) are non-shared; this is a regression net
  # over the already-guarded non-shared binder.
  mappings <- expect_boot_matches_reference(app, "super-4-user-css-safety-adversarial",
                                            formula_name = "formula_text")
  testthat::expect_equal(length(mappings), 3L)
})

# ---- boot PLOT equivalence (built data), beyond the consumer-default text
# oracle above. Each asserts (1) the live render produced no error pane at
# boot, and (2) the app's emitted final-mode code, eval'd in reference.R's
# sandbox, ggplot_build()$data-matches reference.R's own plot. See
# expect_boot_plot_matches_reference / ggp_reference_plot in the helper.

test_that("boot oracle: super-1 boot PLOT (built data) equals reference.R", {
  ref <- ggp_reference_plot("super-1-kitchen-sink")
  app <- boot_super_app("super-1-kitchen-sink")
  app$wait_for_idle(timeout = 25 * 1000)
  expect_boot_plot_matches_reference(app, ref)
})

test_that("boot oracle: super-4 boot PLOT (built data) equals reference.R", {
  ref <- ggp_reference_plot("super-4-user-css-safety-adversarial")
  app <- boot_super_app("super-4-user-css-safety-adversarial")
  app$wait_for_idle(timeout = 25 * 1000)
  expect_boot_plot_matches_reference(app, ref)
})

test_that("boot oracle: super-3 L3 cells' boot PLOTS (built data) equal reference.R", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_super_app("super-3-l3-multi-shared-plotly")
  app$wait_for_idle(timeout = 25 * 1000)
  # Cell A (plain ggplot output): plot1-* ids, reference formula_a.
  ref_a <- ggp_reference_plot("super-3-l3-multi-shared-plotly",
                              formula_name = "formula_a")
  expect_boot_plot_matches_reference(
    app, ref_a,
    code_output_id = "plot1-ptr_code",
    draw_button_id = "plot1-ptr_update_plot",
    code_mode_id   = "plot1-ptr_code_mode"
  )
  # Cell B (plotly-wrapped output): plot2-* ids, reference formula_b.
  ref_b <- ggp_reference_plot("super-3-l3-multi-shared-plotly",
                              formula_name = "formula_b")
  expect_boot_plot_matches_reference(
    app, ref_b,
    code_output_id = "plot2-ptr_code",
    draw_button_id = "plot2-ptr_update_plot",
    code_mode_id   = "plot2-ptr_code_mode"
  )
})
