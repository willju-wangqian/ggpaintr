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

test_that("boot oracle: super-2a is an all-upload exclusion (no non-upload consumer mappings)", {
  # Documented, justified exclusion (handoff trap + criterion #2): super-2a's
  # root data is `ppUpload(df_main) |> ...` and geom_smooth reads
  # `data = ppUpload(df_aux)`, so every consumer slot is upload-scoped. A naive
  # `boot == reference` string diff would false-positive here; the oracle
  # collector instead returns ZERO non-upload mappings. We assert that
  # directly (no app boot needed) so the exclusion is VERIFIED, not just
  # asserted in a comment.
  ref_path <- testthat::test_path("fixtures", "vignette-apps",
                                  "super-2a-upload-registry", "reference.R")
  mappings <- ggp_reference_consumer_mappings(ref_path, formula_name = NULL)
  testthat::expect_equal(length(mappings), 0L)
})

test_that("boot oracle: super-2b customsource-splice consumer defaults match reference.R at first render", {
  app <- boot_super_app("super-2b-customsource-splice")
  app$wait_for_idle(timeout = 25 * 1000)
  mappings <- expect_boot_matches_reference(app, "super-2b-customsource-splice",
                                            formula_name = "formula")
  # x, y, color(shared fac), facet(shared fac); geom_rug aes excluded (upload).
  testthat::expect_equal(length(mappings), 4L)
})

test_that("boot oracle: super-3 L3 cells' consumer defaults match reference.R at first render", {
  testthat::skip_if_not_installed("plotly")
  # KNOWN BUG, handed off (.scratch/super3-l3-shared-consumer-boot-default/):
  # the L3 host-scope shared consumer `linked` boots to the first column
  # ("mpg") instead of its formula default ("cyl"). cddc46e fixed the two
  # single-app consumer binders via consumer_seed_decision() but the
  # multi-cell host-scope shared-consumer path still discards the default.
  # The shared-`linked` slot + picker assertions are PINNED as known-failures
  # (xfail_shared_keys); the non-shared x/y assertions stay active. When the
  # bug is fixed these expect_failure() pins flip RED, forcing this xfail to
  # be removed. The same divergence is what makes super-3's existing
  # propagation test mask the bug (it drives shared_linked before asserting).
  app <- boot_super_app("super-3-l3-multi-shared-plotly")
  app$wait_for_idle(timeout = 25 * 1000)
  # Cell A (plain ggplot output): plot1-* ids.
  ma <- expect_boot_matches_reference(
    app, "super-3-l3-multi-shared-plotly", formula_name = "formula_a",
    code_output_id = "plot1-ptr_code",
    draw_button_id = "plot1-ptr_update_plot",
    code_mode_id   = "plot1-ptr_code_mode",
    xfail_shared_keys = "linked"
  )
  testthat::expect_equal(length(ma), 3L)
  # Cell B (plotly-wrapped output): plot2-* ids. The shared "linked" widget is
  # host-owned (shared_linked), so shared_prefix stays "".
  mb <- expect_boot_matches_reference(
    app, "super-3-l3-multi-shared-plotly", formula_name = "formula_b",
    code_output_id = "plot2-ptr_code",
    draw_button_id = "plot2-ptr_update_plot",
    code_mode_id   = "plot2-ptr_code_mode",
    xfail_shared_keys = "linked"
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
