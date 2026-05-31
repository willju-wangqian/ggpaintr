# Regression: toggling a pipeline-stage checkbox MUST NOT reset the
# ppUpload shortcut textInput to `node$default`.
#
# Root cause (pre-fix): `ptr_setup_source_uis()` declared a reactive dep on
# `state$stage_enabled()`. Every checkbox toggle invalidated the source
# renderUI, which re-emitted `ptr_builtin_upload_build_ui()`, which seeded
# the shortcut textInput from `node$default %||% ""` — wiping any
# user-typed value or upload-derived auto-name.
#
# Fix: drop the spurious dep at R/paintr-server.R::ptr_setup_source_uis
# (the renderUI now depends on `state$tree()` only). Stage CSS toggling
# still works because it propagates via `sendCustomMessage("ptr_set_class",
# ...)` in `ptr_setup_stage_enabled`, not via UI re-render.

test_that("source shortcut survives stage-checkbox toggle", {
  app <- boot_super_app("super-2a-upload-registry")

  # ---- Initial state ------------------------------------------------------
  # Both ppUpload shortcuts boot to their formula-symbol defaults
  # (df_main / df_aux) per the seed-companion contract.
  expect_equal(
    app$get_value(input = "ggplot_1_ppUpload_NA_shortcut"),
    "df_main"
  )

  # ---- User types a different name into the head ppUpload shortcut -------
  set_sentinel(app, "ggplot_1_ppUpload_NA_shortcut", "iris")
  expect_equal(
    app$get_value(input = "ggplot_1_ppUpload_NA_shortcut"),
    "iris"
  )

  # ---- Toggle the dplyr::filter stage off (the pre-fix trigger) ----------
  # Stage ids on this fixture: ggplot_2_stage_enabled (filter) and
  # ggplot_3_stage_enabled (mutate). Either reproduces the bug; pick
  # filter to match the reported repro.
  set_sentinel(app, "ggplot_2_stage_enabled", FALSE)

  # ---- Assert the shortcut survives --------------------------------------
  # Pre-fix observation: this value reverts to "df_main". Post-fix: stays
  # at "iris".
  expect_equal(
    app$get_value(input = "ggplot_1_ppUpload_NA_shortcut"),
    "iris",
    label = paste0(
      "shortcut textInput must not be reset by a stage-checkbox toggle; ",
      "regression of `state$stage_enabled()` dep in ptr_setup_source_uis"
    )
  )

  # ---- And the other stage too, to lock the invariant down ---------------
  set_sentinel(app, "ggplot_3_stage_enabled", FALSE)
  expect_equal(
    app$get_value(input = "ggplot_1_ppUpload_NA_shortcut"),
    "iris"
  )

  # ---- The OTHER source's shortcut is also preserved ---------------------
  expect_equal(
    app$get_value(input = "geom_smooth_0_ppUpload_NA_shortcut"),
    "df_aux"
  )
})

# ---- Property-style sweep -----------------------------------------------
#
# Locks down the invariant across every (shortcut × stage_checkbox) pair on
# the super-2a fixture. The explicit test above documents the original
# repro; this one defends against the broader pattern — *any* renderUI on a
# `_shortcut` host taking a dep on *any* `_stage_enabled` input would
# regress this sweep, not just the specific df_main/filter pair.
#
# Discovery is DOM-driven (no hardcoded ids beyond the sentinel-value
# mapping) so if the fixture grows new ppUpload sources or new pipeline
# stages, the test picks them up automatically.

test_that("every ppUpload shortcut survives every pipeline-stage toggle", {
  app <- boot_super_app("super-2a-upload-registry")

  # ID enumeration is regex-driven on the raw HTML so the test doesn't pull
  # xml2 into the test deps (xml2 is not in Suggests).
  html <- app$get_html("body")
  extract_ids <- function(suffix) {
    m <- regmatches(
      html,
      gregexpr(paste0("id=\"([A-Za-z0-9_]+", suffix, ")\""),
               html, perl = TRUE)
    )[[1]]
    gsub("^id=\"|\"$", "", m)
  }
  shortcut_ids <- extract_ids("_shortcut")
  stage_ids    <- extract_ids("_stage_enabled")
  testthat::expect_gt(length(shortcut_ids), 0L)
  testthat::expect_gt(length(stage_ids), 0L)

  # Seed every shortcut with a per-id sentinel. Using the id itself as the
  # value makes failure messages self-describing.
  sentinels <- setNames(paste0("sentinel_", shortcut_ids), shortcut_ids)
  for (sid in shortcut_ids) {
    set_sentinel(app, sid, sentinels[[sid]])
  }
  for (sid in shortcut_ids) {
    expect_equal(app$get_value(input = sid), sentinels[[sid]],
                 label = paste0("seeded ", sid))
  }

  # For every stage checkbox: toggle off → assert; toggle back on → assert.
  # Both transitions exercise `ptr_setup_stage_enabled` (which writes
  # `state$stage_enabled` either way), so both would have reproduced the
  # original bug.
  for (stage in stage_ids) {
    for (val in c(FALSE, TRUE)) {
      set_sentinel(app, stage, val)
      for (sid in shortcut_ids) {
        expect_equal(
          app$get_value(input = sid),
          sentinels[[sid]],
          label = paste0(
            "shortcut '", sid, "' must survive toggling '", stage,
            "' to ", val
          )
        )
      }
    }
  }
})
