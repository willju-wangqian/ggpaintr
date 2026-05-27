# ADR 0015 PLAN-02 / Option E regression — race-provable test that the
# autoname-vs-assign race PLAN-01 worked around with invalidateLater(50)
# + .binding_pending poll is gone STRUCTURALLY (via server-side
# state$bound_names reactiveVal). See:
#   dev/audit/audit-adr15-autoname-race-20260523-204227.html  (race mechanism)
#   dev/plans/0015-consumer-picker-source-headed-resolution/02-server-side-bound-names.html  (fix design)
#
# Setup: fixture's CSV is named penguins.csv (autoname will fire
# updateTextInput(comp_id, "penguins")) but its columns (col_a, col_b)
# are disjoint from datasets::penguins's (species/island/bill_length_mm/
# body_mass_g/...). If the race fires — entry_reactive snapshots
# comp_id="penguins" before state$eval_env holds the binding — R's
# parent-chain scoping resolves penguins to datasets::penguins and the
# downstream picker would show bill_length_mm etc. The test asserts
# NEITHER any datasets::penguins column appears in any var picker, NOR
# is any picker empty (positive assertion proves the picker bound at all).

test_that("adr15 option E: autoname-fired upload does NOT leak datasets::penguins via scoping fallback", {
  fixture <- "adr15-option-e-race"
  app <- boot_vignette_app(fixture)

  csv_path <- testthat::test_path(
    "fixtures", "vignette-apps", fixture, "penguins.csv"
  )
  app$upload_file(ggplot_1_ppUpload_NA = csv_path)
  # Deliberately do NOT set ggplot_1_ppUpload_NA_shortcut — let
  # ptr_bind_source_autoname() fire its updateTextInput("penguins").
  # That client-roundtripped input update is exactly the signal whose
  # ordering vs. the upload observer's assign() PLAN-01's polling
  # worked around. PLAN-02's req(state$bound_names[[id]]()) closes the
  # race structurally: the consumer's renderUI cannot fire until the
  # source observer has written bound_names AFTER assign() lands.
  app$wait_for_idle(timeout = 15 * 1000)

  uploaded_cols <- c("col_a", "col_b")
  leaked_cols   <- c(
    "bill_length_mm", "bill_depth_mm",
    "flipper_length_mm", "body_mass_g",
    "species", "island"
  )

  pickers <- c(
    "ggplot_1_1_ppVar_NA",
    "ggplot_1_2_ppVar_NA",
    "ggplot_2_1_1_ppVar_NA"
  )
  for (picker in pickers) {
    html <- app$get_html(paste0("#", picker))
    testthat::expect_true(
      !is.null(html) && nzchar(html),
      label = paste0("picker #", picker, " bound (non-empty HTML)")
    )
    # Positive: each uploaded column appears as a picker choice.
    for (col in uploaded_cols) {
      testthat::expect_true(
        grepl(col, html, fixed = TRUE),
        label = paste0("picker #", picker, " offers uploaded col '",
                       col, "'")
      )
    }
    # Negative (the race-provable check): no datasets::penguins column
    # leaks through R's scoping fallback. Any hit here means the
    # autoname-vs-assign race fired and the wrong frame was used.
    for (col in leaked_cols) {
      testthat::expect_false(
        grepl(col, html, fixed = TRUE),
        label = paste0("picker #", picker,
                       " must NOT leak datasets::penguins col '",
                       col, "' (race fired -> scoping fallback)")
      )
    }
  }
})
