# test-super-pressure.R — single-file home for the five super-app pressure tests
# (ADR 0013). Each app lives between an anchor-delimited region. The anchors
# exist so the five app plans (PLAN-02 .. PLAN-06) edit non-overlapping
# regions of this file in independent worktrees.
#
# Anchor pairs (locked by ADR 0013 D3 + PLAN-01; later plans MUST NOT remove
# or rename them, only fill them):
#   # >>> super-1  begin / # <<< super-1  end   -> filled by PLAN-02
#       (super-app-1  kitchen-sink)
#   # >>> super-2a begin / # <<< super-2a end   -> filled by PLAN-03
#       (super-app-2a upload registry)
#   # >>> super-2b begin / # <<< super-2b end   -> filled by PLAN-04
#       (super-app-2b customsource splice)
#   # >>> super-3  begin / # <<< super-3  end   -> filled by PLAN-05
#       (super-app-3  L3 multi-shared plotly)
#   # >>> super-4  begin / # <<< super-4  end   -> filled by PLAN-06
#       (super-app-4  user-css safety-adversarial)
#
# Each region holds at most ONE `test_that()` block. The sentinel-propagation
# helpers used inside live in `helper-super-pressure.R` (auto-loaded by
# testthat from this directory). Per ADR 0013, every assertion in this file
# names an exact sentinel + an exact position regex + an explicit mode — no
# presence-style proxies (see project memory `e2e-assertion-weakness-lens`).

# >>> super-1 begin
test_that("super-1 kitchen-sink: sentinels propagate through every placeholder", {
  app <- boot_super_app("super-1-kitchen-sink")

  # ---- C6 shared-default tiebreak ----------------------------------------
  # `ppNum(1, shared = "lw")` in geom_smooth seeds the shared widget; the
  # defaultless `ppNum(shared = "lw")` in geom_line is silently ignored.
  # First-occurrence wins (ADR §App-1 pressure axis iii).
  expect_equal(app$get_value(input = "shared_lw"), 1)

  # ---- Drive every sentinel ----------------------------------------------
  # ppExpr sentinel — dplyr::mutate(adj = ppExpr(mpg / wt)). Set first so
  # the upstream pipeline resolves with the new derived column "adj" before
  # the downstream y = ppVar(adj) picker reads its column choices.
  # The widget is a text input whose value is the unevaluated expression text.
  set_sentinel(app, "ggplot_3_1_ppExpr_NA", "hp + 1")
  # Activate the ggplot layer's Controls subtab so the aes(x=/y=) ppVar
  # pickers' renderUI binds (project memory `shinytest2-appdir-pkgload`:
  # consumer pickers under a layer's tabset are suspended until the tab
  # is shown). Subtab values are the "Data" / "Controls" labels themselves.
  app$set_inputs(ggplot_subtab = "Controls", wait_ = FALSE)
  app$wait_for_idle(timeout = 25 * 1000)
  # ppVar root sentinel — aes(x = ppVar(mpg)).
  set_sentinel(app, "ggplot_1_1_ppVar_NA", "disp")
  # ppNum value sentinel — size = ppNum(2) in geom_point.
  set_sentinel(app, "geom_point_1_ppNum_NA", 0.7314159)
  # ppText sentinel — labs(title = ppText("Title")).
  set_sentinel(app, "labs_1_ppText_NA", "S_T_2718")
  # Shared "grp" — feeds aes(color = ...) AND facet_wrap(vars(...)).
  set_sentinel(app, "shared_grp", "gear")
  draw_and_wait(app, button_id = "ptr_update_plot")

  # ---- Final-mode propagation assertions ---------------------------------
  # ppNum: size = 0.7314159 inside geom_point(...)
  expect_sentinel_in_code(app, "ptr_code", "0.7314159",
    "geom_point\\([^)]*size\\s*=\\s*([^,)]*)", "final")
  # ppVar root: x = disp inside aes(...)
  expect_sentinel_in_code(app, "ptr_code", "disp",
    "aes\\([^)]*x\\s*=\\s*([^,)]*)", "final")
  # ppText: title = "S_T_2718" inside labs(...)
  expect_sentinel_in_code(app, "ptr_code", "\"S_T_2718\"",
    "labs\\([^)]*title\\s*=\\s*([^,)]*)", "final")
  # ppExpr: dplyr::mutate(adj = hp + 1 ...)
  expect_sentinel_in_code(app, "ptr_code", "hp + 1",
    "dplyr::mutate\\(adj\\s*=\\s*([^)]*)", "final")
  # Shared "grp" reaches BOTH aes(color=) AND facet_wrap(vars(...)).
  # ADR §App-1 pressure axis (ii): call-type-agnostic. Both required.
  expect_sentinel_in_code(app, "ptr_code", "gear",
    "aes\\([^)]*color\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "gear",
    "facet_wrap\\(vars\\(([^)]*)\\)", "final")
  # ppExpr feeding a downstream ppVar picker: aes(y = ppVar(adj))'s picker
  # offers "adj" as a column choice (the mutate-created column flows in).
  expect_picker_populated(app, "ggplot_1_2_ppVar_NA", "adj")
  # No plot-error class on the happy path in final mode.
  expect_no_plot_error(app)

  # ---- B3 toggle differential: final strips ppVar(, preserve retains ----
  expect_sentinel_nowhere(app, "ptr_code", "ppVar(")
  toggle_code_mode(app, "preserve")

  # Preserve-mode round-trip: value placeholders wrap their sentinel,
  # var placeholders render as a bare symbol (per render_placeholder_preserved).
  expect_sentinel_in_code(app, "ptr_code", "0.7314159",
    "ppNum\\(([^)]*)\\)", "preserve")
  expect_sentinel_in_code(app, "ptr_code", "disp",
    "aes\\([^)]*x\\s*=\\s*([^,)]*)", "preserve")
  # Anchor on labs(title = ppText(...)) — geom_smooth(method = ppText("lm"))
  # comes earlier in the rendered code, so a bare `ppText\(` regex would
  # match the wrong occurrence.
  expect_sentinel_in_code(app, "ptr_code", "\"S_T_2718\"",
    "labs\\([^)]*title\\s*=\\s*ppText\\(([^)]*)\\)", "preserve")
  # Preserve mode DOES contain "ppVar(" (B3 canonical-host instance).
  expect_sentinel_in_code(app, "ptr_code", "ppVar(",
    "(ppVar)\\(", "preserve")
  expect_no_plot_error(app)
})
# <<< super-1 end

# >>> super-2a begin
# (PLAN-03 inserts the super-app-2a upload-registry test_that() block here.)
# <<< super-2a end

# >>> super-2b begin
# (PLAN-04 inserts the super-app-2b customsource-splice test_that() block here.)
# <<< super-2b end

# >>> super-3 begin
# (PLAN-05 inserts the super-app-3 L3-multi-shared-plotly test_that() block here.)
# <<< super-3 end

# >>> super-4 begin
# (PLAN-06 inserts the super-app-4 user-css safety-adversarial test_that() block here.)
# <<< super-4 end
