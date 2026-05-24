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
test_that("super-2a upload+registry: sentinels propagate through multi-data-source + custom placeholders", {
  app <- boot_super_app("super-2a-upload-registry")

  fixture_dir <- testthat::test_path(
    "fixtures", "vignette-apps", "super-2a-upload-registry"
  )
  main_csv <- file.path(fixture_dir, "sample_main.csv")
  aux_csv  <- file.path(fixture_dir, "sample_aux.csv")

  # ---- Upload both data sources -------------------------------------------
  # ppUpload at the pipeline head: ggplot_1_ppUpload_NA (+ companion _name).
  # ppUpload in geom_smooth(data = ): geom_smooth_0_ppUpload_NA (+ _name).
  # Ids confirmed by probing ptr_translate + collect_layer_placeholders.
  app$upload_file(ggplot_1_ppUpload_NA = main_csv)
  app$upload_file(geom_smooth_0_ppUpload_NA = aux_csv)
  # Explicit companion-name sets guard against AppDriver-ordering races on
  # the auto-fill (project memory: adr12-bug-3a test does the same thing).
  set_sentinel(app, "ggplot_1_ppUpload_NA_name", "df_main")
  set_sentinel(app, "geom_smooth_0_ppUpload_NA_name", "df_aux")
  app$wait_for_idle(timeout = 25 * 1000)

  # ---- Initial draw at defaults (Scenario 1: both uploads boot) -----------
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app)

  # ---- Layer-data ppVar picker reflects df_aux's columns, not df_main's --
  # Pre-flight: pop the geom_smooth Controls subtab so its in-aes ppVar
  # picker renderUI binds (project memory `shinytest2-appdir-pkgload`).
  app$set_inputs(geom_smooth_subtab = "Controls", wait_ = FALSE)
  app$wait_for_idle(timeout = 25 * 1000)
  # Positive assertion: "mpg" IS offered (in both df_main and df_aux).
  expect_picker_populated(app, "geom_smooth_1_1_ppVar_NA", "mpg")
  # Negative assertion: "cyl" is NOT offered (in df_main but NOT in df_aux).
  # The picker MUST be populated from df_aux's columns (mpg, wt) only — the
  # ADR-0015 eager-bind fix at 6ee63d7 makes this work. If "cyl" appears,
  # that is regression territory (the picker fell back to df_main): STOP
  # and escalate, do not weaken the assertion.
  ppvar_geom_smooth_html <- app$get_html("#geom_smooth_1_1_ppVar_NA") %||% ""
  testthat::expect_false(
    grepl("cyl", ppvar_geom_smooth_html, fixed = TRUE),
    label = paste0(
      "negative assertion: geom_smooth's layer-data ppVar picker MUST NOT ",
      "offer 'cyl' as a choice (cyl is in df_main, NOT in df_aux); ",
      "if this fails, the multi-data-source resolution regressed ",
      "(ADR-0015 eager-bind, 6ee63d7) — STOP and escalate"
    )
  )

  # ---- ppPower custom-value sentinel: 0.42^2 lands in final-mode code ----
  set_sentinel(app, "geom_point_3_ppPower_NA", 0.42)
  # Shared "grp" sentinel (drive once now so it lands before final-mode
  # capture; df_main columns include cyl).
  app$set_inputs(ggplot_subtab = "Controls", wait_ = FALSE)
  app$wait_for_idle(timeout = 25 * 1000)
  set_sentinel(app, "shared_grp", "cyl")
  # ppMultiVar non-scalar sentinel: c("cyl", "am"). df_main has both.
  set_sentinel(app, "geom_point_1_1_ppMultiVar_NA", c("cyl", "am"))
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app)

  # ---- Final-mode propagation assertions ---------------------------------
  # F1-amended: substring (fixed=TRUE) instead of regex. The deparser emits
  # `geom_point(aes(group = interaction(cyl, am)), size = 2, alpha = 0.42^2)`
  # and the prior regex shape `geom_point\\([^)]*alpha\\s*=\\s*([^,)]*)`
  # cannot span the balanced parens of `interaction(cyl, am)` to reach
  # `alpha = 0.42^2`. Substring matching reflects the BDD `Then` clause
  # verbatim.
  code_final <- app$get_value(output = "ptr_code")
  testthat::expect_true(
    is.character(code_final) && length(code_final) == 1L && nzchar(code_final),
    label = paste0(
      "ptr_code is non-empty in final mode; actual=", code_final %||% "<NULL>"
    )
  )
  testthat::expect_true(
    grepl("alpha = 0.42^2", code_final, fixed = TRUE),
    label = paste0(
      "final-mode code contains literal 'alpha = 0.42^2' (ppPower ",
      "resolve_expr returns the call v^2, not a pre-evaluated number); ",
      "actual code=", code_final
    )
  )
  # F2-amended: substring (fixed=TRUE) for ppMultiVar's interaction() shape.
  # The capture-group regex would truncate at the first comma inside
  # `interaction(cyl, am)`.
  testthat::expect_true(
    grepl("interaction(cyl, am)", code_final, fixed = TRUE),
    label = paste0(
      "final-mode code contains literal 'interaction(cyl, am)' from ",
      "ppMultiVar's non-scalar return; actual code=", code_final
    )
  )
  # Shared "grp" reaches BOTH the root aes(color=) and facet_wrap(vars(...)).
  expect_sentinel_in_code(app, "ptr_code", "cyl",
    "aes\\([^)]*color\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "cyl",
    "facet_wrap\\(vars\\(([^)]*)\\)", "final")
  # The geom_smooth's separate aes() (which reads df_aux) does NOT acquire
  # `color = cyl` — shared "grp" stays in root-data scope.
  testthat::expect_false(
    grepl("geom_smooth\\([^)]*color\\s*=\\s*cyl", code_final, perl = TRUE),
    label = paste0(
      "shared 'grp' must stay in root-data scope: geom_smooth's own aes() ",
      "must NOT carry 'color = cyl'; actual code=", code_final
    )
  )

  # ---- B3 toggle differential: preserve retains wrappers, final strips --
  expect_sentinel_nowhere(app, "ptr_code", "ppPower(")
  expect_sentinel_nowhere(app, "ptr_code", "ppMultiVar(")
  expect_sentinel_nowhere(app, "ptr_code", "ppUpload(")

  toggle_code_mode(app, "preserve")
  code_preserve <- app$get_value(output = "ptr_code") %||% ""
  # Both ppUpload bareword companion-names round-trip (per ADR-0010).
  testthat::expect_true(
    grepl("ppUpload(df_main)", code_preserve, fixed = TRUE),
    label = paste0(
      "preserve-mode code contains literal 'ppUpload(df_main)' (bareword ",
      "companion-name); actual code=", code_preserve
    )
  )
  testthat::expect_true(
    grepl("ppUpload(df_aux)", code_preserve, fixed = TRUE),
    label = paste0(
      "preserve-mode code contains literal 'ppUpload(df_aux)' (bareword ",
      "companion-name); actual code=", code_preserve
    )
  )
  # ppPower preserves its 0.42 sentinel inside its wrapper.
  expect_sentinel_in_code(app, "ptr_code", "0.42",
    "ppPower\\(([^)]*)\\)", "preserve")
  # ppMultiVar's preserve-mode shape may be ppMultiVar(c('cyl', 'am')) or
  # ppMultiVar(c("cyl", "am")) depending on quote-folding; substring on
  # 'ppMultiVar(' + both column names covers either.
  testthat::expect_true(
    grepl("ppMultiVar(", code_preserve, fixed = TRUE) &&
      grepl("cyl", code_preserve, fixed = TRUE) &&
      grepl("am", code_preserve, fixed = TRUE),
    label = paste0(
      "preserve-mode code contains ppMultiVar( wrapper enclosing the ",
      "picker's selected columns; actual code=", code_preserve
    )
  )
  expect_no_plot_error(app)

  # ---- D9 validate_input on ppPower (Scenario: global error pane, no crash)
  # Flip back to final-mode for the validate_input flow (the error contract
  # is mode-agnostic, but final makes the post-error code-panel state
  # easier to inspect). F4 was DEFERRED: only assert (a) the error pane
  # populates and (b) the host has no plot-error class. The previously-drawn
  # plot remains visible.
  toggle_code_mode(app, "final")
  set_sentinel(app, "geom_point_3_ppPower_NA", 1.5)
  draw_and_wait(app, "ptr_update_plot")
  # F3-amended: validate_input errors surface in the global #ptr_error pane,
  # NOT a widget-adjacent inline error. Verified via
  # ptr_register_error / ptr_error_ui at R/paintr-server.R:2073-2102.
  # Use get_html (not get_value): ptr_error is a uiOutput that renders a
  # tagList, and get_value returns a list whose scalar coercion is brittle.
  err_html <- app$get_html("#ptr_error") %||% ""
  testthat::expect_true(
    grepl("must be in [0,1]", err_html, fixed = TRUE),
    label = paste0(
      "global #ptr_error pane shows the validate_input error 'must be in ",
      "[0,1]' after ppPower set to 1.5; actual ptr_error HTML=", err_html
    )
  )
  # (b) host retains a previous successful plot — no plot-error class added.
  expect_no_plot_error(app)
})
# <<< super-2a end

# >>> super-2b begin
# (PLAN-04 inserts the super-app-2b customsource-splice test_that() block here.)
# <<< super-2b end

# >>> super-3 begin
test_that("super-3 (L3 multi-cell + shared + plotly): shared key reaches both cells, plotly renders, B3 toggles, ppRange propagates", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_super_app("super-3-l3-multi-shared-plotly")

  # ---- G4 + G6 capture path -------------------------------------------------
  # Every expected placeholder input id from both formulas must be present
  # in the page. If `rlang::expr`-captured formulas had their placeholders
  # silently dropped, these would be missing -- the failure mode for the
  # capture-path regression. The shared key "linked" collapses to one
  # host-owned panel widget `shared_linked` (not namespaced).
  plot1_ids <- c(
    "plot1-ggplot_1_1_ppVar_NA",       # aes(x = ppVar(mpg))
    "plot1-ggplot_1_2_ppVar_NA",       # aes(y = ppVar(wt))
    "plot1-geom_point_1_ppNum_NA",     # geom_point(size = ppNum(2))
    "plot1-geom_point_2_ppNum_NA",     # geom_point(alpha = ppNum(0.7))
    "plot1-scale_x_continuous_1_ppRange_NA",
    "plot1-labs_1_ppText_NA",
    "plot1-ptr_update_plot",
    "plot1-ptr_code_mode"
  )
  plot2_ids <- c(
    "plot2-ggplot_1_1_ppVar_NA",       # aes(x = ppVar(hp))
    "plot2-ggplot_1_2_ppVar_NA",       # aes(y = ppVar(qsec))
    "plot2-geom_point_1_ppNum_NA",     # geom_point(size = ppNum(2))
    "plot2-geom_smooth_1_ppText_NA",   # geom_smooth(method = ppText("lm"))
    "plot2-geom_smooth_2_ppNum_NA",    # geom_smooth(linewidth = ppNum(1))
    "plot2-labs_1_ppText_NA",
    "plot2-ptr_update_plot",
    "plot2-ptr_code_mode"
  )
  for (id in c(plot1_ids, plot2_ids, "shared_linked")) {
    expect_dom_id(app, id)
  }

  # ---- Initial draws: plotly renders, no errors -----------------------------
  draw_and_wait(app, "plot1-ptr_update_plot")
  draw_and_wait(app, "plot2-ptr_update_plot")

  # Plotly host renders a plotly node (Scenario 1).
  expect_rendered(app, "#plot2-custom_plot", "plotly")

  # Plot1 (default ggpaintr-managed ggplot host) renders an <img> and carries
  # no error class on the happy path. The DOM id is `plot1-ptr_plot` because
  # `ptr_register_plot()` writes to `state$server_ns_fn("ptr_plot")` (not
  # `"plot"`); the fixture uses `ptr_ui_plot("plot1")` to wire that id.
  expect_rendered(app, "#plot1-ptr_plot", "ggplot")
  expect_no_plot_error(app, "plot1-ptr_plot")
  plotly_host_html <- app$get_html("#plot2-custom_plot") %||% ""
  testthat::expect_false(
    grepl("shiny-output-error", plotly_host_html, fixed = TRUE) ||
      grepl("ptr-alert--error", plotly_host_html, fixed = TRUE),
    label = "plot2 plotly host has no shiny-output-error / ptr-alert--error class"
  )

  # ---- Shared "linked" -> "gear" propagates to BOTH cells' code -------------
  # The cross-output-backend propagation contract (ADR 0013 §App-3).
  # SCOPE NARROWING FORBIDDEN: this assertion requires BOTH code outputs to
  # contain "color = gear". A single-cell hit means the shared coordinator
  # is broken across backends (project memory `project-shared-section-binder`).
  set_sentinel(app, "shared_linked", "gear")
  draw_and_wait(app, "plot1-ptr_update_plot")
  draw_and_wait(app, "plot2-ptr_update_plot")
  toggle_code_mode_ns <- function(ns_id, mode) {
    set_sentinel(app, paste0(ns_id, "-ptr_code_mode"), mode)
    app$wait_for_idle(timeout = 25 * 1000)
  }
  toggle_code_mode_ns("plot1", "final")
  toggle_code_mode_ns("plot2", "final")
  expect_sentinel_in_code(app, "plot1-ptr_code", "gear",
                          "aes\\([^)]*color\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "plot2-ptr_code", "gear",
                          "aes\\([^)]*color\\s*=\\s*([^,)]*)", "final")

  # ---- ppRange (custom value placeholder) sentinel propagates in plot1 ------
  # Unique pair c(12, 38). Final mode shows the resolved literal, preserve
  # mode shows the ppRange(...) wrapper round-trip.
  set_sentinel(app, "plot1-scale_x_continuous_1_ppRange_NA", c(12, 38))
  draw_and_wait(app, "plot1-ptr_update_plot")
  toggle_code_mode_ns("plot1", "final")
  # Final-mode position regex captures the inner `c(12, 38)` of
  # `scale_x_continuous(limits = c(12, 38))`. The character class
  # `[^)]*` greedily consumes characters up to (not including) the first
  # `)`, so the inclusive closing paren is added explicitly with `\\)`.
  expect_sentinel_in_code(
    app, "plot1-ptr_code", "c(12, 38)",
    "scale_x_continuous\\(limits\\s*=\\s*[^)]*\\)", "final"
  )
  toggle_code_mode_ns("plot1", "preserve")
  # Preserve-mode round-trips the wrapper -> `ppRange(c(12, 38))`. The
  # outer `ppRange(...)` regex spans the entire wrapper including the
  # inner `c(12, 38)`.
  expect_sentinel_in_code(
    app, "plot1-ptr_code", "c(12, 38)",
    "ppRange\\([^)]*\\)\\)", "preserve"
  )

  # ---- B3 toggle next to plotly output: preserve <-> final ------------------
  # The toggle id ptr_ui_toggle_code wires for plot2 is `plot2-ptr_code_mode`
  # (per-module `ns("ptr_code_mode")`, see `code_mode_toggle` in
  # R/paintr-app.R). Preserve mode must show "ppVar(" in plot2's code text;
  # final mode must not. The plotly host must carry no error class either way.
  toggle_code_mode_ns("plot2", "preserve")
  plot2_code_preserve <- app$get_value(output = "plot2-ptr_code") %||% ""
  testthat::expect_true(
    grepl("ppVar(", plot2_code_preserve, fixed = TRUE),
    label = paste0(
      "B3 toggle: plot2 preserve-mode code text contains 'ppVar('; ",
      "actual code_text=", plot2_code_preserve
    )
  )
  plotly_host_html_p <- app$get_html("#plot2-custom_plot") %||% ""
  testthat::expect_false(
    grepl("shiny-output-error", plotly_host_html_p, fixed = TRUE) ||
      grepl("ptr-alert--error", plotly_host_html_p, fixed = TRUE),
    label = "plot2 plotly host has no error class in preserve mode"
  )
  toggle_code_mode_ns("plot2", "final")
  plot2_code_final <- app$get_value(output = "plot2-ptr_code") %||% ""
  testthat::expect_false(
    grepl("ppVar(", plot2_code_final, fixed = TRUE),
    label = paste0(
      "B3 toggle: plot2 final-mode code text does NOT contain 'ppVar('; ",
      "actual code_text=", plot2_code_final
    )
  )
  plotly_host_html_f <- app$get_html("#plot2-custom_plot") %||% ""
  testthat::expect_false(
    grepl("shiny-output-error", plotly_host_html_f, fixed = TRUE) ||
      grepl("ptr-alert--error", plotly_host_html_f, fixed = TRUE),
    label = "plot2 plotly host has no error class in final mode"
  )
})
# <<< super-3 end

# >>> super-4 begin
# (PLAN-06 inserts the super-app-4 user-css safety-adversarial test_that() block here.)
# <<< super-4 end
