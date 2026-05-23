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
# (PLAN-02 inserts the super-app-1 kitchen-sink test_that() block here.)
# <<< super-1 end

# >>> super-2a begin
# (PLAN-03 inserts the super-app-2a upload-registry test_that() block here.)
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
