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
test_that("super-4 user_css + safety + adversarial: user.css + core assets coexist, G5 string-builder propagates, ppColor styling reaches DOM, validate_input retains prior content, denylist rejects adversarial ppExpr, K4 prune leaves no dead user_css path", {
  app <- boot_super_app("super-4-user-css-safety-adversarial")

  # ---- K4 user_css pruning post-check ------------------------------------
  # PLAN-06 K4: after the block exits and AppDriver tears down, no
  # `ggpaintr-user-*` entry whose target dir is gone may remain in the
  # process-global resourcePaths(). The fixture's user.css lives at a
  # persistent path (tests/testthat/fixtures/...), so a leak would manifest
  # as a tempdir-target entry surviving. Defer in parent.frame so the
  # assertion fires AFTER boot_super_app()'s app$stop() deferred earlier.
  withr::defer({
    rp <- shiny::resourcePaths()
    dead_user <- vapply(
      names(rp),
      function(n) startsWith(n, "ggpaintr-user-") && !dir.exists(rp[[n]]),
      logical(1)
    )
    testthat::expect_false(
      any(dead_user),
      label = "no dead ggpaintr-user-* resource path remains after super-4 teardown"
    )
  }, envir = parent.frame())

  # ---- K2 + K3 coexistence -----------------------------------------------
  # PLAN-06 explicitly allows the user.css presence to be proved via "a
  # stylesheet link whose path begins with `ggpaintr-user-`" -- read all
  # <link rel="stylesheet"> tags in the page and assert both prefixes
  # coexist. The literal CSS *rule text* never reaches the head HTML
  # because Shiny serves user.css as a separate stylesheet URL rather
  # than inlining its contents; the link tag is the document-level
  # evidence the browser fetched user.css.
  # `link[rel='stylesheet']` returns a length-N character vector (one element
  # per link tag); collapse so substring checks are length-1.
  stylesheet_html <- paste(
    app$get_html("link[rel='stylesheet']") %||% "",
    collapse = "\n"
  )
  testthat::expect_true(
    grepl("ggpaintr-user-", stylesheet_html, fixed = TRUE) &&
      grepl("user.css", stylesheet_html, fixed = TRUE),
    label = paste0(
      "user.css linked via ggpaintr-user-*/user.css (proves user_css reached the DOM); ",
      "stylesheet_html=", substr(stylesheet_html, 1, 600)
    )
  )
  # Core ggpaintr htmlDependency assets reach the page. htmltools serves
  # the bundle at a *versioned* prefix (`ggpaintr-<ver>/ggpaintr.css`),
  # not the plain `addResourcePath("ggpaintr", ...)` path at
  # R/paintr-build-ui.R:591 -- the registered prefix is the directory map
  # that htmlDependency reads, not the served URL.
  testthat::expect_true(
    grepl("ggpaintr.css", stylesheet_html, fixed = TRUE),
    label = paste0(
      "core ggpaintr.css linked via htmlDependency prefix (proves core assets reached the DOM); ",
      "stylesheet_html=", substr(stylesheet_html, 1, 400)
    )
  )

  # ---- ppColor custom widget styling class reaches the DOM ---------------
  # The ppColor build_ui hook wraps textInput in
  # <div class="ptr-super4-colorpicker">. Read the wrapper (selected by
  # class) and verify the textInput id sits inside it -- this proves the
  # custom build_ui hook ran, the class landed on a real DOM element, and
  # the user.css rule for that class can take effect (the link-tag check
  # above completes the proof that the rule itself was fetched).
  wrap_html <- app$get_html(".ptr-super4-colorpicker") %||% ""
  testthat::expect_true(
    grepl("ptr-super4-colorpicker", wrap_html, fixed = TRUE) &&
      grepl("geom_smooth_2_ppColor_NA", wrap_html, fixed = TRUE),
    label = "ppColor widget container has class ptr-super4-colorpicker and wraps the textInput #geom_smooth_2_ppColor_NA"
  )

  # ---- Initial happy-path draw at defaults -------------------------------
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app)

  # ---- G5 string-builder propagation -------------------------------------
  # ADR §App-4 / PLAN-06 G5 row: paste0 + sprintf are in the closed
  # force-eval whitelist (R/paintr-app.R:260-262). The fixture's
  # `y_arg <- "ppVar(wt)"` got spliced literally into the formula text,
  # which the parser turned into a real ppVar widget. Set the y picker
  # to a literally-unique column and verify the rendered FINAL code
  # contains it inside aes(...). The widget existing + accepting a value
  # is the proof that the string fragment became a real placeholder.
  # super-4's ggplot layer has no data-pipeline verbs, so there is no
  # `ggplot_subtab` (consumer pickers bind immediately, no subtab gate).
  set_sentinel(app, "ggplot_1_2_ppVar_NA", "qsec")
  draw_and_wait(app, "ptr_update_plot")
  expect_sentinel_in_code(app, "ptr_code", "qsec",
    "aes\\([^)]*y\\s*=\\s*([^,)]*)", "final")
  # Snapshot the code panel for later validate_input-retain check.
  code_after_first_ok_draw <- app$get_value(output = "ptr_code") %||% ""
  testthat::expect_true(nzchar(code_after_first_ok_draw),
    label = "code panel non-empty after first ok draw")

  # ---- I5 validate_input rejection: inline error, no crash, prior code kept
  # Drive ppColor to a non-hex string. validate_input returns
  # "must be #RRGGBB hex" (a string, not TRUE) -> runtime res$ok = FALSE ->
  # ptr_register_plot's last_ok_runtime fallback paints the prior plot ->
  # ptr_register_code's fallback preserves the prior code text (PLAN-01).
  set_sentinel(app, "geom_smooth_2_ppColor_NA", "notahex")
  draw_and_wait(app, "ptr_update_plot")
  err_html <- app$get_html("#ptr_error") %||% ""
  testthat::expect_true(
    grepl("must be #RRGGBB hex", err_html, fixed = TRUE),
    label = "inline error pane #ptr_error surfaces validate_input message 'must be #RRGGBB hex'"
  )
  # Plot host retains the prior plot — no shiny-output-error / ptr-alert--error
  # class on #ptr_plot (the last_ok_runtime cache fed the prior plot back).
  expect_no_plot_error(app)
  # Code panel preserved verbatim from the prior successful draw (validate_input
  # failure does NOT clobber the code text).
  code_after_validate_fail <- app$get_value(output = "ptr_code") %||% ""
  testthat::expect_identical(
    code_after_validate_fail, code_after_first_ok_draw
  )

  # Recover: set ppColor to a valid hex, draw, confirm error pane clears and
  # the new sentinel reaches the rendered code. This proves the cache path
  # is transient (not sticky) and the runtime resumed cleanly.
  set_sentinel(app, "geom_smooth_2_ppColor_NA", "#A1B2C3")
  draw_and_wait(app, "ptr_update_plot")
  err_html_after_recover <- app$get_html("#ptr_error") %||% ""
  testthat::expect_false(
    grepl("must be #RRGGBB hex", err_html_after_recover, fixed = TRUE),
    label = "ptr_error clears after recovering ppColor to a valid hex"
  )
  expect_sentinel_in_code(app, "ptr_code", "\"#A1B2C3\"",
    "geom_smooth\\([^)]*color\\s*=\\s*([^,)]*)", "final")

  # ---- J1 + J3 adversarial ppExpr probe ----------------------------------
  # Set the ppExpr subtitle widget to a string containing the literal
  # `eval(parse(text = "1+1"))`. The denylist (R/paintr-utils.R:448) +
  # the recursive AST walker reject it; the canonical literal
  # `is not allowed in an \`expr\` input` (R/paintr-utils.R:609) surfaces
  # in the inline error pane, the adversarial text NEVER reaches the
  # rendered code text, and the host output retains the prior plot.
  set_sentinel(app, "labs_2_ppExpr_NA", "eval(parse(text = \"1+1\"))")
  draw_and_wait(app, "ptr_update_plot")
  err_html_adv <- app$get_html("#ptr_error") %||% ""
  testthat::expect_true(
    grepl("is not allowed in an `expr` input", err_html_adv, fixed = TRUE),
    label = "inline error pane surfaces canonical denylist literal 'is not allowed in an `expr` input'"
  )
  expect_sentinel_nowhere(app, "ptr_code", "eval(parse")
  expect_no_plot_error(app)

  # ---- B3 toggle differential --------------------------------------------
  # Final mode strips every wrapper; preserve mode renders each one at least
  # once. The wrappers checked: ppVar(, ppNum(, ppText(, ppExpr(, ppColor(.
  # The runtime still has a not-ok cached state from the adversarial draw;
  # toggle_code_mode uses the FROZEN snapshot the runtime locked at the last
  # OK Update click (the recover-with-#A1B2C3 draw), so both modes derive
  # from the same source of truth.
  toggle_code_mode(app, "final")
  expect_sentinel_nowhere(app, "ptr_code", "ppVar(")
  expect_sentinel_nowhere(app, "ptr_code", "ppNum(")
  expect_sentinel_nowhere(app, "ptr_code", "ppText(")
  expect_sentinel_nowhere(app, "ptr_code", "ppExpr(")
  expect_sentinel_nowhere(app, "ptr_code", "ppColor(")
  expect_no_plot_error(app)

  toggle_code_mode(app, "preserve")
  code_preserve <- app$get_value(output = "ptr_code") %||% ""
  for (wrapper in c("ppVar(", "ppNum(", "ppText(", "ppExpr(", "ppColor(")) {
    testthat::expect_true(
      grepl(wrapper, code_preserve, fixed = TRUE),
      label = paste0(
        "B3 preserve-mode code contains wrapper '", wrapper, "'; ",
        "actual code_text=", code_preserve
      )
    )
  }
  expect_no_plot_error(app)
})
# <<< super-4 end
