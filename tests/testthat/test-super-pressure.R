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
  # Sentinel value `mpg + wt` (range ~12–44) is deliberately chosen to fit
  # inside `scale_y_continuous(limits = c(0, 50))`. A larger value like
  # `hp + 1` (range ~53–336) drops every row from geom_smooth → loess on
  # empty data → hard error from seq_len(). `expect_no_plot_error` below
  # only checks Shiny-output / ptr-alert CSS classes and would NOT catch
  # that degenerate state — so the recipe doubles as the human-followable
  # eyeball recipe in dev/notes/2026-05-24-super-eyeball-checklist.html.
  set_sentinel(app, "ggplot_3_1_ppExpr_NA", "mpg + wt")
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
  # ppExpr: dplyr::mutate(adj = mpg + wt ...)
  expect_sentinel_in_code(app, "ptr_code", "mpg + wt",
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

  # ---- B3 toggle differential: final strips ppVar( wrappers ---------------
  # ADR 0022: preserve-mode panel emission retired. The render-walker
  # preserve-mode shape is unit-tested directly in test-render-preserve.R;
  # final-mode propagation (substitution) is still asserted here.
  expect_sentinel_nowhere(app, "ptr_code", "ppVar(")
})
# <<< super-1 end

# >>> super-1 no-default begin
# ADR-0016 parallel coverage: same propagation pressure surface as the
# with-default test above, but the fixture's `app-no-default.R` strips every
# positional default from `pp*(...)` calls. The widget IDs are identical
# (the `_NA` suffix is independent of positional-default presence), so the
# sentinel drives map 1:1; the only initial-state divergence is `shared_lw`
# (no formula seed → widget defaults instead of `1`). Drive-and-propagate
# assertions are otherwise identical: this is the parallel test.
test_that("super-1 no-default: sentinels propagate through every placeholder (no positional defaults)", {
  app <- boot_super_app("super-1-kitchen-sink", app_file = "app-no-default.R")

  # ---- Drive every sentinel ----------------------------------------------
  set_sentinel(app, "ggplot_3_1_ppExpr_NA", "mpg + wt")
  app$set_inputs(ggplot_subtab = "Controls", wait_ = FALSE)
  app$wait_for_idle(timeout = 25 * 1000)
  set_sentinel(app, "ggplot_1_1_ppVar_NA", "disp")
  set_sentinel(app, "geom_point_1_ppNum_NA", 0.7314159)
  set_sentinel(app, "labs_1_ppText_NA", "S_T_2718")
  # Drive the shared "lw" widget explicitly — no formula default seeds it.
  set_sentinel(app, "shared_lw", 0.4242)
  set_sentinel(app, "shared_grp", "gear")
  draw_and_wait(app, button_id = "ptr_update_plot")

  # ---- Final-mode propagation assertions ---------------------------------
  expect_sentinel_in_code(app, "ptr_code", "0.7314159",
    "geom_point\\([^)]*size\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "disp",
    "aes\\([^)]*x\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "\"S_T_2718\"",
    "labs\\([^)]*title\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "mpg + wt",
    "dplyr::mutate\\(adj\\s*=\\s*([^)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "gear",
    "aes\\([^)]*color\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "gear",
    "facet_wrap\\(vars\\(([^)]*)\\)", "final")
  expect_sentinel_in_code(app, "ptr_code", "0.4242",
    "geom_smooth\\([^)]*linewidth\\s*=\\s*([^,)]*)", "final")
  expect_picker_populated(app, "ggplot_1_2_ppVar_NA", "adj")
  expect_no_plot_error(app)

  # ---- B3 toggle differential: final strips ppVar( wrappers ---------------
  # ADR 0022: preserve-mode panel emission retired. See test-render-preserve.R
  # for direct render-walker preserve-mode unit tests.
  expect_sentinel_nowhere(app, "ptr_code", "ppVar(")
})
# <<< super-1 no-default end

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
  set_sentinel(app, "ggplot_1_ppUpload_NA_shortcut", "df_main")
  set_sentinel(app, "geom_smooth_0_ppUpload_NA_shortcut", "df_aux")
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

  # ---- B3 toggle differential: final strips wrappers ----------------------
  # ADR 0022: preserve-mode panel emission retired; render-walker preserve
  # shape is unit-tested directly in test-render-preserve.R (including the
  # ppUpload bareword companion-name round-trip per ADR 0010).
  expect_sentinel_nowhere(app, "ptr_code", "ppPower(")
  expect_sentinel_nowhere(app, "ptr_code", "ppMultiVar(")
  expect_sentinel_nowhere(app, "ptr_code", "ppUpload(")

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

# >>> super-2a no-default begin
# ADR-0016 parallel coverage of the with-default test above. app-no-default.R
# strips every `pp*(default)` to `pp*()` (ppUpload bareword companion-names
# stay — they are structural source-binding identifiers, not defaults). The
# adversarial validate_input probe is not re-run (it tests ggpaintr's error
# pane, not formula structure).
test_that("super-2a no-default: sentinels propagate through multi-data-source + custom placeholders (no positional defaults)", {
  app <- boot_super_app("super-2a-upload-registry", app_file = "app-no-default.R")

  # ---- Provide column scope via actual CSV upload ------------------------
  # With no positional defaults, consumer placeholders (ppVar in aes(),
  # ppMultiVar, the shared "grp" widget) need upstream column scope to
  # bind their renderUI. In the with-default variant the positional
  # defaults (`ppVar(mpg)`, `ppMultiVar(cyl)`, etc.) seed column hints
  # into the parser so the pickers render with mtcars columns at boot;
  # the no-default formula has no such hints, so the pickers stay
  # un-rendered until a real upload resolves the data source. Upload the
  # sibling CSVs and assign their companion names BEFORE driving any
  # consumer picker.
  fixture_dir <- testthat::test_path("fixtures", "vignette-apps",
                                     "super-2a-upload-registry")
  app$upload_file(ggplot_1_ppUpload_NA = file.path(fixture_dir, "sample_main.csv"))
  app$upload_file(geom_smooth_0_ppUpload_NA = file.path(fixture_dir, "sample_aux.csv"))
  set_sentinel(app, "ggplot_1_ppUpload_NA_shortcut", "df_main")
  set_sentinel(app, "geom_smooth_0_ppUpload_NA_shortcut", "df_aux")
  app$wait_for_idle(timeout = 25 * 1000)

  # ---- Activate Controls subtab so root ppVar pickers bind ---------------
  app$set_inputs(ggplot_subtab = "Controls", wait_ = FALSE)
  app$wait_for_idle(timeout = 25 * 1000)

  # ---- Drive sentinels and root aes pickers ------------------------------
  set_sentinel(app, "ggplot_1_1_ppVar_NA", "mpg")
  set_sentinel(app, "ggplot_1_2_ppVar_NA", "hp")
  set_sentinel(app, "geom_point_3_ppPower_NA", 0.42)
  set_sentinel(app, "shared_grp", "cyl")
  set_sentinel(app, "geom_point_1_1_ppMultiVar_NA", c("cyl", "am"))
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app)

  # ---- Final-mode propagation assertions --------------------------------
  code_final <- app$get_value(output = "ptr_code")
  testthat::expect_true(
    is.character(code_final) && length(code_final) == 1L && nzchar(code_final),
    label = paste0("ptr_code is non-empty in final mode; actual=", code_final %||% "<NULL>")
  )
  # ppPower's non-identity resolve_expr emits `v^2`, so the deparsed final
  # code carries `alpha = 0.42^2` (proves the custom resolve_expr runs).
  testthat::expect_true(
    grepl("alpha = 0.42^2", code_final, fixed = TRUE),
    label = paste0("final-mode code contains literal 'alpha = 0.42^2'; actual code=", code_final)
  )
  # ppUpload bareword companion names propagate to final-mode `data =`
  # slots (these placeholders carry STRUCTURAL identifiers, not defaults,
  # so they propagate even without driving any widget).
  testthat::expect_true(
    grepl("data = df_main", code_final, fixed = TRUE),
    label = paste0("final-mode root data resolves to df_main; actual code=", code_final)
  )
  testthat::expect_true(
    grepl("data = df_aux", code_final, fixed = TRUE),
    label = paste0("final-mode geom_smooth data resolves to df_aux; actual code=", code_final)
  )
  # ppMultiVar's non-scalar return — `interaction(cyl, am)` — propagates to
  # the geom_point's aes(group=) slot in final mode.
  testthat::expect_true(
    grepl("interaction(cyl, am)", code_final, fixed = TRUE),
    label = paste0("final-mode code contains literal 'interaction(cyl, am)'; actual code=", code_final)
  )
  # Shared "grp" reaches BOTH the root aes(color=) and facet_wrap(vars(...)).
  expect_sentinel_in_code(app, "ptr_code", "cyl",
    "aes\\([^)]*color\\s*=\\s*([^,)]*)", "final")
  expect_sentinel_in_code(app, "ptr_code", "cyl",
    "facet_wrap\\(vars\\(([^)]*)\\)", "final")

  # ---- B3 toggle differential: final strips wrappers ---------------------
  # ADR 0022: preserve-mode panel emission retired; render-walker preserve
  # shape covered by test-render-preserve.R (including ADR 0010 ppUpload
  # bareword companion-name round-trip).
  expect_sentinel_nowhere(app, "ptr_code", "ppPower(")
  expect_sentinel_nowhere(app, "ptr_code", "ppMultiVar(")
  expect_sentinel_nowhere(app, "ptr_code", "ppUpload(")
})
# <<< super-2a no-default end

# >>> super-2b begin
test_that("super-2b customsource-splice: ppSample (D3 source) + !!splice (G3) + layer-upload + shared= on custom consumer all propagate, with layer-data pickers scoped to uploaded columns (ADR-0015 PLAN-01)", {
  # ADR 0013 §App-2b. Pressure axes:
  #   (i)   custom D3 source at formula head + layer-data ppUpload  (two
  #         distinct data-source mechanisms in one formula);
  #   (ii)  placeholders INSIDE a !! splice -- the AST walker MUST descend
  #         into the spliced body and discover ppText / ppNum / ppCoef
  #         under geom_smooth (FEATURE-CHECKLIST G3's canonical example
  #         splices a placeholder-free layer; this stresses the harder
  #         shape);
  #   (iii) shared= on a custom CONSUMER placeholder: ppFactor in
  #         aes(color=) and facet_wrap(vars(...)) collapse to one
  #         shared_fac widget;
  #   (iv)  G6 forwarded-symbol: smooth_template <- rlang::expr(...) is a
  #         local binding the splice resolves at capture time.
  #
  # The layer-data ppUpload picker-scope assertion (mpg yes, Sepal.Length
  # no) is the post-fix value-add specific to this plan: pre-ADR-0015
  # PLAN-01, the layer-aes ppVar under `data = ppUpload(...)` stayed in
  # `recalculating` indefinitely (the walker emitted the UI container but
  # ptr_runtime_input_spec() never produced a row for the picker). The fix
  # (eager-bind consumer pickers under source-headed upstream, merged at
  # 6ee63d7) makes the picker populate from the uploaded CSV's columns.
  # The single-layer binding case is already covered at module level by
  # test-adr15-consumer-binding.R; the unique pressure here is the
  # disjoint-column-set SCOPE DISCRIMINATION in a formula with two
  # distinct data sources (ppSample at head, ppUpload as layer-data).
  app <- boot_super_app("super-2b-customsource-splice")

  # --- Anchor widgets present at boot ------------------------------------
  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_code_mode")
  # ppSample selectInput default-selected option is "iris" (visible in the
  # bound DOM; the other choices ride on the selectized data attribute).
  expect_picker_populated(app, "ggplot_0_ppSample_NA", "iris")

  # --- Scenario: ppSample drives the formula-head data -------------------
  # Pick mtcars (distinct from default iris). After switching to the
  # Controls subtab so the suspended renderUI binds, the downstream root
  # ppVar pickers MUST populate from mtcars's column set -- proves the
  # custom D3 source resolved through asNamespace("datasets").
  set_sentinel(app, "ggplot_0_ppSample_NA", "mtcars")
  set_sentinel(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)
  set_sentinel(app, "ggplot_1_1_ppVar_NA", "mpg")
  set_sentinel(app, "ggplot_1_2_ppVar_NA", "hp")
  # ppFactor on color aes (shared "fac"); pick an mtcars column for now.
  set_sentinel(app, "shared_fac", "cyl")
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app, "ptr_plot")
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "mpg")
  # ADR 0022: preserve-mode panel emission retired. The render-walker
  # round-trip of ppSample("<pick>") is unit-tested in test-render-preserve.R.

  # --- Scenario: spliced ppText / ppNum / ppCoef sentinels propagate ----
  # If the AST walker fails to descend into the spliced segment, the
  # geom_smooth_{1_ppText,2_ppNum,3_ppCoef}_NA input ids never appear in
  # the page and the next set_sentinel() call fails with "Unable to find
  # input binding" -- that failure mode IS the auditor's evidence the
  # regression existed. Literally-unique sentinel values per placeholder.
  set_sentinel(app, "geom_smooth_1_ppText_NA", "loess")
  set_sentinel(app, "geom_smooth_2_ppNum_NA", 0.92)
  set_sentinel(app, "geom_smooth_3_ppCoef_NA", 0.81)
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app, "ptr_plot")

  # ADR 0022: preserve-mode panel emission retired. Render-walker preserve
  # shape for spliced ppText/ppNum/ppCoef wrappers is covered by direct
  # unit tests in test-render-preserve.R.
  expect_sentinel_in_code(app, "ptr_code", "\"loess\"",
    "geom_smooth\\([^)]*method\\s*=\\s*[^,)]*", "final")
  expect_sentinel_in_code(app, "ptr_code", "0.92",
    "geom_smooth\\([^)]*linewidth\\s*=\\s*[^,)]*", "final")
  expect_sentinel_in_code(app, "ptr_code", "0.81",
    "geom_smooth\\([^)]*alpha\\s*=\\s*[^,)]*", "final")

  # --- Scenario: shared= collapse on custom consumer (under iris) -------
  # Switch the root source back to iris (so Species is available), reset
  # the root pickers to iris columns, then set the shared_fac widget to
  # "Species". Final mode MUST show "Species" as a bare symbol in BOTH
  # aes(color = ...) AND facet_wrap(vars(...)). The single-widget collapse
  # is the call-type-agnostic contract for custom CONSUMER placeholders
  # (FEATURE-CHECKLIST E1/E6 cover this only for built-ins -- App-2b is
  # the first app-level assertion for custom-consumer shared=).
  set_sentinel(app, "ggplot_0_ppSample_NA", "iris")
  set_sentinel(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)
  set_sentinel(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_sentinel(app, "ggplot_1_2_ppVar_NA", "Sepal.Width")
  set_sentinel(app, "shared_fac", "Species")
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app, "ptr_plot")
  toggle_code_mode(app, "final")
  expect_sentinel_in_code(app, "ptr_code", "Species",
    "aes\\([^)]*color\\s*=\\s*[^,)]*", "final")
  expect_sentinel_in_code(app, "ptr_code", "Species",
    "facet_wrap\\(vars\\([^)]*\\)\\)", "final")

  # --- Scenario: Layer-data ppUpload + picker SCOPE discrimination ------
  # Upload sample_rug.csv to the df_rug fileInput. The layer-data
  # ppUpload's source-role resolution wires geom_rug's `data =` to df_rug.
  # The layer-aes ppVar pickers under `data = ppUpload(...)` are the
  # PLAN-04 unique value-add: pre-ADR-0015 these stayed in `recalculating`
  # forever; post-fix they populate from the CSV's columns (mpg, wt).
  # SCOPE NARROWING FORBIDDEN: the layer pickers MUST NOT offer iris
  # columns -- proving the layer-data ppUpload owns the layer-aes column
  # scope, NOT the root ppSample("iris"). If the picker shows iris columns,
  # the disjoint-data-source contract is broken; STOP and escalate.
  app$upload_file(
    geom_rug_0_ppUpload_NA = testthat::test_path(
      "fixtures", "vignette-apps",
      "super-2b-customsource-splice", "sample_rug.csv"
    )
  )
  # Companion name input -- set explicitly so the source observer's
  # binding_name resolution finds a deterministic value (the browser
  # autofills this from the filename via ptr_bind_source_autoname();
  # explicit set guards against ordering races under AppDriver).
  set_sentinel(app, "geom_rug_0_ppUpload_NA_shortcut", "df_rug")
  draw_and_wait(app, "ptr_update_plot")

  # Layer ppUpload's source-role resolution propagates to the rendered
  # geom_rug `data =` slot in final mode.
  toggle_code_mode(app, "final")
  expect_sentinel_in_code(app, "ptr_code", "df_rug",
    "geom_rug\\([^)]*data\\s*=\\s*[^,)]*", "final")
  # Picker-population (positive scope): layer-aes ppVar pickers under the
  # ppUpload-headed upstream are populated with the CSV's columns.
  expect_picker_populated(app, "geom_rug_1_1_ppVar_NA", "mpg")
  expect_picker_populated(app, "geom_rug_1_2_ppVar_NA", "wt")
  # Negative scope (PLAN-04 SC(layer-ppUpload)): the layer picker does
  # NOT offer iris columns. HTML-substring inverse per plan SC.
  rug_x_html <- app$get_html("#geom_rug_1_1_ppVar_NA") %||% ""
  testthat::expect_false(
    grepl("Sepal.Length", rug_x_html, fixed = TRUE),
    label = paste0(
      "scope discrimination: geom_rug aes(x) picker does NOT offer the ",
      "iris column 'Sepal.Length' (proving the layer-data ppUpload owns ",
      "the layer-aes column scope, not the root ppSample); ",
      "actual html=", substr(rug_x_html, 1, 600)
    )
  )
  expect_no_plot_error(app, "ptr_plot")

  # --- Scenario: B3 toggle differential ----------------------------------
  # Final mode: NONE of the seven placeholder wrapper call-forms appear.
  # ADR 0022: preserve-mode panel emission retired; the symmetrical
  # "every wrapper IS present in preserve" assertion lives in
  # test-render-preserve.R as a direct render-walker unit test.
  toggle_code_mode(app, "final")
  for (wrapper in c("ppSample(", "ppFactor(", "ppText(",
                    "ppCoef(", "ppNum(", "ppVar(", "ppUpload(")) {
    expect_sentinel_nowhere(app, "ptr_code", wrapper)
  }
  expect_no_plot_error(app, "ptr_plot")
})
# <<< super-2b end

# >>> super-2b no-default begin
# ADR-0016 parallel coverage. Strips positional defaults from every pp* call
# in the canonical formula and splice template; ppUpload's df_rug companion
# name stays. The bespoke layer-scope-discrimination probe (Sepal.Length not
# offered by geom_rug ppVar) is not re-run — it tests ADR-0015 scope
# resolution, which is formula-structure-agnostic.
test_that("super-2b no-default: ppSample + !! splice + layer-upload + shared= on custom consumer all propagate (no positional defaults)", {
  app <- boot_super_app("super-2b-customsource-splice", app_file = "app-no-default.R")

  # --- Anchor widgets present at boot ------------------------------------
  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_code_mode")
  # ppSample widget defaults to "iris" via build_ui's selected = "iris".
  expect_picker_populated(app, "ggplot_0_ppSample_NA", "iris")

  # --- Drive sentinels through the splice + shared layers ----------------
  set_sentinel(app, "ggplot_0_ppSample_NA", "iris")
  set_sentinel(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)
  set_sentinel(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_sentinel(app, "ggplot_1_2_ppVar_NA", "Sepal.Width")
  set_sentinel(app, "shared_fac", "Species")
  set_sentinel(app, "geom_smooth_1_ppText_NA", "loess")
  set_sentinel(app, "geom_smooth_2_ppNum_NA", 0.92)
  set_sentinel(app, "geom_smooth_3_ppCoef_NA", 0.81)
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app, "ptr_plot")

  # --- Final-mode propagation across spliced segment ---------------------
  toggle_code_mode(app, "final")
  expect_sentinel_in_code(app, "ptr_code", "\"loess\"",
    "geom_smooth\\([^)]*method\\s*=\\s*[^,)]*", "final")
  expect_sentinel_in_code(app, "ptr_code", "0.92",
    "geom_smooth\\([^)]*linewidth\\s*=\\s*[^,)]*", "final")
  expect_sentinel_in_code(app, "ptr_code", "0.81",
    "geom_smooth\\([^)]*alpha\\s*=\\s*[^,)]*", "final")
  # Shared "fac" reaches BOTH aes(color=) and facet_wrap(vars(...)).
  expect_sentinel_in_code(app, "ptr_code", "Species",
    "aes\\([^)]*color\\s*=\\s*[^,)]*", "final")
  expect_sentinel_in_code(app, "ptr_code", "Species",
    "facet_wrap\\(vars\\([^)]*\\)\\)", "final")

  # ADR 0022: preserve-mode panel emission retired. Render-walker preserve
  # shape for spliced ppText/ppNum/ppCoef wrappers is covered by direct
  # unit tests in test-render-preserve.R.
  expect_no_plot_error(app, "ptr_plot")
})
# <<< super-2b no-default end

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
  # ADR 0022: preserve-mode panel emission retired; the ppRange wrapper
  # round-trip is unit-tested in test-render-preserve.R.

  # ---- B3 toggle next to plotly output: final mode only --------------------
  # The toggle id ptr_ui_toggle_code wires for plot2 is `plot2-ptr_code_mode`
  # (per-module `ns("ptr_code_mode")`, see `code_mode_toggle` in
  # R/paintr-app.R). Final mode must not contain "ppVar("; the plotly host
  # must carry no error class. (Pre-ADR-0022 this block also drove the
  # radio to "preserve" and asserted "ppVar(" surfaced in plot2's code
  # text; that integration is now covered by direct render unit tests.)
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

# >>> super-3 no-default begin
# ADR-0016 parallel coverage. Strips positional defaults from every pp* in
# both cells' rlang::expr() formulas. The plotly-host non-error checks are
# re-run because they validate the L3 multi-output surface under any
# initial state.
test_that("super-3 no-default (L3 multi-cell + shared + plotly): shared key reaches both cells, plotly renders (no positional defaults)", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_super_app("super-3-l3-multi-shared-plotly", app_file = "app-no-default.R")

  # ---- Anchor every expected placeholder input id from both formulas ----
  plot1_ids <- c(
    "plot1-ggplot_1_1_ppVar_NA",
    "plot1-ggplot_1_2_ppVar_NA",
    "plot1-geom_point_1_ppNum_NA",
    "plot1-geom_point_2_ppNum_NA",
    "plot1-scale_x_continuous_1_ppRange_NA",
    "plot1-labs_1_ppText_NA",
    "plot1-ptr_update_plot",
    "plot1-ptr_code_mode"
  )
  plot2_ids <- c(
    "plot2-ggplot_1_1_ppVar_NA",
    "plot2-ggplot_1_2_ppVar_NA",
    "plot2-geom_point_1_ppNum_NA",
    "plot2-geom_smooth_1_ppText_NA",
    "plot2-geom_smooth_2_ppNum_NA",
    "plot2-labs_1_ppText_NA",
    "plot2-ptr_update_plot",
    "plot2-ptr_code_mode"
  )
  for (id in c(plot1_ids, plot2_ids, "shared_linked")) {
    expect_dom_id(app, id)
  }

  # ---- Drive sentinels in cell A ----------------------------------------
  set_sentinel(app, "plot1-ggplot_1_1_ppVar_NA", "mpg")
  set_sentinel(app, "plot1-ggplot_1_2_ppVar_NA", "wt")
  set_sentinel(app, "plot1-scale_x_continuous_1_ppRange_NA", c(12, 38))
  # Drive sentinels in cell B
  set_sentinel(app, "plot2-ggplot_1_1_ppVar_NA", "hp")
  set_sentinel(app, "plot2-ggplot_1_2_ppVar_NA", "qsec")
  set_sentinel(app, "plot2-geom_smooth_1_ppText_NA", "lm")
  # Shared "linked" reaches BOTH cells
  set_sentinel(app, "shared_linked", "gear")
  draw_and_wait(app, "plot1-ptr_update_plot")
  draw_and_wait(app, "plot2-ptr_update_plot")

  expect_rendered(app, "#plot2-custom_plot", "plotly")
  expect_rendered(app, "#plot1-ptr_plot", "ggplot")
  expect_no_plot_error(app, "plot1-ptr_plot")

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
  expect_sentinel_in_code(
    app, "plot1-ptr_code", "c(12, 38)",
    "scale_x_continuous\\(limits\\s*=\\s*[^)]*\\)", "final"
  )

  # ADR 0022: preserve-mode panel emission retired. The ppRange wrapper
  # round-trip is covered by direct render-walker unit tests in
  # test-render-preserve.R.
})
# <<< super-3 no-default end

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
  # Final mode strips every wrapper. ADR 0022: the symmetrical "preserve
  # mode renders each wrapper" half is covered by direct render-walker
  # unit tests in test-render-preserve.R. The runtime still has a not-ok
  # cached state from the adversarial draw; final mode reads from the
  # FROZEN snapshot the runtime locked at the last OK Update click (the
  # recover-with-#A1B2C3 draw), so the substituted code below derives
  # from that ok snapshot.
  toggle_code_mode(app, "final")
  expect_sentinel_nowhere(app, "ptr_code", "ppVar(")
  expect_sentinel_nowhere(app, "ptr_code", "ppNum(")
  expect_sentinel_nowhere(app, "ptr_code", "ppText(")
  expect_sentinel_nowhere(app, "ptr_code", "ppExpr(")
  expect_sentinel_nowhere(app, "ptr_code", "ppColor(")
  expect_no_plot_error(app)
})
# <<< super-4 end

# >>> super-4 no-default begin
# ADR-0016 parallel coverage. Strips positional defaults from every pp* in
# the paste0/sprintf-assembled formula. Notably exercises ppColor build_ui's
# NULL-`selected` arrival path: with no formula default, `selected` arrives
# NULL and build_ui's fallback ("#3366FF") seeds the textInput. The K4
# user_css pruning + denylist adversarial probes are NOT re-run — they test
# orthogonal surfaces already covered above.
test_that("super-4 no-default: ppColor NULL-arrival fallback, G5 string-builder, validate_input, sentinel propagation (no positional defaults)", {
  app <- boot_super_app("super-4-user-css-safety-adversarial",
                        app_file = "app-no-default.R")

  # ---- ppColor build_ui NULL-arrival: build_ui's fallback seeds the input
  # When the formula carries no positional default, `node$default` is NULL
  # at the build_ui injection point, so `selected` arrives NULL and the
  # build_ui chooses its fallback `"#3366FF"`. That fallback is what the
  # textInput reads as its initial value (per project memory
  # `shinytest2-appdir-pkgload`: get_value on the input id reads the
  # browser-side value, which the seed populates).
  initial_color <- app$get_value(input = "geom_smooth_2_ppColor_NA")
  testthat::expect_identical(
    initial_color, "#3366FF",
    label = paste0(
      "ppColor build_ui NULL-arrival fallback seeds the textInput with ",
      "'#3366FF' when the formula provides no positional default; ",
      "actual=", initial_color %||% "<NULL>"
    )
  )

  # ---- Initial happy-path draw at no-default state -----------------------
  draw_and_wait(app, "ptr_update_plot")
  expect_no_plot_error(app)

  # ---- G5 string-builder propagation: the paste0-assembled y = ppVar()
  # widget exists (the assembled formula text contained `y = ppVar()` which
  # the parser turned into a real placeholder).
  set_sentinel(app, "ggplot_1_2_ppVar_NA", "qsec")
  draw_and_wait(app, "ptr_update_plot")
  expect_sentinel_in_code(app, "ptr_code", "qsec",
    "aes\\([^)]*y\\s*=\\s*([^,)]*)", "final")
  code_after_first_ok_draw <- app$get_value(output = "ptr_code") %||% ""
  testthat::expect_true(nzchar(code_after_first_ok_draw),
    label = "code panel non-empty after first ok draw")

  # ---- validate_input rejection: inline error pane surfaces, no crash ----
  # NOTE: in the with-default test we also assert the prior code text
  # survives the validate_input fail (last_ok_runtime cache fallback). That
  # assertion is omitted here because the no-default's first draw is a
  # partial-aes state (root x = ppVar() unset → aes(y = qsec) only), and
  # the last_ok_runtime cache semantics under partial-aes states are not
  # part of the no-default formula's contract — they are a runtime-cache
  # implementation detail tested independently in the with-default block.
  set_sentinel(app, "geom_smooth_2_ppColor_NA", "notahex")
  draw_and_wait(app, "ptr_update_plot")
  err_html <- app$get_html("#ptr_error") %||% ""
  testthat::expect_true(
    grepl("must be #RRGGBB hex", err_html, fixed = TRUE),
    label = "inline error pane shows validate_input message"
  )
  expect_no_plot_error(app)

  # ---- Recover and propagate the recovered color -------------------------
  set_sentinel(app, "geom_smooth_2_ppColor_NA", "#A1B2C3")
  draw_and_wait(app, "ptr_update_plot")
  err_html_after_recover <- app$get_html("#ptr_error") %||% ""
  testthat::expect_false(
    grepl("must be #RRGGBB hex", err_html_after_recover, fixed = TRUE),
    label = "ptr_error clears after recovering ppColor to a valid hex"
  )
  expect_sentinel_in_code(app, "ptr_code", "\"#A1B2C3\"",
    "geom_smooth\\([^)]*color\\s*=\\s*([^,)]*)", "final")
})
# <<< super-4 no-default end
