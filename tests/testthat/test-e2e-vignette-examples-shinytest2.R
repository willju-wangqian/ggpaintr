# =============================================================================
# E2E browser test of vignette examples  (plan: #E2E,
#   dev/plans/2026-05-17-e2e-browser-vignette-examples.html)
#
# Knitting proves a vignette chunk *evaluates*; it does not prove the generated
# Shiny app *works in a browser*. Each covered example is booted in real
# headless Chromium via shinytest2, then asserted: documented ids present, one
# real interaction, plot + generated-code outputs render, no inline error on
# the happy path. App fixtures: tests/testthat/fixtures/vignette-apps/<slug>/
# (verbatim-equivalent to the named chunk — factory <=> vignette diffable).
# Helpers: tests/testthat/helper-vignette-apps.R.
#
# Gating: every test starts skip_on_cran() + skip_if_not_installed("shinytest2")
# (+ chromote, + each example's extension package). With the browser stack the
# suite is fully green; without it every test skips cleanly — no spurious fail.
# Under R CMD check (CRAN) it all-skips; devtools::test() sets NOT_CRAN so it
# runs. NOTE: if chromote/Chrome do not resolve the whole suite all-skips —
# acceptable, but it must be reported, not hidden (see the plan status block).
#
# ---------------------------------------------------------------------------
# COVERAGE AUDIT — every vignette example, covered or excluded-with-reason.
# Nothing runnable is dropped silently.
# ---------------------------------------------------------------------------
#
# COVERED (one test_that each; fixture slug == test):
#   ggpaintr-use-cases.Rmd
#     app-basic ............. L1 ptr_app() canonical (+ the BDD interaction:
#                             set a var input, re-draw, code reflects it)
#     app-grid-shared-added . L1 ptr_app_grid() with a shared widget
#     module-app ............ L2 ptr_ui()/ptr_server()
#     single-instance-shared  L2 single-instance inline shared section
#     l2-shared ............. L2 coordinator trio ptr_shared()/_panel()/_server()
#                             (+ BDD: one shared panel widget drives both tiles)
#     l2-shared-partition ... L2 partition: formula-local ppVar(shared=) consumer
#                             keys (ax1/ax2) bound per-module + a panel value
#                             key (sz); asserts the inline pickers are POPULATED
#                             (W1 #B1: the binder-less embed path)
#     grid-shared-partition . L1 ptr_app_grid() same partition (formula-local
#                             axis keys + panel sz); same host shape as embed
#     l3-pieces ............. L3 bare pieces, hand-laid page (no combinators)
#     l3-pieces-toggle ...... L3 combinators ptr_ui_inline_error()/_toggle_code()
#     l3-plotly ............. L3 own-the-render-path: moduleServer + state$runtime()
#     l3-gg-extra ........... L3 ptr_gg_extra() programmatic layer injection
#   ggpaintr-gallery.Rmd
#     plotly-paintr ......... §5.1 worked L3 plotly custom-render example
#     ggiraph-paintr ........ §5.2 worked L3 ggiraph custom-render example
#   ggpaintr-customization.Rmd
#     ui-text-example ....... ptr_ui_text() copy overrides
#     bslib ................. the bslib page_sidebar wrapper (lifted from the
#                             eval=FALSE `ptr-app-bslib-source` chunk: runnable
#                             equivalent = the exported ptr_app_bslib() it shows)
#     value-range ........... custom *value* placeholder (value-range-app)
#     consumer-colvars ...... custom *consumer* placeholder (consumer-colvars-app)
#     source-dataset ........ custom *source* placeholder (source-dataset-app)
#   ADR 0009 features (no vignette pairing; browser-only contracts):
#     adr9-code-mode-toggle . ptr_code_mode radio drives ptr_register_code's
#                             final-vs-preserve branch. Regression net for the
#                             bug fixed in 2c504da (PLAN-08 added the UI radio
#                             but the server never read it). Differential
#                             assertion: code_final must NOT contain ppVar(;
#                             code_preserve MUST contain ppVar(; the two must
#                             not be identical; round-trip back to final.
#     adr9-default-seeding .. ppVar(<sym>) populates node$default (PLAN-06)
#                             which reaches the widget via invoke_build_ui's
#                             extra$selected gate (PLAN-07). Asserts the
#                             picker's initial Shiny input value matches the
#                             formula default with NO user interaction.
#     adr9-shared-default ... PLAN-07 shared_widget_default() first-occurrence-
#                             wins. Two ppVar(<sym>, shared='col') with
#                             different defaults; the shared widget seeds from
#                             the first occurrence and silently ignores the
#                             second. Asserts initial value of #shared_col.
#     adr9-named-args-custom  PLAN-03 named_args registry slot end-to-end:
#                             a custom hinted_text placeholder declares
#                             named_args = list(hint = ptr_default_string()),
#                             the formula passes hint = "...", and the value
#                             reaches build_ui via do.call (PLAN-07) and lands
#                             on textInput's placeholder= DOM attribute.
#
# EXCLUDED (with reason):
#   * Non-app chunks (no shiny.appobj): every `setup`, `libs`, `clipboard`
#     chunk; gallery `custom-placeholders-setup`; customization registration
#     chunks `value-range`/`consumer-colvars`/`consumer-numrange`/`source-dataset`
#     and `unregister` — placeholder registrations are folded into the COVERED
#     fixture that uses them, not booted alone.
#   * Display-only / not a ggpaintr app: all gallery `*-original` chunks (plain
#     ggplot2 graphics); customization `css-example` (eval=FALSE, references a
#     my-theme.css that does not ship); customization `ptr-app-dark` (eval=FALSE
#     one-line re-theme of ptr_app_bslib — wiring identical to COVERED `bslib`).
#   * Code fragments, not self-contained apps: use-cases `init-state` (a server
#     body, no ui/shinyApp), `l3-code-pane`, `l3-error-ui` (output$ snippets).
#   * Runnable but structurally redundant with a COVERED representative — same
#     boot/render/interact wiring, differing only in formula content and/or an
#     extension package (knit-time `eval=interactive()` + the parse/translate
#     unit suite already exercise formula-specific behavior; e2e adds no new
#     signal, and ~20 extra headless boots would break the runtime budget):
#       use-cases:  formula-tour, pipeline-formula-tour, app-grid-shared
#                   (empty shared_ui), normalize-cols,
#                   l3-decompose (navbarPage/ptr_ui_assets escape hatch — UI
#                   fragment, "server unchanged"), l3-shared, l3-pieces-shared
#         -> represented by app-basic / app-grid-shared-added / l2-shared /
#            l3-pieces / l3-plotly.
#     NOTE (W1 #B1/#B1b): `l2-shared-partition` was previously listed here as
#     "represented by l2-shared" — that was WRONG and is why bug B1 survived.
#     `l2-shared`'s `ppVar(shared='metric')` is referenced in BOTH formulas: a
#     PANEL (cross-formula) consumer key, host-bound by `ptr_shared_server()`.
#     `l2-shared-partition`'s `ppVar(shared='ax1')` is referenced in ONE
#     formula: a FORMULA-LOCAL consumer key, bound by `ptr_server()`
#     itself. These are DISTINCT ownership paths (ADR 0005 partition); a panel
#     key and a formula-local key must EACH have their own booting fixture.
#     `l2-shared-partition` (embed) and `grid-shared-partition` (grid, same
#     host shape) are therefore COVERED, not redundant.
#       gallery §3, §4.1–4.6, §6.1–6.5 (mpg-paintr, pipe-paintr, pca-paintr,
#         kmeans-paintr, regress-paintr, rolling-paintr, coefs-paintr,
#         pcp-paintr, ridges-paintr, repel-paintr, alluvial-paintr, dist-paintr)
#         -> all L1 ptr_app() formula recipes; represented by app-basic plus
#            the custom-placeholder COVERED tests (value-range / consumer-colvars
#            / source-dataset) which exercise the same recipe machinery.
#   * ggpaintr-safety.Rmd / ggpaintr-llm.Rmd — no interactive app to boot
#     (safety = prose + denylist; llm = ellmer wiring, not a Shiny app).
# =============================================================================

# --- ggpaintr-use-cases.Rmd --------------------------------------------------

test_that("use-cases app-basic: L1 ptr_app boots, renders, re-renders on input", {
  app <- boot_vignette_app("app-basic")

  # Documented ids present (the canonical top-level set).
  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_error")

  # Happy path: pick valid x/y columns, then draw.
  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  code_before <- app$get_value(output = "ptr_code")
  expect_match(code_before, "Sepal.Length")

  # BDD: change a var input and re-draw -> outputs reflect the new input.
  set_input(app, "ggplot_1_1_ppVar_NA", "Petal.Width")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_no_inline_error(app, "ptr_error")
  code_after <- app$get_value(output = "ptr_code")
  expect_false(identical(code_before, code_after))
  expect_match(code_after, "Petal.Width")
})

test_that("use-cases app-grid-shared-added: L1 ptr_app_grid with shared widget", {
  app <- boot_vignette_app("app-grid-shared-added")

  expect_dom_id(app, "shared_metric")        # the shared selectInput
  expect_dom_id(app, "ptr_shared_draw_all")  # the Draw all button (>=2 formulas)
  expect_dom_id(app, "plot_1-ptr_plot")
  expect_dom_id(app, "plot_2-ptr_plot")

  # "Petal.Width" is a literal in neither grid formula, so its appearance
  # in a cell's code proves the shared widget drove that cell (not a render
  # on defaults).
  set_input(app, "shared_metric", "Petal.Width")  # x of both plots
  draw(app, "ptr_shared_draw_all")
  expect_rendered(app, "#plot_1-ptr_plot", "ggplot")
  expect_rendered(app, "#plot_2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "plot_1-ptr_error")
  expect_no_inline_error(app, "plot_2-ptr_error")
  expect_match(app$get_value(output = "plot_1-ptr_code"), "Petal.Width",
               fixed = TRUE)
  expect_match(app$get_value(output = "plot_2-ptr_code"), "Petal.Width",
               fixed = TRUE)
})

test_that("use-cases module-app: L2 ptr_ui/ptr_server (id omitted)", {
  app <- boot_vignette_app("module-app")

  # The vignette omits `id` (single ggpaintr module). id = NULL ->
  # moduleServer(NULL) -> NS(NULL) identity -> ids are un-namespaced
  # (bare), same shape as ptr_app(); no `p-` prefix.
  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ptr_code")

  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})

test_that("use-cases single-instance-shared: inline shared section, no coordinator", {
  app <- boot_vignette_app("single-instance-shared")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")

  # y = var - ppVar(shared='col'): the shared key `col` drives x AND one y
  # operand; the OTHER y operand is a plain formula-local `var` the user
  # must pick (an unselected var legitimately yields no mapping), so the
  # happy path sets BOTH pickers. "Petal.Width" is a literal nowhere in
  # the formula, so its presence in the code can only come from the
  # inline shared widget propagating into both x and y.
  set_input(app, "ggplot_1_2_1_ppVar_NA", "Sepal.Length")  # unshared y operand
  set_input(app, "shared_col", "Petal.Width")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  # B1-class: the inline shared var picker must be POPULATED (not an empty
  # uiOutput), and the chosen column must reach the generated code.
  expect_picker_populated(app, "shared_col", "Petal.Width")
  expect_match(app$get_value(output = "ptr_code"), "Petal.Width",
               fixed = TRUE)
})

test_that("use-cases l2-shared: coordinator trio drives both module tiles", {
  app <- boot_vignette_app("l2-shared")

  expect_dom_id(app, "shared_metric")
  expect_dom_id(app, "ptr_shared_draw_all")
  expect_dom_id(app, "plot_1-ptr_plot")
  expect_dom_id(app, "plot_2-ptr_plot")

  # `metric` is a panel key (both formulas) -> owned by the standalone
  # ptr_shared_panel, excluded from each module's inline section via
  # shared = obj. Assert NO empty inline shared section renders in either
  # module (the presence-only gap that let this slip past the vignette
  # audit; mirrors l2-shared-partition's expect_no_dom_id for sz).
  expect_no_dom_id(app, "plot_1-shared_metric")
  expect_no_dom_id(app, "plot_2-shared_metric")

  # BDD: change the single shared panel widget -> every tile re-renders.
  # "Petal.Width" is a literal in NEITHER formula (y is Sepal.Length /
  # Sepal.Width), so its presence in a tile's code can only come from the
  # shared widget propagating — a true propagation proof, not "renders".
  set_input(app, "shared_metric", "Petal.Width")
  draw(app, "ptr_shared_draw_all")
  expect_rendered(app, "#plot_1-ptr_plot", "ggplot")
  expect_rendered(app, "#plot_2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "plot_1-ptr_error")
  expect_no_inline_error(app, "plot_2-ptr_error")
  expect_match(app$get_value(output = "plot_1-ptr_code"), "Petal.Width",
               fixed = TRUE)
  expect_match(app$get_value(output = "plot_2-ptr_code"), "Petal.Width",
               fixed = TRUE)
})

test_that("use-cases l2-shared-partition: formula-local ppVar(shared=) pickers are POPULATED in the embed path (W1 #B1/#B1b)", {
  app <- boot_vignette_app("l2-shared-partition")
  app$wait_for_idle(timeout = 25 * 1000)

  # Host shape: panel holds only the cross-formula value key `sz`; the
  # Draw-all button exists (>=2 formulas); both module plots present.
  expect_dom_id(app, "shared_sz")            # panel slider (panel_keys == "sz")
  expect_dom_id(app, "ptr_shared_draw_all")
  expect_dom_id(app, "plot_1-ptr_plot")
  expect_dom_id(app, "plot_2-ptr_plot")

  # CORE W1 ASSERTION (must fail the way bug B1 fails): each formula-local
  # ppVar(shared=) consumer key renders a POPULATED column picker inside its
  # owning module. Pre-fix these were blank uiOutputs (no host binder in the
  # embed path) — DOM-present but empty, so a bare expect_dom_id would NOT
  # have caught it. We assert a real iris column option is offered.
  expect_picker_populated(app, "plot_1-shared_ax1", "Petal.Length")
  expect_picker_populated(app, "plot_2-shared_ax2", "Petal.Length")

  # W2 (#B2) exclude: the panel key `sz` is rendered once, in the standalone
  # panel only — never duplicated inline (the fixture passes shared = obj).
  expect_no_dom_id(app, "plot_1-shared_sz")
  expect_no_dom_id(app, "plot_2-shared_sz")

  # BDD: selecting a column in the formula-local picker then redrawing makes
  # that plot use it (plot_1's x is ppVar(shared='ax1') and y is
  # var - ppVar(shared='ax1'), so ax1 drives x and the shared y term; the
  # other y operand is a plain formula-local var the user must also pick).
  set_input(app, "plot_1-ggplot_1_2_1_ppVar_NA", "Sepal.Length")  # unshared y operand
  set_input(app, "plot_1-shared_ax1", "Petal.Length")
  set_input(app, "plot_2-shared_ax2", "Sepal.Length")
  draw(app, "ptr_shared_draw_all")
  expect_rendered(app, "#plot_1-ptr_plot", "ggplot")
  expect_rendered(app, "#plot_2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "plot_1-ptr_error")
  expect_no_inline_error(app, "plot_2-ptr_error")
  expect_match(app$get_value(output = "plot_1-ptr_code"), "Petal.Length")
})

test_that("grid-shared-partition: ptr_app_grid binds formula-local ppVar(shared=) per cell (W1 #B1, same host shape)", {
  app <- boot_vignette_app("grid-shared-partition")
  app$wait_for_idle(timeout = 25 * 1000)

  expect_dom_id(app, "shared_sz")            # panel slider, the only panel key
  expect_dom_id(app, "ptr_shared_draw_all")
  expect_dom_id(app, "plot_1-ptr_plot")
  expect_dom_id(app, "plot_2-ptr_plot")

  # Same core assertion on the grid host shape: each cell's formula-local
  # axis picker is populated, not a blank uiOutput.
  expect_picker_populated(app, "plot_1-shared_ax1", "Petal.Length")
  expect_picker_populated(app, "plot_2-shared_ax2", "Petal.Length")

  # issues/01 fix: ptr_app_grid_components() now forwards shared = obj to
  # each cell's ptr_ui(), so the cross-formula panel key `sz` is
  # excluded from the inline section and lives ONLY in the standalone
  # panel above -- not double-rendered per cell.
  expect_no_dom_id(app, "plot_1-shared_sz")
  expect_no_dom_id(app, "plot_2-shared_sz")

  set_input(app, "plot_1-shared_ax1", "Petal.Length")
  set_input(app, "plot_2-shared_ax2", "Sepal.Length")
  draw(app, "ptr_shared_draw_all")
  expect_rendered(app, "#plot_1-ptr_plot", "ggplot")
  expect_rendered(app, "#plot_2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "plot_1-ptr_error")
  expect_no_inline_error(app, "plot_2-ptr_error")
})

test_that("use-cases l3-pieces: L3 bare pieces, hand-laid page", {
  app <- boot_vignette_app("l3-pieces")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_error")

  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})

test_that("use-cases l3-pieces-toggle: L3 combinators (inline error + toggle code)", {
  app <- boot_vignette_app("l3-pieces-toggle")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ptr_code")

  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  # The test name promises the toggle-code combinator; assert its actual
  # behavior, not just that #ptr_code exists. ptr_ui_toggle_code() wraps the
  # code in a .ptr-code-window that starts closed; clicking the injected
  # .ptr-code-toggle button flips the `ptr-open` class (ggpaintr-ui.js).
  expect_no_match(app$get_html(".ptr-code-window") %||% "", "ptr-open",
                  fixed = TRUE)
  app$click(selector = ".ptr-code-toggle")
  app$wait_for_idle(timeout = 5 * 1000)
  expect_match(app$get_html(".ptr-code-window"), "ptr-open", fixed = TRUE)
})

test_that("use-cases l3-plotly: L3 own-the-render-path with state$runtime()", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("l3-plotly")

  expect_dom_id(app, "plot1-ptr_update_plot")
  expect_dom_id(app, "plot1-custom_plot")

  set_input(app, "plot1-ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "plot1-ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "plot1-ptr_update_plot")
  expect_rendered(app, "#plot1-custom_plot", "plotly")
  # Bug-1 + combinator lock: ptr_ui_code("plot1") under moduleServer("plot1")
  # binds plot1-ptr_code (un-namespaced ptr_ui_code() left it empty), and the
  # </> toggle composes with the raw plotly htmlwidget (plotlyOutput is a
  # tagList, so the injected .ptr-code-toggle is a .ptr-output sibling, not
  # wiped on render). Window starts closed; clicking the toggle opens it;
  # the code pane then shows the run's generated code.
  expect_no_match(app$get_html(".ptr-code-window") %||% "", "ptr-open",
                  fixed = TRUE)
  app$click(selector = ".ptr-code-toggle")
  app$wait_for_idle(timeout = 5 * 1000)
  expect_match(app$get_html(".ptr-code-window"), "ptr-open", fixed = TRUE)
  expect_match(app$get_value(output = "plot1-ptr_code"), "Sepal.Length",
               fixed = TRUE)
})

test_that("use-cases l3-gg-extra: ptr_gg_extra() programmatic layer injection", {
  app <- boot_vignette_app("l3-gg-extra")

  expect_dom_id(app, "add_log")
  expect_dom_id(app, "p-ptr_update_plot")
  expect_dom_id(app, "p-ptr_plot")

  # Formula uses literal aes(mpg, hp) -- no placeholders to set.
  draw(app, "p-ptr_update_plot")
  expect_host_settled(app, "p-ptr_plot", "ggplot", "p-ptr_error")
  expect_code_nonempty(app, "p-ptr_code")
  img_before <- app$get_html("#p-ptr_plot")

  # Inject an extra layer; the next runtime cycle folds it in (no error).
  # The wait_for_idle here is a distinct concern -- it lets the
  # observeEvent(input$add_log) -> ptr_gg_extra() register before the
  # redraw; it is NOT the error-sampling settle (that is expect_host_settled).
  app$click("add_log")
  app$wait_for_idle(timeout = 25 * 1000)
  draw(app, "p-ptr_update_plot")
  expect_host_settled(app, "p-ptr_plot", "ggplot", "p-ptr_error")
  # B1-class: prove ptr_gg_extra() actually injected the layer, not that the
  # plot merely still renders. scale_x_log10() changes the x axis, so the
  # rendered <img> base64 must differ from the pre-injection render.
  img_after <- app$get_html("#p-ptr_plot")
  expect_false(identical(img_before, img_after))
})

# --- ggpaintr-gallery.Rmd ----------------------------------------------------

test_that("gallery plotly-paintr (§5.1): module + custom plotly host output", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("plotly-paintr")

  expect_dom_id(app, "plotly_demo-ptr_update_plot")
  expect_dom_id(app, "interactive_plot")

  set_input(app, "plotly_demo-ggplot_1_1_ppVar_NA", "displ")  # mpg data
  set_input(app, "plotly_demo-ggplot_1_2_ppVar_NA", "hwy")
  draw(app, "plotly_demo-ptr_update_plot")
  expect_rendered(app, "#plotly_demo-ptr_plot", "ggplot")  # bundled pane
  # issues/02 durable fix: poll for the terminal success state (custom host
  # rendered AND error pane cleared, simultaneously) before asserting --
  # deterministic, not a heuristic-timeout race. See expect_host_settled().
  expect_host_settled(app, "interactive_plot", "plotly",
                      "plotly_demo-ptr_error")
})

test_that("gallery ggiraph-paintr (§5.2): module + custom ggiraph host output", {
  testthat::skip_if_not_installed("ggiraph")
  testthat::skip_if_not_installed("colourpicker")
  app <- boot_vignette_app("ggiraph-paintr")

  expect_dom_id(app, "ggiraph_demo-ptr_update_plot")
  expect_dom_id(app, "interactive_plot")

  set_input(app, "ggiraph_demo-ggplot_1_1_ppVar_NA", "displ")  # mpg data
  set_input(app, "ggiraph_demo-ggplot_1_2_ppVar_NA", "hwy")
  draw(app, "ggiraph_demo-ptr_update_plot")
  expect_rendered(app, "#ggiraph_demo-ptr_plot", "ggplot")
  # issues/02 durable fix (same as §5.1, by parity).
  expect_host_settled(app, "interactive_plot", "ggiraph",
                      "ggiraph_demo-ptr_error")
})

# --- ggpaintr-customization.Rmd ---------------------------------------------

test_that("customization ui-text-example: ptr_ui_text copy overrides", {
  app <- boot_vignette_app("ui-text-example")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")

  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  # The test name promises copy overrides; assert the overridden strings
  # actually rendered (stock defaults are NOT "Render"/"Iris explorer"), not
  # merely that the app booted with some copy.
  expect_match(app$get_html("#ptr_update_plot"), "Render", fixed = TRUE)
  expect_match(app$get_html("body"), "Iris explorer", fixed = TRUE)
})

test_that("customization bslib: page_sidebar wrapper (ptr_app_bslib)", {
  testthat::skip_if_not_installed("bslib")
  app <- boot_vignette_app("bslib")

  expect_dom_id(app, "ptr-ptr_update_plot")
  expect_dom_id(app, "ptr-ptr_plot")
  expect_dom_id(app, "ptr-ptr_code")

  set_input(app, "ptr-ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ptr-ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr-ptr_update_plot")
  expect_rendered(app, "#ptr-ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr-ptr_code")
  expect_no_inline_error(app, "ptr-ptr_error")
})

test_that("customization value-range: custom value placeholder", {
  app <- boot_vignette_app("value-range")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "xlim_1_range_NA")  # the registered range sliderInput

  # aes(mpg, hp) is literal; the registered range slider drives xlim().
  set_input(app, "xlim_1_range_NA", c(10, 40))
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  # B1-class: prove the custom value placeholder's resolve_expr actually
  # reached the generated code (the non-default slider pair 10/40, distinct
  # from the c(0,100) default), not merely that *some* code exists.
  # shinytest2 delivers the slider value as integers, so resolve_expr's
  # c(!!10L, !!40L) deparses to c(10L, 40L) — assert the verified-actual
  # codegen (the non-default pair, distinct from the c(0,100) default).
  code <- app$get_value(output = "ptr_code")
  expect_match(code, "xlim(c(10L, 40L))", fixed = TRUE)
})

test_that("customization consumer-colvars: custom consumer placeholder", {
  testthat::skip_if_not_installed("dplyr")
  app <- boot_vignette_app("consumer-colvars")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")
  expect_dom_id(app, "ggplot_2_1_colvars_NA")  # the registered colvars selectInput

  # Pick upstream columns; switch to the Controls subtab so the downstream
  # var-picker renderUI binds (it is suspended while the Data subtab is shown).
  set_input(app, "ggplot_2_1_colvars_NA", c("mpg", "hp", "wt"))
  set_input(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)
  set_input(app, "ggplot_1_1_ppVar_NA", "mpg")
  set_input(app, "ggplot_1_2_ppVar_NA", "hp")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  # B1-class: the consumer picker must be POPULATED with upstream columns
  # (an empty selectInput would pass a bare expect_dom_id), and the selected
  # columns must reach the generated dplyr::select() via resolve_expr.
  expect_picker_populated(app, "ggplot_2_1_colvars_NA", "mpg")
  expect_match(app$get_value(output = "ptr_code"), '"mpg".*"hp".*"wt"')
})

test_that("customization source-dataset: custom source placeholder", {
  app <- boot_vignette_app("source-dataset")

  expect_dom_id(app, "ptr_update_plot")
  # The source selectInput has static choices, so it is populated at boot.
  expect_picker_populated(app, "ggplot_0_dataset_NA", "iris")

  # Choose the source dataset; switch to the Controls subtab so the
  # downstream var-picker renderUI binds (suspended under the Data subtab).
  set_input(app, "ggplot_0_dataset_NA", "iris")
  set_input(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)
  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  # B1-class: prove the custom SOURCE actually resolved — the downstream var
  # picker is populated from the resolved iris columns (empty if resolve_data
  # never ran), and the selection reaches the generated code.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  expect_match(app$get_value(output = "ptr_code"), "Sepal.Length", fixed = TRUE)
})

# --- ADR 0009 features ------------------------------------------------------
# These four fixtures are NOT vignette-paired — they cover ADR-0009 behaviours
# that have no vignette chunk and that can ONLY be observed end-to-end in a
# real browser (DOM widget initial state, radio-driven render branches,
# named_args → rendered attribute). Each fixture lives at
# tests/testthat/fixtures/vignette-apps/adr9-*/app.R and explains its purpose
# in its own boot-block comment.

test_that("adr9-code-mode-toggle: ptr_code_mode radio switches code panel between final and preserve", {
  app <- boot_vignette_app("adr9-code-mode-toggle")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_code")
  expect_dom_id(app, "ptr_code_mode")

  # Drive a first render so the code panel has substituted text to compare
  # against. ggpaintr only redraws on Update click, so the inputs must be
  # set BEFORE the click.
  set_input(app, "ggplot_1_1_ppVar_NA", "mpg")
  set_input(app, "ggplot_1_2_ppVar_NA", "cyl")
  draw(app, "ptr_update_plot")
  expect_code_nonempty(app, "ptr_code")

  # FINAL mode (default) — substituted text. The placeholder call form
  # must NOT appear; the chosen values MUST appear.
  code_final <- app$get_value(output = "ptr_code")
  expect_false(grepl("ppVar(", code_final, fixed = TRUE),
               label = "final-mode code text does not contain ppVar(")
  expect_match(code_final, "mpg", fixed = TRUE)
  expect_match(code_final, "cyl", fixed = TRUE)

  # Flip to PRESERVE mode. radioGroupButtons writes a new value to
  # input$ptr_code_mode; ptr_register_code re-renders. wait_=TRUE because
  # this is one of the few inputs that DOES trigger an output update by
  # itself (no Update click required).
  app$set_inputs(ptr_code_mode = "preserve", wait_ = TRUE, timeout_ = 10000)
  code_preserve <- app$get_value(output = "ptr_code")

  # Preserve-mode text MUST contain the placeholder call form and MUST NOT
  # match the final-mode text. This is the regression-net: if the
  # ptr_register_code wiring breaks (pre-2c504da behaviour), the radio is
  # inert and code_preserve == code_final → both expectations fail.
  expect_true(grepl("ppVar(", code_preserve, fixed = TRUE),
              label = "preserve-mode code text contains ppVar(")
  expect_false(identical(code_final, code_preserve),
               label = "final and preserve modes produce different code text")

  # And back to FINAL — the toggle is bidirectional.
  app$set_inputs(ptr_code_mode = "final", wait_ = TRUE, timeout_ = 10000)
  expect_identical(app$get_value(output = "ptr_code"), code_final)
})

test_that("adr9-default-seeding: ppVar(default = <sym>) seeds the picker initial value", {
  app <- boot_vignette_app("adr9-default-seeding")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ggplot_1_1_ppVar_NA")
  expect_dom_id(app, "ggplot_1_2_ppVar_NA")

  # No user interaction yet — assert the widgets initialised to the formula
  # defaults via PLAN-07's invoke_build_ui seeding path. A pre-PLAN-07
  # ggpaintr would render these pickers empty even though node$default was
  # populated by PLAN-06's parser.
  expect_equal(app$get_value(input = "ggplot_1_1_ppVar_NA"), "mpg",
               label = "ppVar(default = mpg) seeds x picker")
  expect_equal(app$get_value(input = "ggplot_1_2_ppVar_NA"), "cyl",
               label = "ppVar(default = cyl) seeds y picker")

  # And the seeded defaults reach the rendered code on a first draw with
  # no further input — proves the default is a real value, not just a
  # display-only ghost.
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_no_inline_error(app, "ptr_error")
  code <- app$get_value(output = "ptr_code")
  expect_match(code, "mpg", fixed = TRUE)
  expect_match(code, "cyl", fixed = TRUE)
})

test_that("adr9-shared-default: shared widget seeds from FIRST occurrence's default (PLAN-07)", {
  app <- boot_vignette_app("adr9-shared-default")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "shared_col")

  # First-occurrence-wins: geom_point's ppVar(shared='col', default=hp) is
  # the first 'col' occurrence in formula order; geom_smooth's
  # ppVar(shared='col', default=wt) is the second and is silently ignored
  # for seeding (matches ADR 0009 §8 "first wins silently; no
  # translate-time abort"). The widget must initialise to "hp", NOT "wt".
  expect_equal(app$get_value(input = "shared_col"), "hp",
               label = "shared widget seeds from first occurrence's default")

  # Functional check: the seeded shared value reaches BOTH layers' code.
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_no_inline_error(app, "ptr_error")
  code <- app$get_value(output = "ptr_code")
  expect_match(code, "hp", fixed = TRUE)
})

test_that("adr9-named-args-custom: declared named_args reach the build_ui hook as a named arg", {
  app <- boot_vignette_app("adr9-named-args-custom")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")

  # The custom 'hinted_text' build_ui passes named_args$hint through to
  # shiny::textInput's `placeholder=` attribute. If PLAN-06's parser fails
  # to populate node$named_args, or PLAN-07's invoke_build_ui fails to
  # thread it via do.call, the placeholder attribute is empty and this
  # assertion fails. We search the full page HTML (defensive against the
  # exact textInput id) for the rendered attribute.
  html <- app$get_html("body")
  expect_match(
    html %||% "",
    'placeholder="Type your plot title"',
    fixed = TRUE,
    info = "build_ui received named_args$hint and rendered it as textInput placeholder"
  )

  # And the placeholder still produces working code on draw.
  set_input(app, "ggplot_1_1_ppVar_NA", "mpg")
  set_input(app, "ggplot_1_2_ppVar_NA", "cyl")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_no_inline_error(app, "ptr_error")
})
