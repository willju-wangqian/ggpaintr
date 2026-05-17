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
#     module-app ............ L2 ptr_module_ui()/ptr_module_server()
#     single-instance-shared  L2 single-instance inline shared section
#     l2-shared ............. L2 coordinator trio ptr_shared()/_panel()/_server()
#                             (+ BDD: one shared panel widget drives both tiles)
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
#                   (empty shared_ui), normalize-cols, l2-shared-partition,
#                   l3-decompose (navbarPage/ptr_ui_assets escape hatch — UI
#                   fragment, "server unchanged"), l3-shared, l3-pieces-shared
#         -> represented by app-basic / app-grid-shared-added / l2-shared /
#            l3-pieces / l3-plotly.
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
  set_input(app, "ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_var_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
  code_before <- app$get_value(output = "ptr_code")
  expect_match(code_before, "Sepal.Length")

  # BDD: change a var input and re-draw -> outputs reflect the new input.
  set_input(app, "ggplot_1_1_var_NA", "Petal.Width")
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

  set_input(app, "shared_metric", "Sepal.Length")  # x of both plots
  draw(app, "ptr_shared_draw_all")
  expect_rendered(app, "#plot_1-ptr_plot", "ggplot")
  expect_rendered(app, "#plot_2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "plot_1-ptr_error")
  expect_no_inline_error(app, "plot_2-ptr_error")
})

test_that("use-cases module-app: L2 ptr_module_ui/ptr_module_server", {
  app <- boot_vignette_app("module-app")

  expect_dom_id(app, "p-ptr_update_plot")
  expect_dom_id(app, "p-ptr_plot")
  expect_dom_id(app, "p-ptr_code")

  set_input(app, "p-ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "p-ggplot_1_2_var_NA", "Petal.Length")
  draw(app, "p-ptr_update_plot")
  expect_rendered(app, "#p-ptr_plot", "ggplot")
  expect_code_nonempty(app, "p-ptr_code")
  expect_no_inline_error(app, "p-ptr_error")
})

test_that("use-cases single-instance-shared: inline shared section, no coordinator", {
  app <- boot_vignette_app("single-instance-shared")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")

  # One shared key drives both x and y of the single plot.
  set_input(app, "shared_col", "Sepal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})

test_that("use-cases l2-shared: coordinator trio drives both module tiles", {
  app <- boot_vignette_app("l2-shared")

  expect_dom_id(app, "shared_metric")
  expect_dom_id(app, "ptr_shared_draw_all")
  expect_dom_id(app, "plot_1-ptr_plot")
  expect_dom_id(app, "plot_2-ptr_plot")

  # BDD: change the single shared panel widget -> every tile re-renders.
  set_input(app, "shared_metric", "Sepal.Length")
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

  set_input(app, "ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_var_NA", "Petal.Length")
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

  set_input(app, "ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_var_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})

test_that("use-cases l3-plotly: L3 own-the-render-path with state$runtime()", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("l3-plotly")

  expect_dom_id(app, "plot1-ptr_update_plot")
  expect_dom_id(app, "plot1-custom_plot")

  set_input(app, "plot1-ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "plot1-ggplot_1_2_var_NA", "Petal.Length")
  draw(app, "plot1-ptr_update_plot")
  expect_rendered(app, "#plot1-custom_plot", "plotly")
})

test_that("use-cases l3-gg-extra: ptr_gg_extra() programmatic layer injection", {
  app <- boot_vignette_app("l3-gg-extra")

  expect_dom_id(app, "add_log")
  expect_dom_id(app, "p-ptr_update_plot")
  expect_dom_id(app, "p-ptr_plot")

  # Formula uses literal aes(mpg, hp) -- no placeholders to set.
  draw(app, "p-ptr_update_plot")
  expect_rendered(app, "#p-ptr_plot", "ggplot")
  expect_code_nonempty(app, "p-ptr_code")
  expect_no_inline_error(app, "p-ptr_error")

  # Inject an extra layer; the next runtime cycle folds it in (no error).
  app$click("add_log")
  app$wait_for_idle(timeout = 25 * 1000)
  draw(app, "p-ptr_update_plot")
  expect_rendered(app, "#p-ptr_plot", "ggplot")
  expect_no_inline_error(app, "p-ptr_error")
})

# --- ggpaintr-gallery.Rmd ----------------------------------------------------

test_that("gallery plotly-paintr (§5.1): module + custom plotly host output", {
  testthat::skip_if_not_installed("plotly")
  app <- boot_vignette_app("plotly-paintr")

  expect_dom_id(app, "plotly_demo-ptr_update_plot")
  expect_dom_id(app, "interactive_plot")

  set_input(app, "plotly_demo-ggplot_1_1_var_NA", "displ")  # mpg data
  set_input(app, "plotly_demo-ggplot_1_2_var_NA", "hwy")
  draw(app, "plotly_demo-ptr_update_plot")
  expect_rendered(app, "#plotly_demo-ptr_plot", "ggplot")  # bundled pane
  expect_rendered(app, "#interactive_plot", "plotly")      # custom host output
  expect_no_inline_error(app, "plotly_demo-ptr_error")
})

test_that("gallery ggiraph-paintr (§5.2): module + custom ggiraph host output", {
  testthat::skip_if_not_installed("ggiraph")
  testthat::skip_if_not_installed("colourpicker")
  app <- boot_vignette_app("ggiraph-paintr")

  expect_dom_id(app, "ggiraph_demo-ptr_update_plot")
  expect_dom_id(app, "interactive_plot")

  set_input(app, "ggiraph_demo-ggplot_1_1_var_NA", "displ")  # mpg data
  set_input(app, "ggiraph_demo-ggplot_1_2_var_NA", "hwy")
  draw(app, "ggiraph_demo-ptr_update_plot")
  expect_rendered(app, "#ggiraph_demo-ptr_plot", "ggplot")
  expect_rendered(app, "#interactive_plot", "ggiraph")
  expect_no_inline_error(app, "ggiraph_demo-ptr_error")
})

# --- ggpaintr-customization.Rmd ---------------------------------------------

test_that("customization ui-text-example: ptr_ui_text copy overrides", {
  app <- boot_vignette_app("ui-text-example")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ptr_plot")

  set_input(app, "ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_var_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})

test_that("customization bslib: page_sidebar wrapper (ptr_app_bslib)", {
  testthat::skip_if_not_installed("bslib")
  app <- boot_vignette_app("bslib")

  expect_dom_id(app, "ptr-ptr_update_plot")
  expect_dom_id(app, "ptr-ptr_plot")
  expect_dom_id(app, "ptr-ptr_code")

  set_input(app, "ptr-ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "ptr-ggplot_1_2_var_NA", "Petal.Length")
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
  set_input(app, "ggplot_1_1_var_NA", "mpg")
  set_input(app, "ggplot_1_2_var_NA", "hp")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})

test_that("customization source-dataset: custom source placeholder", {
  app <- boot_vignette_app("source-dataset")

  expect_dom_id(app, "ptr_update_plot")
  expect_dom_id(app, "ggplot_0_dataset_NA")  # the registered dataset selectInput

  # Choose the source dataset; switch to the Controls subtab so the
  # downstream var-picker renderUI binds (suspended under the Data subtab).
  set_input(app, "ggplot_0_dataset_NA", "iris")
  set_input(app, "ggplot_subtab", "Controls")
  app$wait_for_idle(timeout = 15 * 1000)
  set_input(app, "ggplot_1_1_var_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_var_NA", "Petal.Length")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_code_nonempty(app, "ptr_code")
  expect_no_inline_error(app, "ptr_error")
})
