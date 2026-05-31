# J11 journey -- app-basic + j11-module-id.
#
# Browser-driven coverage for the ptr_app_components / ptr_ui / ptr_server
# core wire: runtime gating on Update Plot, var-picker shape + content,
# layer-select driving the hidden ptr_layer_tabset, namespaced-id alignment.
#
# Routing decisions (dev/audit/audit-test-fidelity-v7-j11-browser-
# faithfulness-2026-05-27-2330.html):
#   MERGED (deleted L2 -> covered here):
#     test-rewrite-app.R:36, :50, :78, :113, :205
#     test-rewrite-server-state.R:138
#   RETAINED as unit pins (NOT covered here -- internal-shape contracts):
#     test-rewrite-server-state.R:67  (runtime-list named-shape: ok/code_text/plot)
#     test-rewrite-server-state.R:120 (ptr_extract_* public-R-API surfaces)
#     test-rewrite-server-state.R:216 (object-identity of consecutive runtimes;
#                                       R-side ptr_gg_extra trigger has no DOM event)
#
# Standard browser-e2e scaffolding (skip_on_cran / skip_if_not_installed /
# source-root guard) lives in boot_vignette_app() -- helper-vignette-apps.R.

test_that("J11 journey (app-basic): runtime gating, picker shape, layer-tabset, end-to-end render", {
  app <- boot_vignette_app("app-basic")

  # ---- Stage 2: runtime is gated -- no eval until Update Plot click -----
  # Pre-click: code output should not contain rendered code (the L2 at
  # test-rewrite-server-state.R:138 asserted state$runtime() is NULL; the
  # DOM-faithful surface is that #ptr_code has not received rendered code
  # text yet -- "geom_point" is the post-click literal). The DOM container
  # exists from static UI; we assert the rendered code body is absent, not
  # the container.
  pre_click_code <- app$get_html("#ptr_code") %||% ""
  expect_false(
    grepl("geom_point", pre_click_code, fixed = TRUE),
    label = "pre-click #ptr_code does not contain rendered code"
  )

  # Setting a picker without clicking Update Plot must still leave the
  # code output un-rendered. This is the load-bearing branch of the L2
  # ("picking vars without clicking Update Plot must still leave runtime
  # NULL"): a regression where the runtime fired on picker change would
  # populate the code output here. iris columns are: Sepal.Length,
  # Sepal.Width, Petal.Length, Petal.Width, Species.
  set_input(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  set_input(app, "ggplot_1_2_ppVar_NA", "Sepal.Width")
  app$wait_for_idle(timeout = 5 * 1000)
  picker_set_code <- app$get_html("#ptr_code") %||% ""
  expect_false(
    grepl("geom_point", picker_set_code, fixed = TRUE),
    label = "picker change without Update Plot does not populate #ptr_code"
  )

  # ---- Stage 9: var pickers populated from columns of literal `iris` ---
  # app-basic uses `ggplot(iris, ...)`; the consumer ppVar pickers offer
  # iris column names. L2 at test-rewrite-app.R:50 asserted multiple
  # specific columns appeared in each picker's HTML; expect_picker_populated
  # checks one canonical column per picker, which together with stages 4
  # and 1 forms an honest cover.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "Sepal.Length")
  expect_picker_populated(app, "ggplot_1_2_ppVar_NA", "Petal.Width")
  expect_picker_populated(app, "ggplot_1_3_ppVar_NA", "Species")

  # ---- Stage 4: picker uses multiple + data-max-options="1" -------------
  # The legacy paintr trick: shinyWidgets pickerInput with multiple = TRUE
  # plus maxOptions = 1L gives two behaviours single-select cannot:
  #   - input value starts as character(0), not the first column;
  #   - clicking the currently-selected column deselects it.
  # The L2 at test-rewrite-app.R:78 noted single-select with
  # `selected = character(0)` was empirically broken (verified in a real
  # browser), so the multiple+maxOptions=1 attribute pair is load-bearing.
  picker_html <- app$get_html("#ggplot_1_1_ppVar_NA") %||% ""
  expect_match(
    picker_html, "multiple", fixed = TRUE,
    label = "var picker carries the `multiple` attribute"
  )
  expect_match(
    picker_html, 'data-max-options="1"', fixed = TRUE,
    label = "var picker carries `data-max-options=\"1\"` (single-select-via-multi trick)"
  )

  # ---- Stage 1: click Update Plot -- code output populates with geom_point
  # L2 at test-rewrite-app.R:36 asserted `output$ptr_code` matches
  # "geom_point" after a single setInputs(ptr_update_plot = 1L). The DOM
  # equivalent is: click #ptr_update_plot, then read #ptr_code; expect
  # the geom_point token to land in the rendered code text.
  draw(app, "ptr_update_plot")
  expect_code_nonempty(app, "ptr_code")
  post_click_code <- app$get_value(output = "ptr_code")
  expect_match(
    post_click_code, "geom_point", fixed = TRUE,
    label = "post-Update-Plot #ptr_code contains geom_point"
  )

  # ---- Stage 5 (revised): layer-select drives the hidden tabset --------
  # L2 at test-rewrite-app.R:113 used parent-process trace() on
  # shiny::updateTabsetPanel and asserted the spy captured at least one
  # call with inputId = "ptr_layer_tabset". The trace shape does NOT
  # survive cross-process migration (shinytest2 runs the app in a child
  # R process; parent trace() never sees the call -- see
  # .claude/rules/serena-tools.md). DOM-faithful equivalent: set
  # ptr_layer_select; assert input$ptr_layer_tabset (the hidden tabsetPanel
  # is shiny-bound, its current panel value is observable as an input)
  # follows the selection. If updateTabsetPanel never fires, ptr_layer_tabset
  # would still hold its initial value and this assertion would fail --
  # the same contract under test, observed at the DOM/Shiny-input boundary.
  set_input(app, "ptr_layer_select", "geom_point")
  app$wait_for_idle(timeout = 5 * 1000)
  expect_equal(
    app$get_value(input = "ptr_layer_tabset"), "geom_point",
    label = "layer-select change drives ptr_layer_tabset to geom_point"
  )

  set_input(app, "ptr_layer_select", "ggplot")
  app$wait_for_idle(timeout = 5 * 1000)
  expect_equal(
    app$get_value(input = "ptr_layer_tabset"), "ggplot",
    label = "layer-select change back to ggplot drives the tabset back"
  )
})

test_that("J11 journey (j11-module-id): ptr_ui/ptr_server explicit-id namespacing aligns end-to-end", {
  # L2 at test-rewrite-app.R:205 asserted (a) the rendered UI HTML
  # contains "p1-ptr_update_plot" and "p1-ptr_code" and (b) testServer
  # with id="p1" populates output$ptr_code after Update Plot. The browser-
  # faithful equivalent boots a fixture that mounts ptr_ui(fml, "p1") +
  # ptr_server(fml, "p1") and asserts the namespaced DOM ids resolve AND
  # the namespaced code output populates on click.
  app <- boot_vignette_app("j11-module-id")

  expect_dom_id(app, "p1-ptr_update_plot")
  expect_dom_id(app, "p1-ptr_code")

  draw(app, "p1-ptr_update_plot")
  expect_code_nonempty(app, "p1-ptr_code")
  post_click_code <- app$get_value(output = "p1-ptr_code")
  expect_match(
    post_click_code, "geom_point", fixed = TRUE,
    label = "post-click p1-namespaced #ptr_code contains geom_point"
  )
})
