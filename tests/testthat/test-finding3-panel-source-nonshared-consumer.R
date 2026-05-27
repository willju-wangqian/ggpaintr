# FINDING #3 (composite scenario, placeholder-role-coverage2.html v7):
# panel-owned ppUpload(shared='ds') + downstream *non-shared* ppVar(...)
# consumers in two formulas. Prior coverage (test-shared-source-panel-multi-
# instance.R WE#3) uses shared-section consumers (ppVar(shared=...)); this
# pins the strictly different non-shared composite end-to-end.
#
# Mechanism under test:
#  * `ptr_setup_panel_sources()` (R/paintr-shared-ui.R) resolves the panel
#    source at host scope.
#  * `ptr_setup_pipelines()` (R/paintr-server.R) mirrors the resolved frame
#    into per-instance `state$resolved_sources` + `state$bound_names` for
#    each formula.
#  * The non-shared consumer's `source_ready` gate (R/paintr-server.R)
#    req()s those slots, and the per-instance renderUI populates the picker
#    from `names(<resolved-df>)`.
# No test prior to this one assertively pins all three steps together.

test_that(
  "FINDING #3 -- panel-owned ppUpload feeds non-shared ppVar consumers", {
    skip_on_cran()
    skip_if_not_installed("shinytest2")
    skip_if_not_installed("chromote")
    pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
    skip_if(
      !file.exists(file.path(pkg, "DESCRIPTION")),
      "finding3 e2e needs source root; absent under .Rcheck sandbox"
    )
    withr::local_envvar(GGP_PKG = pkg)
    prune_dead_ggpaintr_resource_paths()

    app_dir <- test_path(
      "fixtures", "vignette-apps",
      "finding3-panel-source-nonshared-consumer"
    )
    app <- suppressWarnings(
      shinytest2::AppDriver$new(
        app_dir,
        name = "finding3-panel-source-nonshared-consumer",
        load_timeout = 60 * 1000,
        timeout = 30 * 1000
      )
    )
    withr::defer(app$stop())

    app$wait_for_idle(timeout = 25 * 1000)

    # Panel-owned source: rendered once at host scope.
    expect_dom_id(app, "shared_ds")
    # The non-shared consumers' ids are namespaced per-instance; no
    # un-namespaced #ggplot_1_1_ppVar_NA exists at host scope.
    expect_no_dom_id(app, "ggplot_1_1_ppVar_NA")

    csv_path <- test_path("fixtures", "penguins.csv")
    app$upload_file(shared_ds = csv_path)
    set_input(app, "shared_ds_shortcut", "penguins")
    app$wait_for_idle(timeout = 15 * 1000)

    # Both non-shared pickers should be populated from the panel-resolved df.
    expect_picker_populated(
      app, "p1-ggplot_1_1_ppVar_NA", "bill_length_mm"
    )
    expect_picker_populated(
      app, "p2-ggplot_1_1_ppVar_NA", "bill_depth_mm"
    )

    # Contract B: the fixture's positional defaults `ppVar('xa')` and
    # `ppVar('xb')` are NOT columns in penguins.csv. The picker renders
    # with options populated (asserted above) but with NO selection -- there
    # is no first-column auto-fallback. The user MUST pick before drawing,
    # which is exactly what the `set_input(...)` lines below do.
    #
    # Pinning this contract here defends against a future regression to
    # Contract A (auto-fallback) which would silently mask formula-author
    # typos like `ppVar('food')` for `ppVar('food_g')`. If we ever WANT
    # auto-fallback, this assertion is the canary that must flip.
    # See dev/audit/audit-weak-assertions-2026-05-27.md "Notes" for the
    # full discussion.
    expect_equal(
      app$get_value(input = "p1-ggplot_1_1_ppVar_NA") %||% character(0),
      character(0),
      label = "ppVar('xa') -> no auto-fallback selection (Contract B)"
    )
    expect_equal(
      app$get_value(input = "p2-ggplot_1_1_ppVar_NA") %||% character(0),
      character(0),
      label = "ppVar('xb') -> no auto-fallback selection (Contract B)"
    )

    set_input(app, "p1-ggplot_1_1_ppVar_NA", "bill_length_mm")
    set_input(app, "p2-ggplot_1_1_ppVar_NA", "bill_depth_mm")
    draw(app, "p1-ptr_update_plot")
    draw(app, "p2-ptr_update_plot")
    expect_rendered(app, "#p1-ptr_plot", "ggplot")
    expect_rendered(app, "#p2-ptr_plot", "ggplot")
    expect_no_inline_error(app, "p1-ptr_error")
    expect_no_inline_error(app, "p2-ptr_error")
  }
)
