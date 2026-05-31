# test-shared-source-panel-multi-instance.R -- ADR 0023 / PLAN-08.
#
# End-to-end browser regression for the multi-instance panel-owned shared
# source path: a panel-owned `ppUpload(shared='ds')` resolves once and the
# panel-owned `ppVar(shared='col')` picker -- plus the per-instance plot
# outputs -- all read from that single upstream resolution. This is the
# textbook Bug 1 reproducer from ADR 0023's Context; Plans 01-07 each PASS
# their own gates but none exercise the composition in a real browser, so
# this file is the assertion that the per-plan mechanism actually delivers
# the promised user observable.
#
# Project memory `shinytest2-appdir-pkgload` is the load-bearing protocol:
#   * Fixture app.R first line: pkgload::load_all(Sys.getenv("GGP_PKG"))
#   * Test sets GGP_PKG = normalizePath(test_path("..","..")).
#   * AppDriver$new wrapped in suppressWarnings (benign warnings).
#   * NEVER app$get_values() (500s on custom-renderer apps).
#   * set_inputs(..., wait_ = FALSE) for placeholder sets; click Draw.
#   * `var` pickers are suspended -- open the layer's Controls subtab
#     after setting upstream inputs so the renderUI binds.
#
# Test 4 (R1 negative) does not need a browser -- the abort fires at
# `ptr_shared()` construction time in the test process.

# ---------------------------------------------------------------------------
# Scenario: ADR worked example #1 -- two plots sharing one uploaded dataset
# ---------------------------------------------------------------------------
test_that("ADR worked example #1 -- two plots sharing one uploaded dataset", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "panel-multi e2e needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-panel-multi")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "shared-source-panel-multi",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)

  # The panel-owned source (#shared_ds fileInput) and the panel-owned
  # consumer container (#shared_col / its uiOutput) must be in the DOM at
  # boot -- both belong to the host scope, NOT per-instance.
  expect_dom_id(app, "shared_ds")

  # Upload the textbook CSV; the panel host observer must resolve it once
  # and mirror into the per-instance state for both p1 and p2 (Plan 05).
  csv_path <- test_path("fixtures", "penguins.csv")
  upload_file(app, shared_ds = csv_path)
  # ADR 0025 §3: the shortcut textbox NO LONGER names an upload (the F2
  # name-override role is retired -- shared uploads bind under the canonical
  # auto-name `ds`, with the shortcut left empty). The fixture has no
  # `penguins` object in its env, so typing "penguins" here would switch the
  # source to the env-shortcut loader and fail (`object 'penguins' not
  # found`). Leave the shortcut empty; the upload binds under the canonical.

  # The plan's "p1_subtab / p2_subtab = Controls" prescription assumes the
  # standalone ptr_app() layer Data/Controls subtab, but the embedded
  # ptr_ui() layer picker uses a hidden tabset (`p1-ptr_layer_tabset`)
  # driven by `p1-ptr_layer_select`, with no Data/Controls split inside
  # each layer panel. Panel-shared `#shared_col` lives at host scope and
  # is bound at boot (not suspended). Just wait for the panel-source
  # observer to mirror the uploaded df into panel_sources, then assert.
  app$wait_for_idle(timeout = 15 * 1000)

  # The panel-owned consumer picker must offer the uploaded columns.
  # penguins.csv columns: species,island,bill_length_mm,bill_depth_mm,
  # flipper_length_mm,body_mass_g,sex,year -- spot-check three.
  shared_col_html <- app$get_html("#shared_col") %||% ""
  expect_true(nzchar(shared_col_html),
              label = "#shared_col picker rendered (host scope, not empty)")
  for (col in c("species", "bill_length_mm", "body_mass_g", "year")) {
    expect_true(
      grepl(col, shared_col_html, fixed = TRUE),
      label = paste0("#shared_col offers \"", col, "\"")
    )
  }

  # Drive the picker to a real column and click Draw on both instances.
  set_input(app, "shared_col", "bill_length_mm")
  draw(app, "p1-ptr_update_plot")
  draw(app, "p2-ptr_update_plot")

  # Both plot outputs render -- ggpaintr's default renderer is renderPlot()
  # which emits a base64 <img> (NOT raw <svg>; see helper-vignette-apps.R
  # `expect_rendered("ggplot")`'s pat = "<img"). PLAN-08 SC #3's "<svg"
  # wording is a prose-vs-renderer drift -- surfaced in the implementer
  # report. The semantic gate ("non-empty rendered output") is what matters,
  # and is what the bug would break.
  expect_rendered(app, "#p1-ptr_plot", "ggplot")
  expect_rendered(app, "#p2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "p1-ptr_error")
  expect_no_inline_error(app, "p2-ptr_error")
})


# ---------------------------------------------------------------------------
# Scenario: ADR worked example R1 -- panel-owned consumer + formula-local
# sources aborts at ptr_shared() time (e2e smoke; no fixture)
# ---------------------------------------------------------------------------
test_that("R1 e2e smoke (class + names)", {
  # Plan 03 invariant exercised from this file as well so the multi-instance
  # regression file fully covers the ADR's worked examples (positive AND
  # negative). Construction failure -- no browser needed.
  formulas <- c(
    "ggplot(mtcars, aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(iris,   aes(x = ppVar(shared='col'))) + geom_point()"
  )
  err <- expect_error(
    ptr_shared(formulas),
    class = "ptr_panel_consumer_source_mismatch"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "col",    fixed = TRUE)
  expect_match(msg, "mtcars", fixed = TRUE)
  expect_match(msg, "iris",   fixed = TRUE)
  # Structured fields on the condition -- embedders can tryCatch and render
  # their own UI; the partition-mismatch carries the consumer key and the
  # offending source descriptors verbatim.
  expect_equal(err$key, "col")
  expect_true("mtcars" %in% err$sources)
  expect_true("iris"   %in% err$sources)
  expect_setequal(err$formulas, formulas)
})


# ---------------------------------------------------------------------------
# Scenario: ADR worked example #4 -- single-instance shared source still
# works post-Cut-2 (Cut-1 regression preserved by Plan 06's partition-aware,
# rather than blanket, skip).
# ---------------------------------------------------------------------------
test_that("ADR worked example #4 -- single-instance shared source still works", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "single-instance shared-source e2e needs the package source root; absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  # Re-use the existing fixture per PLAN-08 SC5 ("re-uses the existing
  # single-instance fixture from test-shared-source-rendering.R if present").
  app_dir <- test_path("fixtures", "vignette-apps", "shared-source-rendering-bare")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "panel-multi-cut1-regression",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)
  # The single-instance fileInput must render -- the Cut-1 fix that this
  # plan's Cut-2 partition-aware skip must NOT have re-broken.
  expect_dom_id(app, "shared_ds")

  csv_path <- test_path("fixtures", "penguins.csv")
  upload_file(app, shared_ds = csv_path)
  # ADR 0025 §3: shortcut no longer names an upload; leave it empty so the
  # upload binds under the canonical auto-name (no `penguins` env frame).
  set_input(app, "ggplot_subtab", "Controls")
  # ADR 0025 §7 A2: the shortcut bind is debounced 400ms. The subtab-switch
  # render settles fast, so wait_for_idle() can return BEFORE the debounce
  # fires and the picker repopulates from the resolved source. Sleep past
  # the window, then re-idle. (Worked example #1 happens not to hit this
  # ordering because it does not interleave a subtab-switch render.)
  Sys.sleep(0.5)
  app$wait_for_idle(timeout = 15 * 1000)

  # Consumer picker `shared_a` (both x and y in the bare fixture's formula)
  # must populate with the uploaded columns -- single-instance ppVar(shared=)
  # is the host-scope binder path the regression protects.
  expect_picker_populated(app, "shared_a", "bill_length_mm")

  set_input(app, "shared_a", "bill_length_mm")
  draw(app, "ptr_update_plot")
  expect_rendered(app, "#ptr_plot", "ggplot")
  expect_no_inline_error(app, "ptr_error")
})


# ---------------------------------------------------------------------------
# Scenario: ADR worked example #2 -- default_arg primes pickers at boot.
# ---------------------------------------------------------------------------
test_that("ADR worked example #2 -- default_arg primes pickers at boot", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "panel-multi default-arg e2e needs the package source root; absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps",
                       "shared-source-panel-multi-default")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "shared-source-panel-multi-default",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)
  # The host source UI is still emitted -- default_arg primes the *resolved
  # value*, not the widget shape (ADR R4).
  expect_dom_id(app, "shared_ds")
  # The browser-driven file slot is NEVER written by default_arg (ADR R3 /
  # PLAN-04 Scenario 7).
  expect_null(app$get_value(input = "shared_ds"))

  # Embedded ptr_ui() has no Data/Controls subtab (it uses a hidden layer
  # tabset selected by `p1-ptr_layer_select`); host-scope `#shared_col` is
  # bound at boot. Just wait for the default-arg observer to prime
  # panel_sources, then assert.
  app$wait_for_idle(timeout = 15 * 1000)

  # The shared consumer picker must be populated from the default-arg-
  # resolved mtcars frame at boot. Spot-check three mtcars columns.
  shared_col_html <- app$get_html("#shared_col") %||% ""
  for (col in c("mpg", "cyl", "hp")) {
    expect_true(
      grepl(col, shared_col_html, fixed = TRUE),
      label = paste0("#shared_col offers \"", col,
                     "\" from default-arg-resolved mtcars at boot")
    )
  }
  # And the picker's *value* is either NULL (no auto-selection, the
  # default for shinyWidgets pickers without a `default` arg or a
  # `spec = list(shared_col = ...)` seed) or a valid mtcars column.
  # The 3 `grepl` spot-checks above are the load-bearing proof that the
  # picker is bound to real choices (not a blank uiOutput); the value
  # check below just guards against accidental garbage selection.
  val <- app$get_value(input = "shared_col")
  expect_true(
    is.null(val) ||
      (is.character(val) && length(val) == 1L && val %in% names(mtcars)),
    label = paste0("#shared_col value is NULL or a names(mtcars) member ",
                   "at boot (got: ", deparse(val), ")")
  )
})


# ---------------------------------------------------------------------------
# Scenario: ADR worked example #3 -- mixed scope (panel source, formula-
# local consumers) wires correctly.
# ---------------------------------------------------------------------------
test_that("ADR worked example #3 -- mixed scope wires correctly", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "panel-multi mixed-scope e2e needs the package source root; absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  prune_dead_ggpaintr_resource_paths()

  app_dir <- test_path("fixtures", "vignette-apps",
                       "shared-source-panel-multi-mixed")
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = "shared-source-panel-multi-mixed",
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop())

  app$wait_for_idle(timeout = 25 * 1000)
  # Panel-owned source: present at host scope.
  expect_dom_id(app, "shared_ds")
  # Formula-local consumers: NO panel-scope #shared_colA / #shared_colB;
  # they live ONLY under per-instance namespaces (canonical_shared_id
  # convention -- see R/paintr-shared.R / test-ui-pieces.R).
  expect_no_dom_id(app, "shared_colA")
  expect_no_dom_id(app, "shared_colB")

  csv_path <- test_path("fixtures", "penguins.csv")
  upload_file(app, shared_ds = csv_path)
  # ADR 0025 §3: shortcut no longer names an upload; leave it empty so the
  # upload binds under the canonical auto-name (no `penguins` env frame).
  # Embedded ptr_ui() uses a hidden layer tabset rather than the
  # Data/Controls split (see notes in the worked-example #1 scenario);
  # the formula-local consumer pickers are bound at boot.
  app$wait_for_idle(timeout = 15 * 1000)

  # Each formula-local consumer picker reads from the panel-resolved df.
  expect_picker_populated(app, "p1-shared_colA", "bill_length_mm")
  expect_picker_populated(app, "p2-shared_colB", "bill_depth_mm")

  set_input(app, "p1-shared_colA", "bill_length_mm")
  set_input(app, "p2-shared_colB", "bill_depth_mm")
  draw(app, "p1-ptr_update_plot")
  draw(app, "p2-ptr_update_plot")
  expect_rendered(app, "#p1-ptr_plot", "ggplot")
  expect_rendered(app, "#p2-ptr_plot", "ggplot")
  expect_no_inline_error(app, "p1-ptr_error")
  expect_no_inline_error(app, "p2-ptr_error")
})
