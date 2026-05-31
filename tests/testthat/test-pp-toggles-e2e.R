# ADR 0020 / 0021 PLAN-06 — SC7's post-flush half: shinytest2 boot proves the
# rendered widget's value attribute AGREES with both the snapshot value and
# the post-flush `app$get_value()` reading.
#
# Mirrors the boot pattern from helper-vignette-apps.R but parameterizes the
# fixture root (plan owns `tests/testthat/fixtures/pp-toggles-apps/<slug>/`).

boot_pp_toggles_app <- function(slug) {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  pkg <- normalizePath(testthat::test_path("..", ".."), mustWork = FALSE)
  testthat::skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "e2e pp-toggles-app boot needs the package source root (pkgload::load_all); absent under the R CMD check .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  if (exists("prune_dead_ggpaintr_resource_paths",
             envir = asNamespace("ggpaintr"), inherits = FALSE) ||
      exists("prune_dead_ggpaintr_resource_paths")) {
    try(prune_dead_ggpaintr_resource_paths(), silent = TRUE)
  }
  app_dir <- testthat::test_path("fixtures", "pp-toggles-apps", slug)
  app <- suppressWarnings(
    shinytest2::AppDriver$new(
      app_dir,
      name = paste0("e2e-pp-toggles-", slug),
      load_timeout = 60 * 1000,
      timeout = 30 * 1000
    )
  )
  withr::defer(app$stop(), envir = parent.frame())
  app
}

test_that("SC7 e2e: ppLayerOff(hide=TRUE) — geom_point checkbox boots unchecked", {
  app <- boot_pp_toggles_app("pp-layer-off-hide")
  # The geom_point layer checkbox should boot with value = FALSE (matches
  # both the snapshot's `geom_point_checkbox` and ADR 0020 default_active=FALSE).
  v <- app$get_value(input = "geom_point_checkbox")
  expect_false(isTRUE(v))
  # And the rendered checkbox tag's value attribute must agree (DOM-level).
  html <- app$get_html("#geom_point_checkbox")
  expect_false(
    is.null(html) || grepl('checked="checked"', html, fixed = TRUE),
    label = "rendered #geom_point_checkbox has no checked attribute"
  )
})

test_that("SC8 spec = at boot overrides formula-side ppLayerOff default (shinytest2)", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")

  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "e2e pp-toggles-app boot needs the package source root (pkgload::load_all); absent under the R CMD check .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)
  app <- suppressWarnings(shinytest2::AppDriver$new(
    test_path("fixtures", "pp-toggles-apps", "pp-layer-off-spec-override"),
    name = "pp-layer-off-spec-override"
  ))
  on.exit(app$stop(), add = TRUE)

  # BDD `Then`: input value after flush equals TRUE. Poll for the seeded
  # value rather than sampling once — the spec `=` override lands a flush
  # after the Connect handshake, and under parallel-execution CPU contention
  # that lag is seconds (a single immediate read races it).
  expect_input_eventually(app, "geom_point_checkbox", TRUE)
})

test_that("SC7 e2e: ppVerbSwitch(switch_on=FALSE) — stage checkbox boots unchecked", {
  app <- boot_pp_toggles_app("pp-verb-switch-off")
  # Don't use app$get_values() — it 500s on custom-renderer apps with a
  # pre-draw silent error. Instead, query the .ptr-stage-head block DOM
  # under the ggplot layer's Data subtab and check that no
  # checked="checked" attribute appears in it (i.e. the stage boots OFF).
  #
  # The ggplot layer's stage-head DOM ids are
  # `<ns>(<stage_id>_stage_block)`; the layer panel ns prefixes them with
  # the layer's name. For ptr_app's default plot module the ns is empty
  # so the id is just `<stage_id>_stage_block`. Look for any
  # ptr-stage-head with a checkbox lacking checked="checked".
  html <- app$get_html("body")
  expect_true(grepl("ptr-stage-head", html %||% "", fixed = TRUE),
              info = "expected at least one .ptr-stage-head block in the rendered app")
  # Extract the stage-head <div>...</div> blocks and assert at least one
  # is unchecked (the ppVerbSwitch(..., switch_on = FALSE)'d filter stage).
  stage_blocks <- regmatches(
    html,
    gregexpr('class="ptr-stage-head">[\\s\\S]*?</div>', html, perl = TRUE)
  )[[1L]]
  expect_true(length(stage_blocks) >= 1L)
  any_unchecked <- any(!vapply(
    stage_blocks,
    function(b) grepl('checked="checked"', b, fixed = TRUE),
    logical(1)
  ))
  expect_true(any_unchecked,
              info = "at least one ppVerbSwitch(switch_on = FALSE) stage checkbox boots unchecked")
})
