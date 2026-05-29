# ADR 0025 contract (ii), bug fix 2026-05-29: the post-boot new-source clear
# must fire when a BARE-DATA-LAYER source (`geom_rug(data = ppUpload(), ...)`)
# is shortcut-renamed to a different dataset. The clear is gated on
# `consumer_upstream_source_state()` reporting a CHANGED source `identity`
# across renders. A layer source publishes its bound dataset name under
# `layer_name`, not the source node id, so the pre-fix helper (which only read
# `state$bound_names[[sid]]`) produced a CONSTANT `identity` of `sid##` -- the
# rename was invisible and a stale column pick rode onto the new dataset.
#
# Two seams:
#   1. unit  -- `consumer_upstream_source_state()` identity stability under the
#               layer-keyed bound-name regime. Discriminating: omit `layer_name`
#               (the pre-fix call) and the two renames collapse to one identity;
#               pass it and they diverge.
#   2. e2e   -- the real ChickWeight -> PlantGrowth rename on a layer ppUpload,
#               with a shared `weight` column so the stale pick is observable
#               (a non-shared column would be dropped by the picker's own
#               intersect() and hide the bug).

# ---- Seam 1: identity tracks the layer-keyed bound name -------------------

test_that("consumer_upstream_source_state: layer source identity tracks the renamed dataset", {
  # A bare-data-layer source: its node id differs from the layer-name key the
  # resolved frame is bound under (`state$bound_names[["geom_rug"]]`).
  src <- list(
    id = "geom_rug_0_ppUpload_NA",
    shortcut_id = "geom_rug_0_ppUpload_NA_shortcut",
    default = NULL
  )

  # `bound_names` is keyed by LAYER NAME for a bare-data layer; there is no
  # entry under the source node id. `bound` is the live dataset name.
  bound <- "ChickWeight"
  state <- list(bound_names = list(geom_rug = function() bound))

  # No file uploaded; the shortcut textbox carries the dataset name.
  read_input <- function(id) {
    if (identical(id, src$shortcut_id)) bound else NULL
  }

  chick <- ggpaintr:::consumer_upstream_source_state(
    list(src), read_input, state, layer_name = "geom_rug"
  )
  bound <- "PlantGrowth"
  plant <- ggpaintr:::consumer_upstream_source_state(
    list(src), read_input, state, layer_name = "geom_rug"
  )

  # The fix: the rename changes the snapshot identity, so the caller's
  # `!identical(identity, last_identity)` edge fires the contract-(ii) clear.
  expect_false(
    identical(chick$identity, plant$identity),
    label = "layer-source identity changes when the shortcut renames the dataset"
  )
  expect_true(chick$user_supplied)
  expect_true(plant$user_supplied)

  # Discriminator: WITHOUT `layer_name` (the pre-fix call signature) the helper
  # cannot see the layer-keyed bound name, so both renames collapse to the same
  # constant `sid##` identity -- this is exactly the silent miss the fix cures.
  bound <- "ChickWeight"
  chick0 <- ggpaintr:::consumer_upstream_source_state(list(src), read_input, state)
  bound <- "PlantGrowth"
  plant0 <- ggpaintr:::consumer_upstream_source_state(list(src), read_input, state)
  expect_identical(
    chick0$identity, plant0$identity,
    label = "pre-fix regime (no layer_name): identity is constant across the rename (the bug)"
  )
})

# ---- Seam 2: end-to-end rename clears the layer picker --------------------

app_dir_layer_rename <- function() {
  test_path("fixtures", "vignette-apps", "adr25-layer-shortcut-rename-clear")
}

test_that("adr25: shortcut-renaming a LAYER ppUpload to a new dataset clears the picker", {
  skip_on_cran()
  skip_if_not_installed("shinytest2")
  skip_if_not_installed("chromote")
  pkg <- normalizePath(test_path("..", ".."), mustWork = FALSE)
  skip_if(
    !file.exists(file.path(pkg, "DESCRIPTION")),
    "adr25 browser test needs the package source root (pkgload::load_all); absent under .Rcheck sandbox"
  )
  withr::local_envvar(GGP_PKG = pkg)

  app <- suppressWarnings(shinytest2::AppDriver$new(
    app_dir_layer_rename(),
    name = "adr25-layer-shortcut-rename-clear", timeout = 30 * 1000
  ))
  on.exit(app$stop(), add = TRUE)
  app$wait_for_idle(timeout = 20 * 1000)

  # Reveal the layer's Data subtab so its source/consumer pickers bind.
  app$set_inputs(ptr_layer_select = "geom_rug", wait_ = FALSE)
  app$set_inputs(geom_rug_subtab = "Data", wait_ = FALSE)
  app$wait_for_idle(timeout = 20 * 1000)

  # Name the layer source `ChickWeight`, then pick its `weight` column.
  app$set_inputs(geom_rug_0_ppUpload_NA_shortcut = "ChickWeight", wait_ = FALSE)
  app$wait_for_idle(timeout = 20 * 1000)
  app$set_inputs(geom_rug_1_1_ppVar_NA = "weight", wait_ = FALSE)
  app$wait_for_idle(timeout = 20 * 1000)
  expect_equal(
    app$get_value(input = "geom_rug_1_1_ppVar_NA"), "weight",
    label = "user picks `weight` on the ChickWeight layer source"
  )

  # Rename to `PlantGrowth` (also has a `weight` column). Contract (ii): the
  # picker clears -- the stale `weight` must NOT ride onto the new dataset.
  app$set_inputs(geom_rug_0_ppUpload_NA_shortcut = "PlantGrowth", wait_ = FALSE)
  app$wait_for_idle(timeout = 20 * 1000)
  after <- app$get_value(input = "geom_rug_1_1_ppVar_NA")
  expect_true(
    is.null(after) || length(after) == 0L || identical(after, ""),
    label = "post-rename layer consumer picker is cleared (stale `weight` does not carry)"
  )

  # Sanity: the data side DID switch (picker now lists PlantGrowth's `group`),
  # so the clear is a genuine contract-(ii) clear, not a frozen render.
  html <- app$get_html("#geom_rug_1_1_ppVar_NA")
  expect_true(
    !is.null(html) && grepl("group", html, fixed = TRUE),
    label = "picker columns updated to the renamed dataset (PlantGrowth has `group`)"
  )
})
