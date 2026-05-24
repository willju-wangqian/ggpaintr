# ADR 0020 PLAN-02 — snapshot + UI readers honor node-level
# `default_active` / `default_stage_enabled` fields stamped by ppLayerOff /
# ppVerbOff (Plan 01). Covers SC1-SC6 + SC8 of the plan; SC7's post-flush
# browser half lives in `test-pp-off-e2e.R`.

# ---- SC1 / SC2 — snapshot reads carrier-node fields ----

test_that("SC1: snapshot's layer_checkbox entry reads default_active = FALSE", {
  tree <- ptr_translate("ggplot() + ppLayerOff(geom_point(), TRUE)")
  spec <- ptr_runtime_input_spec(tree)
  snap <- ggpaintr:::ptr_default_snapshot(spec, tree)
  expect_false(snap[["geom_point_checkbox"]])
})

test_that("SC1: snapshot's layer_checkbox entry defaults to TRUE without ppLayerOff", {
  tree <- ptr_translate("ggplot() + geom_point()")
  spec <- ptr_runtime_input_spec(tree)
  snap <- ggpaintr:::ptr_default_snapshot(spec, tree)
  expect_true(snap[["geom_point_checkbox"]])
})

test_that("SC2: snapshot's stage_enabled entry reads default_stage_enabled = FALSE", {
  tree <- ptr_translate(
    "ggplot(mtcars |> ppVerbOff(mutate(mpg = mpg + 100), TRUE))"
  )
  spec <- ptr_runtime_input_spec(tree)
  stage_rows <- spec[spec$role == "stage_enabled", , drop = FALSE]
  expect_equal(nrow(stage_rows), 1L)
  sid <- stage_rows$input_id[1L]
  snap <- ggpaintr:::ptr_default_snapshot(spec, tree)
  expect_false(snap[[sid]])
})

test_that("SC2: snapshot's stage_enabled entry defaults to TRUE without ppVerbOff", {
  tree <- ptr_translate(
    "ggplot(mtcars |> mutate(mpg = mpg + 100))"
  )
  spec <- ptr_runtime_input_spec(tree)
  stage_rows <- spec[spec$role == "stage_enabled", , drop = FALSE]
  if (nrow(stage_rows) > 0L) {
    sid <- stage_rows$input_id[1L]
    snap <- ggpaintr:::ptr_default_snapshot(spec, tree)
    expect_true(snap[[sid]])
  } else {
    succeed("formula produced no stage_enabled row; field not exercised")
  }
})

# ---- SC3 — snapshot ignores checkbox_defaults at the read site ----

test_that("SC3: snapshot ignores checkbox_defaults argument for layer_checkbox", {
  tree <- ptr_translate("ggplot() + geom_point()")
  spec <- ptr_runtime_input_spec(tree)
  snap_a <- ggpaintr:::ptr_default_snapshot(spec, tree, checkbox_defaults = NULL)
  snap_b <- suppressWarnings(ggpaintr:::ptr_default_snapshot(
    spec, tree, checkbox_defaults = list(geom_point = FALSE)
  ))
  expect_identical(snap_a, snap_b)
  expect_true(snap_a[["geom_point_checkbox"]])
})

# ---- SC4 — layer-checkbox UI honors node$default_active ----

test_that("SC4: layer-checkbox UI value reads node$default_active = FALSE", {
  tree <- ptr_translate("ggplot() + ppLayerOff(geom_point(), TRUE)")
  layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "geom_point"), logical(1)
  ))]]
  panel <- ggpaintr:::build_ui_for(layer)
  rendered <- as.character(panel)
  # checkbox should NOT carry checked="checked" when default_active is FALSE.
  expect_no_match(rendered, 'checked="checked"', fixed = TRUE)
  # And the content div should carry the disabled class.
  expect_match(rendered, "ptr-layer-disabled", fixed = TRUE)
})

test_that("SC4: layer-checkbox UI value reads node$default_active = TRUE (default)", {
  tree <- ptr_translate("ggplot() + geom_point()")
  layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "geom_point"), logical(1)
  ))]]
  panel <- ggpaintr:::build_ui_for(layer)
  rendered <- as.character(panel)
  expect_match(rendered, 'checked="checked"', fixed = TRUE)
  expect_no_match(rendered, "ptr-layer-disabled", fixed = TRUE)
})

test_that("SC4: layer-checkbox UI ignores checkbox_defaults at read site", {
  # ADR 0020: the layer-checkbox UI now reads node$default_active and does
  # NOT consult the legacy checkbox_defaults arg, even when threaded in.
  tree <- ptr_translate("ggplot() + geom_point()")
  layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "geom_point"), logical(1)
  ))]]
  panel <- ggpaintr:::build_ui_for(
    layer, checkbox_defaults = c(geom_point = FALSE)
  )
  rendered <- as.character(panel)
  # Should still be checked since default_active is TRUE (the legacy arg is
  # ignored at this read site).
  expect_match(rendered, 'checked="checked"', fixed = TRUE)
  expect_no_match(rendered, "ptr-layer-disabled", fixed = TRUE)
})

# ---- SC5 — per-layer Data-panel stage block honors default_stage_enabled ----

test_that("SC5: find_layer_placeholders_with_stage threads default_stage_enabled = FALSE", {
  tree <- ptr_translate(
    "ggplot(mtcars |> ppVerbOff(mutate(x = ppNum), TRUE), aes(x = mpg)) + geom_point()"
  )
  ggplot_layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "ggplot"), logical(1)
  ))]]
  entries <- ggpaintr:::find_layer_placeholders_with_stage(
    ggplot_layer$data_arg
  )
  expect_true(length(entries) >= 1L)
  for (e in entries) {
    expect_false(isTRUE(e$default_stage_enabled %||% TRUE),
                 info = "expected FALSE for ppVerbOff(..., TRUE) stage entry")
  }
})

test_that("SC5: per-layer stage controllable_region renders default_on = FALSE", {
  tree <- ptr_translate(
    "ggplot(mtcars |> ppVerbOff(mutate(x = ppNum), TRUE), aes(x = mpg)) + geom_point()"
  )
  ggplot_layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "ggplot"), logical(1)
  ))]]
  panel <- ggpaintr:::build_ui_for(ggplot_layer)
  rendered <- as.character(panel)
  # Extract the stage-head wrapper div and assert no checked attr.
  m <- regmatches(
    rendered,
    regexpr('class="ptr-stage-head">[\\s\\S]*?</div>[\\s\\S]*?</div>',
            rendered, perl = TRUE)
  )
  expect_true(length(m) >= 1L,
              info = "expected a .ptr-stage-head block in rendered UI")
  expect_no_match(m[[1L]], 'checked="checked"', fixed = TRUE)
})

test_that("SC5: bare pipeline stage (no ppVerbOff) renders default_on = TRUE", {
  tree <- ptr_translate(
    "ggplot(mtcars |> mutate(x = ppNum), aes(x = mpg)) + geom_point()"
  )
  ggplot_layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "ggplot"), logical(1)
  ))]]
  panel <- ggpaintr:::build_ui_for(ggplot_layer)
  rendered <- as.character(panel)
  m <- regmatches(
    rendered,
    regexpr('class="ptr-stage-head">[\\s\\S]*?</div>[\\s\\S]*?</div>',
            rendered, perl = TRUE)
  )
  expect_true(length(m) >= 1L)
  expect_match(m[[1L]], 'checked="checked"', fixed = TRUE)
})

# ---- SC6 — shared-stage UI honors default_stage_enabled ----

test_that("SC6: collect_orphan_shared_stages carries default_stage_enabled = FALSE", {
  tree <- ptr_translate(paste0(
    "ggplot(mtcars |> ppVerbOff(filter(ppVar(shared = \"v\") > 0), TRUE), ",
    "aes(x = ppVar(shared = \"v\"))) + geom_point()"
  ))
  orphans <- ggpaintr:::collect_orphan_shared_stages(tree)
  expect_true(length(orphans) >= 1L)
  expect_false(isTRUE(orphans[[1L]]$default_stage_enabled %||% TRUE))
})

test_that("SC6: collect_orphan_shared_stages defaults to TRUE without ppVerbOff", {
  tree <- ptr_translate(paste0(
    "ggplot(mtcars |> filter(ppVar(shared = \"v\") > 0), ",
    "aes(x = ppVar(shared = \"v\"))) + geom_point()"
  ))
  orphans <- ggpaintr:::collect_orphan_shared_stages(tree)
  if (length(orphans) >= 1L) {
    expect_true(isTRUE(orphans[[1L]]$default_stage_enabled %||% TRUE))
  } else {
    succeed("no orphan stages found; field not exercised on this branch")
  }
})

# ---- SC8 — spec_defaults_from_state agrees with snapshot ----

test_that("SC8: ptr_spec_defaults_from_state honors default_active = FALSE", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  state <- ptr_init_state(
    "ggplot() + ppLayerOff(geom_point(), TRUE)",
    envir = e
  )
  defaults <- ggpaintr:::ptr_spec_defaults_from_state(state)
  expect_false(defaults[["geom_point_checkbox"]])
  # And state$checkbox_defaults must be removed (no remaining consumers).
  expect_false("checkbox_defaults" %in% names(state))
})

test_that("SC8: ptr_spec_defaults_from_state agrees with snapshot for stage_enabled", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  state <- ptr_init_state(
    "ggplot(mtcars |> ppVerbOff(mutate(mpg = mpg + 100), TRUE))",
    envir = e
  )
  defaults <- ggpaintr:::ptr_spec_defaults_from_state(state)
  spec <- state$input_spec
  stage_rows <- spec[spec$role == "stage_enabled", , drop = FALSE]
  expect_equal(nrow(stage_rows), 1L)
  sid <- stage_rows$input_id[1L]
  expect_false(defaults[[sid]])
  # And without ppVerbOff, the same field defaults to TRUE.
  state2 <- ptr_init_state(
    "ggplot(mtcars |> mutate(mpg = mpg + 100))",
    envir = e
  )
  defaults2 <- ggpaintr:::ptr_spec_defaults_from_state(state2)
  spec2 <- state2$input_spec
  stage_rows2 <- spec2[spec2$role == "stage_enabled", , drop = FALSE]
  if (nrow(stage_rows2) > 0L) {
    sid2 <- stage_rows2$input_id[1L]
    expect_true(defaults2[[sid2]])
  }
})

# ---- snapshot vs UI agreement (single-formula assertions) ----

test_that("snapshot value agrees with UI checkboxInput value attribute (off case)", {
  formula <- "ggplot() + ppLayerOff(geom_point(), TRUE)"
  tree <- ptr_translate(formula)
  spec <- ptr_runtime_input_spec(tree)
  snap <- ggpaintr:::ptr_default_snapshot(spec, tree)
  layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "geom_point"), logical(1)
  ))]]
  rendered <- as.character(ggpaintr:::build_ui_for(layer))
  expect_false(snap[["geom_point_checkbox"]])
  expect_no_match(rendered, 'checked="checked"', fixed = TRUE)
})

test_that("snapshot value agrees with UI checkboxInput value attribute (on case)", {
  formula <- "ggplot() + geom_point()"
  tree <- ptr_translate(formula)
  spec <- ptr_runtime_input_spec(tree)
  snap <- ggpaintr:::ptr_default_snapshot(spec, tree)
  layer <- tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "geom_point"), logical(1)
  ))]]
  rendered <- as.character(ggpaintr:::build_ui_for(layer))
  expect_true(snap[["geom_point_checkbox"]])
  expect_match(rendered, 'checked="checked"', fixed = TRUE)
})
