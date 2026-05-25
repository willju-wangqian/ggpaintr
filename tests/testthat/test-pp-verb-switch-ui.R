# ADR 0021 / PLAN-05 — UI emission of stage_label.
#
# Verifies that the carrier ptr_call's `stage_label` (stamped by
# `ppVerbSwitch(..., label=)` per PLAN-03) is threaded through the
# entries-list packing in `find_layer_placeholders_with_stage()` /
# `collect_orphan_shared_stages()` and surfaces as the
# `controllable_region(head_label = ...)` argument of the per-layer pipeline
# stage block (`build_pipeline_stage_ui()`) and the shared-stage block
# (`wrap_shared_widgets_with_stage_blocks()`).
#
# Precedence (non-negotiable per ADR / plan): user-declared `stage_label`
# first (plain text, passed through unchanged) -> auto-derived `verb()`
# (wrapped in `<code>`) -> NULL.

# Helper: grab the `ggplot` layer (the host that carries the data_arg
# pipeline in every formula used here).
ggplot_layer_of <- function(tree) {
  tree$layers[[which(vapply(
    tree$layers, function(l) identical(l$name, "ggplot"), logical(1)
  ))]]
}

# Helper: extract the .ptr-stage-head <span>...</span> from a rendered UI
# panel. Anchors on the wrapper class so the regex stops at the head's
# closing </label>, not a later widget's.
extract_stage_head <- function(rendered) {
  regmatches(
    rendered,
    regexpr('class="ptr-stage-head">[[:print:][:space:]]*?</label>',
            rendered, perl = TRUE)
  )
}

# ---- SC-1: per-layer pipeline -- user stage_label replaces auto-label ----

test_that("SC-1: ppVerbSwitch(..., 'Transform x') renders head label as plain text 'Transform x'", {
  tree <- ptr_translate(
    'ggplot(mtcars |> ppVerbSwitch(mutate(x = ppNum), TRUE, "Transform x"), aes(x = mpg, y = wt)) + geom_point()'
  )
  panel <- ggpaintr:::build_ui_for(ggplot_layer_of(tree))
  rendered <- as.character(panel)
  head <- extract_stage_head(rendered)
  expect_true(length(head) == 1L,
              info = "expected exactly one .ptr-stage-head block")
  # User-declared plain text appears in the head label.
  expect_match(head[[1L]], "Transform x", fixed = TRUE)
  # The user's plain text is NOT wrapped in <code>...</code>.
  expect_no_match(head[[1L]], "<code>Transform x", fixed = TRUE)
  expect_no_match(head[[1L]], "Transform x</code>", fixed = TRUE)
  # And the auto-label <code>mutate()</code> must NOT leak past the user
  # override.
  expect_no_match(head[[1L]], "<code>mutate()</code>", fixed = TRUE)
})

# ---- SC-2: fallback to auto-label when stage_label is NULL ----

test_that("SC-2: ppVerbSwitch(..., TRUE) [no label] falls back to <code>mutate()</code> head label", {
  tree <- ptr_translate(
    'ggplot(mtcars |> ppVerbSwitch(mutate(x = ppNum), TRUE), aes(x = mpg, y = wt)) + geom_point()'
  )
  panel <- ggpaintr:::build_ui_for(ggplot_layer_of(tree))
  rendered <- as.character(panel)
  head <- extract_stage_head(rendered)
  expect_true(length(head) == 1L)
  expect_match(head[[1L]], "<code>mutate()</code>", fixed = TRUE)
})

# ---- SC-3: bare-pipeline entries with no carrier emit no stage block ----

test_that("SC-3: entries with NA stage_id / NA verb / NULL stage_label produce no stage block (bare .ptr-stage-row)", {
  # Synthesize an entries list matching the "bare pipeline" branch shape:
  # placeholder not enclosed in any stage carrier. `build_pipeline_stage_ui`
  # routes via the `is.na(sid)` branch and never calls `controllable_region`.
  ph <- ggpaintr:::ptr_ph_value(
    id = "x_node", keyword = "ppNum", expr = quote(ppNum)
  )
  entries <- list(
    list(
      ph = ph,
      stage_id = NA_character_,
      verb = NA_character_,
      default_stage_enabled = NULL,
      stage_label = NULL
    )
  )
  out <- ggpaintr:::build_pipeline_stage_ui(
    entries, ui_text = NULL, layer_name = "ggplot", ns_fn = identity
  )
  expect_true(length(out) >= 1L)
  rendered <- as.character(out[[1L]])
  # No controllable_region was emitted -> no .ptr-stage-head and no head
  # label rendering at all.
  expect_no_match(rendered, "ptr-stage-head", fixed = TRUE)
  # The bare placeholder branch wraps each widget in .ptr-stage-row.
  expect_match(rendered, "ptr-stage-row", fixed = TRUE)
})

# ---- SC-4: shared-stage variant honors stage_label ----

test_that("SC-4: ppVerbSwitch wrapping a shared placeholder threads stage_label = 'Custom' to the shared-stage emitter", {
  formula <- paste0(
    'ggplot(mtcars |> ppVerbSwitch(filter(ppVar(shared = "v") > 0), TRUE, "Custom"), ',
    'aes(x = ppVar(shared = "v"))) + geom_point()'
  )
  tree <- ptr_translate(formula)
  orphans <- ggpaintr:::collect_orphan_shared_stages(tree)
  expect_true(length(orphans) >= 1L)
  expect_identical(orphans[[1L]]$stage_label, "Custom")
  # Drive the emitter directly so we can read the rendered head label.
  # Build a single fake "widget" body to stand in for the shared widget.
  widgets <- list(shiny::div(id = "v_widget", "shared-widget-body"))
  # `entries` mirrors what the shared-section caller passes: one per shared
  # widget, with a `key` matching the orphan stage's shared_keys.
  entries <- list(list(key = "v"))
  out <- ggpaintr:::wrap_shared_widgets_with_stage_blocks(
    entries, widgets, orphans, ns_fn = identity
  )
  expect_true(length(out) == 1L)
  rendered <- as.character(out[[1L]])
  head <- extract_stage_head(rendered)
  expect_true(length(head) == 1L)
  # User-declared plain-text "Custom" in the head label, not <code>filter()</code>.
  expect_match(head[[1L]], "Custom", fixed = TRUE)
  expect_no_match(head[[1L]], "<code>filter()</code>", fixed = TRUE)
  expect_no_match(head[[1L]], "<code>Custom", fixed = TRUE)
})

# ---- SC-5: per-placeholder label-suffix rule unchanged ----

test_that("SC-5: per-placeholder label suffix is identical with vs without ppVerbSwitch wrapper (stage-level stage_label does NOT bleed into per-widget label)", {
  # Wrapper case: ppVerbSwitch with a label override.
  tree_W <- ptr_translate(
    'ggplot(mtcars |> ppVerbSwitch(mutate(x = ppNum), TRUE, "Custom"), aes(x = mpg, y = wt)) + geom_point()'
  )
  layer_W <- ggplot_layer_of(tree_W)
  entries_W <- ggpaintr:::find_layer_placeholders_with_stage(layer_W$data_arg)
  expect_true(length(entries_W) >= 1L)
  ph_id_W <- entries_W[[1L]]$ph$id
  override_W <- ggpaintr:::pipeline_override_for_node(tree_W, ph_id_W)

  # No-wrapper case: bare mutate(x = ppNum) with no ppVerbSwitch.
  tree_N <- ptr_translate(
    'ggplot(mtcars |> mutate(x = ppNum), aes(x = mpg, y = wt)) + geom_point()'
  )
  layer_N <- ggplot_layer_of(tree_N)
  entries_N <- ggpaintr:::find_layer_placeholders_with_stage(layer_N$data_arg)
  expect_true(length(entries_N) >= 1L)
  ph_id_N <- entries_N[[1L]]$ph$id
  override_N <- ggpaintr:::pipeline_override_for_node(tree_N, ph_id_N)

  # The per-placeholder `label_suffix` (driven by verb + sid) is identical
  # between the two cases. The stage-level `stage_label` override only
  # touches the stage-block head label; the per-widget label-suffix rule
  # is untouched (ADR 0021 "Out of scope" §4 + plan 05 SC-5).
  expect_identical(override_W$label_suffix, override_N$label_suffix)
  # And the same for the param_override (also verb-driven).
  expect_identical(override_W$param_override, override_N$param_override)
})

# ---- SC-6: ppVerbSwitch(switch_on = FALSE) stage block still uses auto-label ----

test_that("SC-6: ppVerbSwitch carrier without a label slot renders head label as <code>mutate()</code> (auto-label fallback)", {
  tree <- ptr_translate(
    'ggplot(mtcars |> ppVerbSwitch(mutate(x = ppNum), switch_on = FALSE), aes(x = mpg, y = wt)) + geom_point()'
  )
  panel <- ggpaintr:::build_ui_for(ggplot_layer_of(tree))
  rendered <- as.character(panel)
  head <- extract_stage_head(rendered)
  expect_true(length(head) == 1L)
  expect_match(head[[1L]], "<code>mutate()</code>", fixed = TRUE)
})

# ---- SC-7: entries list carries stage_label field ----

test_that("SC-7: entries packed from a ppVerbSwitch(..., 'X') carrier have identical(entry$stage_label, 'X')", {
  tree <- ptr_translate(
    'ggplot(mtcars |> ppVerbSwitch(mutate(x = ppNum), TRUE, "X"), aes(x = mpg)) + geom_point()'
  )
  layer <- ggplot_layer_of(tree)
  entries <- ggpaintr:::find_layer_placeholders_with_stage(layer$data_arg)
  expect_true(length(entries) >= 1L)
  # Every entry stamped by the carrier has stage_label = "X" (the carrier's
  # stamp propagates to every placeholder under the stage's branch).
  for (e in entries) {
    expect_true("stage_label" %in% names(e),
                info = "entry must have a stage_label field after PLAN-05")
    expect_identical(e$stage_label, "X")
  }
})
