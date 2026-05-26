# FINDING #8 from dev/notes/placeholder-role-coverage2.html — spec= entry
# aimed at a `source_companion` id (the bare-name textInput inside ppUpload's
# tagList) must land on the companion's FIRST render at boot, before any
# user interaction. The downstream ppVar consumer should then resolve
# names(<seeded-frame>) immediately.
#
# Pre-fix bug (collapse misclassification in apply_spec_at_boot):
#   - state$spec_seed[[<companion-id>]] is written correctly (PLAN-02).
#   - The row is then added to `seeded`, which skips the onFlushed
#     updateTextInput dispatch (apply_spec_at_boot @ R/paintr-server.R:646).
#   - BUT no reader exists for state$spec_seed[[<companion-id>]] in the
#     server tree: every spec_seed read uses raw_id = node$id (the source
#     id), never the companion id; and ptr_builtin_upload_build_ui hard-
#     codes `value = node$default %||% ""` for the companion textInput
#     (R/paintr-builtins.R:228-250).
#   - Net: the companion's first render is blank regardless of spec=.
#
# Fix (minimal): exclude `source_companion` rows from `seeded` so they fall
# through to the onFlushed `updateTextInput` dispatch (apply_spec_entry @
# R/paintr-server.R:691-695 already handles the role correctly).

test_that("finding #8: spec= for a source_companion id overrides node$default at boot", {
  app <- boot_vignette_app("finding8-source-companion-spec")

  # apply_spec_at_boot defers `updateTextInput` for source_companion rows
  # into a session$onFlushed(once = TRUE) callback. Wait for the event
  # loop to settle so the dispatch + downstream resolve_upload_source
  # observer chain reaches quiescence.
  app$wait_for_idle(timeout = 15 * 1000)

  # 1. The companion textInput's runtime input value reflects the spec
  #    override, not the formula's `node$default` ("iris"). textInput has
  #    a standard Shiny binding that sends its initial value back to the
  #    server — `app$get_value(input = ...)` is the authoritative read.
  expect_equal(
    app$get_value(input = "ggplot_0_ppUpload_NA_name"),
    "mtcars",
    label = "companion id honored at boot (\"mtcars\" from spec, overriding default \"iris\")"
  )

  # 2. Downstream ppVar consumer is populated from names(mtcars), not
  #    names(iris). Asserts the full chain: spec → companion update →
  #    resolve_upload_source → state$eval_env / bound_names → consumer
  #    source_ready gate releases → ppVar picker renders with mtcars
  #    columns. `cyl` is an mtcars-only column (no overlap with iris's
  #    Sepal.Length / Sepal.Width / Petal.Length / Petal.Width / Species),
  #    so its presence discriminates the spec-override case from the
  #    pre-fix default-only case.
  expect_picker_populated(app, "ggplot_1_1_ppVar_NA", "cyl")

  # 3. No inline error along the way (the spec id is known and the
  #    resolve path is happy with `mtcars`).
  expect_no_inline_error(app, "ptr_error")
})
