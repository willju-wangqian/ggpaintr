# ADR 0025 §3 / PLAN-02: translate-time stamp of `node$auto_name` for
# non-shared `ptr_ph_data_source` nodes. The stamp is the deterministic
# binding contract — same field read by the substitute walker
# (R/paintr-substitute.R, empty-snapshot fallback) and by
# `resolve_upload_source()` (R/paintr-server.R, empty-textbox fallback).
#
# For non-shared sources:
#   `node$auto_name == node$default %||% node$id`
# For shared sources the field stays NULL here; the runtime stamp inside
# `ptr_setup_panel_sources()` overwrites it with `<obj$id>_<key>` (see
# test-auto-name-shared-canonical.R).

test_that("ppUpload() (no default, no shared) — auto_name equals node$id", {
  tree <- ptr_translate(
    "ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"
  )
  sources <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(sources, 1L)
  node <- sources[[1L]]
  expect_identical(node$auto_name, node$id)
  expect_match(node$auto_name, "_ppUpload_NA$")
})

test_that("ppUpload(penguins) — auto_name equals node$default verbatim", {
  tree <- ptr_translate(
    "ppUpload(penguins) |> ggplot(aes(x = ppVar('species'))) + geom_bar()"
  )
  sources <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(sources, 1L)
  node <- sources[[1L]]
  expect_identical(node$default, "penguins")
  expect_identical(node$auto_name, "penguins")
})

test_that("shared source — translate-time auto_name stays NULL (runtime owns it)", {
  # Shared sources are stamped by `ptr_setup_panel_sources()` at host-
  # scope server time; the translate-time helper must NOT pre-populate
  # `node$auto_name` for them or the panel-scope `<obj$id>_<key>` write
  # would collide with the per-instance `node$id %||% node$default` value.
  tree <- ptr_translate(
    "ppUpload(shared='ds') |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"
  )
  sources <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(sources, 1L)
  expect_null(sources[[1L]]$auto_name)
})
