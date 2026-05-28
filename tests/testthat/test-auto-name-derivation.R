# ADR 0025 §3 / PLAN-02: translate-time stamp of `node$auto_name` for
# non-shared `ptr_ph_data_source` nodes. The stamp is the deterministic
# binding contract — same field read by the substitute walker
# (R/paintr-substitute.R, empty-snapshot fallback) and by
# `resolve_upload_source()` (R/paintr-server.R, empty-textbox fallback).
#
# For non-shared sources the auto-name is a system-generated, translate-
# time-stable `df_<hash(node$id)>` symbol — it depends ONLY on `node$id`
# (stamped at translate time), NOT on `node$default`. This is the
# post-upload binding name: once the user uploads a file the source
# mutex clears the shortcut textbox (R/paintr-server.R), so the binder
# falls back to `auto_name`. Deriving it from `node$default` leaked the
# env-shortcut symbol (e.g. `df_rug`) into the generated code; the
# `df_<hash>` form is a stable, `make.names`-valid system name instead.
# (The `df_` prefix guarantees validity: `rlang::hash` is hex, so a bare
# hash can start with a digit.)
#
# For shared sources the field stays NULL here; the runtime stamp inside
# `ptr_setup_panel_sources()` overwrites it with `<obj$id>_<key>` (see
# test-auto-name-shared-canonical.R).

# Expected auto-name for a node, computed the same way the helper does
# (`ptr_hash` = first 6 hex of rlang::hash). Kept inline so the test is
# self-contained and pins the derivation independently of the helper.
expected_auto_name <- function(node) paste0("df_", substr(rlang::hash(node$id), 1L, 6L))

test_that("ppUpload() (no default, no shared) — auto_name is df_<hash(id)>", {
  tree <- ptr_translate(
    "ppUpload() |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"
  )
  sources <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(sources, 1L)
  node <- sources[[1L]]
  expect_match(node$auto_name, "^df_[0-9a-f]{6}$")
  expect_identical(node$auto_name, expected_auto_name(node))
  # auto_name no longer equals node$id (the pre-fix behaviour).
  expect_false(identical(node$auto_name, node$id))
})

test_that("ppUpload(penguins) — auto_name is df_<hash(id)>, NOT the default symbol", {
  tree <- ptr_translate(
    "ppUpload(penguins) |> ggplot(aes(x = ppVar('species'))) + geom_bar()"
  )
  sources <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(sources, 1L)
  node <- sources[[1L]]
  # The user-supplied default is still recorded verbatim (seeds the
  # shortcut textbox at boot)...
  expect_identical(node$default, "penguins")
  # ...but the auto-name (the post-upload binding name) must NOT leak it.
  expect_match(node$auto_name, "^df_[0-9a-f]{6}$")
  expect_identical(node$auto_name, expected_auto_name(node))
  expect_false(identical(node$auto_name, node$default))
})

test_that("shared source — translate-time auto_name stays NULL (runtime owns it)", {
  # Shared sources are stamped by `ptr_setup_panel_sources()` at host-
  # scope server time; the translate-time helper must NOT pre-populate
  # `node$auto_name` for them or the panel-scope `<obj$id>_<key>` write
  # would collide with the per-instance value.
  tree <- ptr_translate(
    "ppUpload(shared='ds') |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"
  )
  sources <- find_nodes(tree, is_ptr_ph_data_source)
  expect_length(sources, 1L)
  expect_null(sources[[1L]]$auto_name)
})
