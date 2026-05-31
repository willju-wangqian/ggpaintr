# PLAN-02 (ADR 0010) — stamp_current_pick_walk.ptr_ph_data_source reads the
# companion-id snapshot for ppUpload preserve-mode round-trip.
#
# The fileInput value at `snapshot[[node$id]]` is a four-column data.frame and
# is not the meaningful "current pick" for upload-style data-source
# placeholders. The user-typed display name lives at
# `snapshot[[node$shortcut_id]]`. PLAN-02 rewires the stamp method to read
# the companion-id snapshot when present, while preserving the legacy
# node$id path for companion-less data-source registry entries.

# ---- helpers --------------------------------------------------------------

# Synthetic ppUpload data-source node with explicit shortcut_id, bypassing
# the registry so these tests stay order-independent w.r.t. PLAN-01.
ppupload_node <- function(id = "up1", shortcut_id = "up1_shortcut") {
  ggpaintr:::ptr_ph_data_source(
    id = id,
    keyword = "ppUpload",
    expr = quote(ppUpload()),
    shortcut_id = shortcut_id
  )
}

# ---- scenario 1: companion-id snapshot populates current_pick -------------

# Scenarios 1, 2, 5, 6, 7 (companion-id populates / empty-string leaves
# NULL / render emits ppUpload(penguins) / render emits bare ppUpload /
# end-to-end translate+render) are absorbed into the preserve-mode unit
# blocks in test-adr10-ppupload-name-browser.R:84 / :120 (post-extension
# 2026-05-27), which exercise the same stamp_current_pick_walk +
# render_placeholder_preserved + ptr_render(preserve_placeholders = TRUE)
# code paths against a real translated tree.

# ---- scenario 3: NULL / absent companion value leaves current_pick NULL ---

test_that("absent companion-id key in snapshot leaves current_pick NULL", {
  n <- ppupload_node()
  stamped <- ggpaintr:::stamp_current_pick_walk(n, list())
  expect_null(stamped$current_pick)
})

# ---- scenario 4: companion-less source still uses node$id path ------------

test_that("companion-less data-source node still reads from snapshot[[node$id]]", {
  # Simulate a fabricated registry entry with shortcut = FALSE: the node
  # has shortcut_id = NULL, so the stamp pass falls back to node$id.
  n <- ggpaintr:::ptr_ph_data_source(
    id = "src1",
    keyword = "ppTestSrc",
    expr = quote(ppTestSrc()),
    shortcut_id = NULL
  )
  snap <- list(src1 = "iris")
  stamped <- ggpaintr:::stamp_current_pick_walk(n, snap)
  expect_equal(stamped$current_pick, "iris")
})

# Scenarios 5/6/7 absorbed by test-adr10-ppupload-name-browser.R preserve-mode
# unit tests (see header note at top of file).
