# PLAN-02 (ADR 0010) — stamp_current_pick_walk.ptr_ph_data_source reads the
# companion-id snapshot for ppUpload preserve-mode round-trip.
#
# The fileInput value at `snapshot[[node$id]]` is a four-column data.frame and
# is not the meaningful "current pick" for upload-style data-source
# placeholders. The user-typed display name lives at
# `snapshot[[node$companion_id]]`. PLAN-02 rewires the stamp method to read
# the companion-id snapshot when present, while preserving the legacy
# node$id path for companion-less data-source registry entries.

# ---- helpers --------------------------------------------------------------

# Synthetic ppUpload data-source node with explicit companion_id, bypassing
# the registry so these tests stay order-independent w.r.t. PLAN-01.
ppupload_node <- function(id = "up1", companion_id = "up1_name") {
  ggpaintr:::ptr_ph_data_source(
    id = id,
    keyword = "ppUpload",
    expr = quote(ppUpload()),
    companion_id = companion_id
  )
}

# ---- scenario 1: companion-id snapshot populates current_pick -------------

test_that("companion-id snapshot value populates current_pick on ppUpload node", {
  n <- ppupload_node()
  snap <- list(up1_name = "penguins", up1 = NULL)
  stamped <- ggpaintr:::stamp_current_pick_walk(n, snap)
  expect_equal(stamped$current_pick, "penguins")
})

# ---- scenario 2: empty-string companion value leaves current_pick NULL ----

test_that("empty-string companion-id value leaves current_pick NULL", {
  n <- ppupload_node()
  snap <- list(up1_name = "")
  stamped <- ggpaintr:::stamp_current_pick_walk(n, snap)
  # The existing .snapshot_value_is_set guard treats "" as not-set, so the
  # stamp pass leaves current_pick untouched (NULL on a fresh node).
  expect_null(stamped$current_pick)
})

# ---- scenario 3: NULL / absent companion value leaves current_pick NULL ---

test_that("absent companion-id key in snapshot leaves current_pick NULL", {
  n <- ppupload_node()
  stamped <- ggpaintr:::stamp_current_pick_walk(n, list())
  expect_null(stamped$current_pick)
})

# ---- scenario 4: companion-less source still uses node$id path ------------

test_that("companion-less data-source node still reads from snapshot[[node$id]]", {
  # Simulate a fabricated registry entry without companion_id_fn: the node
  # has companion_id = NULL, so the stamp pass falls back to node$id.
  n <- ggpaintr:::ptr_ph_data_source(
    id = "src1",
    keyword = "ppTestSrc",
    expr = quote(ppTestSrc()),
    companion_id = NULL
  )
  snap <- list(src1 = "iris")
  stamped <- ggpaintr:::stamp_current_pick_walk(n, snap)
  expect_equal(stamped$current_pick, "iris")
})

# ---- scenario 5: render_placeholder_preserved emits ppUpload(penguins) ----

test_that("render_placeholder_preserved emits ppUpload(penguins) bareword", {
  n <- ppupload_node()
  n$current_pick <- "penguins"
  out <- ggpaintr:::render_placeholder_preserved(n)
  expect_equal(out, "ppUpload(penguins)")
})

# ---- scenario 6: render_placeholder_preserved emits bare ppUpload when no pick
# (ADR 0012 follow-up: vignette-review commit 8235d8a drops empty parens for
# placeholders with no current_pick and no shared key — preserve-mode emits
# the bare symbol so callers / round-trip render see a stable identifier.)

test_that("render_placeholder_preserved emits bare ppUpload when current_pick is NULL", {
  n <- ppupload_node()
  # current_pick is unset (NULL) by default on a freshly constructed node.
  out <- ggpaintr:::render_placeholder_preserved(n)
  expect_equal(out, "ppUpload")
})

# ---- scenario 7: end-to-end round-trip through translate + render ---------

test_that("end-to-end translate + stamp + ptr_render preserves ppUpload(penguins)", {
  # Use ptr_translate -> ptr_assign_ids so the ppUpload node gets a real id
  # and the registry-driven companion_id (id %s "_name"). Then stamp from a
  # snapshot that maps that companion_id to "penguins", and render in
  # preserve mode. The rendered text must contain ppUpload(penguins) and
  # not the bare ppUpload().
  r <- ggpaintr:::ptr_translate(
    "ppUpload() |> ggplot(aes(x = ppVar)) + geom_point()"
  )
  r <- ggpaintr:::ptr_assign_ids(r)

  # Locate the ppUpload source node to read its assigned ids.
  src_nodes <- ggpaintr:::find_nodes(r, ggpaintr:::is_ptr_ph_data_source)
  expect_true(length(src_nodes) >= 1L)
  src <- src_nodes[[1L]]
  expect_equal(src$keyword, "ppUpload")
  expect_true(!is.null(src$companion_id) && nzchar(src$companion_id))

  # Build a snapshot keyed by the assigned companion_id and by the consumer's
  # node$id, so both placeholders get a current_pick.
  consumer_nodes <- ggpaintr:::find_nodes(r, ggpaintr:::is_ptr_ph_data_consumer)
  expect_true(length(consumer_nodes) >= 1L)
  consumer <- consumer_nodes[[1L]]

  snap <- stats::setNames(
    list("penguins", "mpg"),
    c(src$companion_id, consumer$id)
  )

  stamped <- ggpaintr:::stamp_current_pick_walk(r, snap)
  # ptr_render is internal (not exported in NAMESPACE), so use `:::`.
  out <- ggpaintr:::ptr_render(stamped, preserve_placeholders = TRUE)

  expect_true(grepl("ppUpload(penguins)", out, fixed = TRUE))
  # The empty-form ppUpload() must not appear when a current_pick is set.
  expect_false(grepl("ppUpload()", out, fixed = TRUE))
})
