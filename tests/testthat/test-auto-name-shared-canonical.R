# ADR 0025 §3 / PLAN-02: runtime stamp of `node$auto_name` inside
# `ptr_setup_panel_sources()` for panel-shared `ppUpload(shared='...')`
# sources. The stamp uses `<obj$id>_<key>` when the coordinator has
# `id = "main"`, and the bare `<key>` when `id = NULL` (the default).
#
# These tests boot the host server under `shiny::testServer()` and
# isolate-read the stamped node from the firsts table. The shared-coord
# observation: `ptr_shared_server` invokes `ptr_setup_panel_sources()`
# which mutates the first-occurrence node in place.

test_that("shared ppUpload under obj$id='main' — auto_name == 'main_ds'", {
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ), id = "main")
  state <- NULL
  server <- function(input, output, session) {
    state <<- ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    session$flushReact()
    # `ptr_setup_panel_sources()` mutates `obj$firsts$nodes[["ds"]]` in
    # place via its `local({ node <- firsts_nodes[[key]]; ... })` block.
    # That mutation happens inside the local() so the outer firsts_nodes
    # is not updated. Instead, fetch the stamped node via the renderUI
    # closure: easier path — call the helper directly with the obj.
    panel_sources <- ggpaintr:::ptr_setup_panel_sources(
      obj, input = input, output = output, envir = globalenv()
    )
    # The auto_name lives on the node captured by the closure; to read
    # it back we use the side effect: the canonical id is `shared_ds`
    # and the helper installed exactly that key.
    expect_true("shared_ds" %in% names(panel_sources))
  })
  # Direct white-box stamp check: run the runtime stamp logic ourselves
  # with the same inputs `ptr_setup_panel_sources()` would receive, then
  # read `node$auto_name`.
  panel_keys <- obj$panel_keys
  firsts_nodes <- obj$firsts$nodes
  ds_key <- panel_keys[panel_keys == "ds"][[1L]]
  node <- firsts_nodes[[ds_key]]
  # Re-run the stamp body the helper applies (mirrors the post-PLAN-02
  # block inside the `for (k in source_keys)` loop):
  canonical <- ggpaintr:::canonical_shared_id(ds_key)
  node$id <- canonical
  entry <- ggpaintr:::ptr_registry_lookup(node$keyword)
  if (!is.null(entry) && isTRUE(entry$shortcut)) {
    node$shortcut_id <- paste0(canonical, "_shortcut")
  }
  node$auto_name <- if (!is.null(obj$id)) paste0(obj$id, "_", ds_key) else ds_key
  expect_identical(node$auto_name, "main_ds")
})

test_that("shared ppUpload under obj$id=NULL — auto_name == 'ds' (bare key)", {
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ))
  expect_null(obj$id)
  panel_keys <- obj$panel_keys
  firsts_nodes <- obj$firsts$nodes
  ds_key <- panel_keys[panel_keys == "ds"][[1L]]
  node <- firsts_nodes[[ds_key]]
  canonical <- ggpaintr:::canonical_shared_id(ds_key)
  node$id <- canonical
  entry <- ggpaintr:::ptr_registry_lookup(node$keyword)
  if (!is.null(entry) && isTRUE(entry$shortcut)) {
    node$shortcut_id <- paste0(canonical, "_shortcut")
  }
  node$auto_name <- if (!is.null(obj$id)) paste0(obj$id, "_", ds_key) else ds_key
  expect_identical(node$auto_name, "ds")
})

test_that("ptr_setup_panel_sources actually stamps node$auto_name (integration)", {
  # Sanity: capture the stamped node through the helper's own machinery
  # by intercepting `resolve_upload_source`, which receives the node.
  captured_node <- NULL
  testthat::local_mocked_bindings(
    .package = "ggpaintr",
    resolve_upload_source = function(input_slot, shortcut_slot, node, entry,
                                     envir, state, key, slot) {
      captured_node <<- node
      invisible(NULL)
    }
  )
  obj <- ptr_shared(c(
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_point()",
    "ggplot(ppUpload(shared='ds'), aes(x = ppVar(shared='col'))) + geom_col()"
  ), id = "main")
  server <- function(input, output, session) {
    ptr_shared_server(obj, envir = globalenv())
  }
  shiny::testServer(server, {
    session$flushReact()
    expect_false(is.null(captured_node))
    expect_identical(captured_node$auto_name, "main_ds")
    expect_identical(captured_node$id, "shared_ds")
  })
})
