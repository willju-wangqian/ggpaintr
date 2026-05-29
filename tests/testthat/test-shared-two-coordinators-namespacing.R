# Two `ptr_shared(..., id = ...)` coordinators on one page sharing a
# panel-owned source key must each render their inner widget at the
# coordinator-namespaced DOM id, not at the bare `shared_<key>`. Pinned by
# ADR 0025 "Worked example #2".
#
# Pre-fix: `ptr_setup_panel_sources()` copied `rendered_node <- node` but
# never stamped the namespaced id onto the copy, so both coordinators'
# rendered fileInput tags carried bare `id="shared_<key>"`. Two such
# elements on one page collide — Shiny binds one, the other is dead.
#
# Two formulas per coordinator are required: a single-formula coordinator
# treats the `shared = "ds"` key as formula-local (rendered by the
# per-instance `ptr_setup_source_uis()` path, which already stamped
# correctly). The bug only manifests when the key is panel-owned, which
# requires >=2 formulas sharing it.

source_render_html <- function(obj) {
  # Drive `ptr_shared_server()` under testServer and read the panel-source
  # `renderUI` output. The output slot id is `source_output_id(canonical)`
  # = `"shared_<key>_ui"`, namespaced by `shared_ns(obj)` = `<id>-`.
  out <- ""
  shiny::testServer(function(input, output, session) {
    ptr_shared_server(obj)
    session$flushReact()
    out <<- paste(
      as.character(session$output$`left-shared_ds_ui` %||% ""),
      collapse = "\n"
    )
  }, {})
  out
}

source_render_html_for <- function(obj, slot) {
  out <- ""
  shiny::testServer(function(input, output, session) {
    ptr_shared_server(obj)
    session$flushReact()
    out <<- paste(
      as.character(session$output[[slot]] %||% ""),
      collapse = "\n"
    )
  }, {})
  out
}

# ADR 0025 item #7: the shortcut textInput is no longer rendered inside the
# source uiOutput (`<id>-shared_<key>_ui`, which now holds ONLY the
# fileInput). It is emitted as a STATIC sibling by the panel body
# (`shared_panel_body_tag` -> `build_ui_for`). Assert shortcut ids against
# this panel-body HTML, fileInput ids against the uiOutput render above.
panel_body_html <- function(obj) {
  as.character(shared_panel_body_tag(obj, obj$panel_keys))
}

# Two-formula coordinator so `ppUpload(shared = "ds")` becomes a panel key.
plots_ds <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar('mpg'))) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(y = ppVar('hp')))  + geom_histogram()"
)

test_that("left coordinator renders panel-source widget at namespaced DOM id", {
  left <- ptr_shared(formulas = plots_ds, id = "left")
  expect_equal(left$panel_keys, "ds")
  html <- source_render_html_for(left, "left-shared_ds_ui")
  # Inner fileInput must carry the coordinator-namespaced id.
  expect_match(html, 'id="left-shared_ds"', fixed = TRUE)
  expect_match(html, 'id="left-shared_ds-label"', fixed = TRUE)
  expect_match(html, 'id="left-shared_ds_progress"', fixed = TRUE)
  expect_false(grepl('id="shared_ds"', html, fixed = TRUE))
  # Shortcut companion (autoname text input) lives in the panel body now
  # (static sibling, item #7), likewise coordinator-namespaced.
  body <- panel_body_html(left)
  expect_match(body, 'id="left-shared_ds_shortcut"', fixed = TRUE)
  expect_match(body, 'id="left-shared_ds_shortcut-label"', fixed = TRUE)
  # The bare shortcut id must NOT appear when a coordinator id is supplied.
  expect_false(grepl('id="shared_ds_shortcut"', body, fixed = TRUE))
})

test_that("right coordinator renders panel-source widget at distinct namespaced DOM id", {
  right <- ptr_shared(formulas = plots_ds, id = "right")
  html <- source_render_html_for(right, "right-shared_ds_ui")
  expect_match(html, 'id="right-shared_ds"', fixed = TRUE)
  expect_false(grepl('id="shared_ds"', html, fixed = TRUE))
  expect_false(grepl('id="left-shared_ds"', html, fixed = TRUE))
  # Shortcut companion (static sibling, item #7) in the panel body.
  body <- panel_body_html(right)
  expect_match(body, 'id="right-shared_ds_shortcut"', fixed = TRUE)
  expect_false(grepl('id="left-shared_ds_shortcut"', body, fixed = TRUE))
})

test_that("two coordinators sharing key 'ds' produce disjoint DOM ids (no collision)", {
  left  <- ptr_shared(formulas = plots_ds, id = "left")
  right <- ptr_shared(formulas = plots_ds, id = "right")
  html_left  <- source_render_html_for(left,  "left-shared_ds_ui")
  html_right <- source_render_html_for(right, "right-shared_ds_ui")
  # Each rendered separately — assert ids are distinct across the two.
  pull_ids <- function(h) regmatches(h, gregexpr('id="[^"]+"', h))[[1L]]
  ids_left  <- pull_ids(html_left)
  ids_right <- pull_ids(html_right)
  shared <- intersect(ids_left, ids_right)
  expect_equal(shared, character(0L),
               label = "no DOM-id collision between two coordinators")
  # Spot-check that distinct namespaced ids do exist.
  expect_true(any(grepl("^id=\"left-shared_ds",  ids_left)))
  expect_true(any(grepl("^id=\"right-shared_ds", ids_right)))
})

test_that("default `id = NULL` still renders panel-source widget at bare DOM id", {
  # Byte-for-byte preservation when no coordinator id is supplied:
  # `shared_ns(obj)` with `obj$id = NULL` is `identity`, so the rendered
  # widget keeps its bare ids exactly as it did before the namespacing
  # work landed. Single-coordinator apps must not regress.
  obj <- ptr_shared(formulas = plots_ds)
  expect_null(obj$id)
  html <- source_render_html_for(obj, "shared_ds_ui")
  expect_match(html, 'id="shared_ds"', fixed = TRUE)
  # Bare shortcut id (id = NULL coordinator) in the panel body, item #7.
  expect_match(panel_body_html(obj), 'id="shared_ds_shortcut"', fixed = TRUE)
})
