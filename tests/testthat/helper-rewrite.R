# Helpers for the rewrite test suite (test-rewrite-*.R).
#
# `find_nodes` was promoted to the package (R/paintr-build-ui.R) so observers
# inside ptr_server_internal can call it. Tests use the exported (or unexported via
# devtools::load_all) version directly — no helper redefinition needed.
#
# `build_ui_for.{ptr_ph_value, ptr_ph_data_source}` now return `uiOutput`
# containers (ADR 0012 / PLAN-01 -- Bug B); the actual widget tag is composed
# inside the server-side renderUI bodies in `ptr_setup_value_uis()` /
# `ptr_setup_source_uis()`. Tests that assert on the widget tag bypass
# `build_ui_for` and call the composition path directly:
#   - `.value_widget`  -- the same path `ptr_setup_value_uis()` uses
#                        (`invoke_build_ui`) so copy / ns / `node$default`
#                        precedence all run as in production.
#   - `.source_widget` -- the same composition `ptr_setup_source_uis()` uses
#                        (no `invoke_build_ui` helper exists for source
#                        nodes; the renderUI body assembles the widget
#                        directly, so this helper mirrors it).
.value_widget <- function(node, layer_name = NULL, ui_text = NULL,
                          ns_fn = identity, extra = list()) {
  invoke_build_ui(node, ui_text = ui_text, layer_name = layer_name,
                  ns_fn = ns_fn, extra = extra)
}
# Drive `ptr_server_internal` under `shiny::testServer` and return the HTML
# rendered into `output$<raw_id>_ui` by `ptr_setup_value_uis` /
# `ptr_setup_source_uis` after the first flush. This is the post-PLAN-01
# replacement for "build a layer panel statically and assert on its HTML"
# when the assertion targets widget-level copy (label / placeholder / help)
# that's now composed inside the renderUI body, not the static container.
#
# `raw_id` is the placeholder's bare id (no namespace, no `_ui` suffix);
# this function appends `_ui` to look up the renderUI slot.
.render_widget_html <- function(formula, raw_id, ui_text = NULL,
                                spec = NULL, envir = NULL) {
  env <- envir %||% list2env(list(mtcars = mtcars), parent = globalenv())
  server <- function(input, output, session) {
    args <- list(input, output, session, formula, envir = env)
    if (!is.null(ui_text)) args$ui_text <- ui_text
    if (!is.null(spec)) args$spec <- spec
    do.call(ptr_server_internal, args)
  }
  out <- NULL
  shiny::testServer(server, {
    session$flushReact()
    out <<- session$getOutput(paste0(raw_id, "_ui"))
  })
  paste(as.character(out), collapse = "\n")
}

.source_widget <- function(node, ui_text = NULL, ns_fn = identity) {
  copy <- ptr_resolve_ui_text(
    "control", keyword = node$keyword, param = node$param,
    layer_name = node$layer_name, ui_text = ui_text
  )
  entry <- ptr_registry_lookup(node$keyword)
  fmls <- names(formals(entry$build_ui))
  accepts_dots <- "..." %in% fmls
  extra_named <- build_ui_copy_args(fmls, copy)
  if (identical(node$keyword, "ppUpload") &&
      (accepts_dots || "file_copy" %in% fmls)) {
    extra_named$file_copy <- ptr_resolve_ui_text("upload_file", ui_text = ui_text)
    extra_named$name_copy <- ptr_resolve_ui_text("upload_name", ui_text = ui_text)
  }
  if (accepts_dots || "named_args" %in% fmls) {
    extra_named$named_args <- node$named_args %||% list()
  }
  rendered_node <- node
  rendered_node$id <- ns_fn(node$id)
  if (!is.null(node$shortcut_id)) {
    rendered_node$shortcut_id <- ns_fn(node$shortcut_id)
  }
  do.call(entry$build_ui, c(list(rendered_node, label = copy$label), extra_named))
}
