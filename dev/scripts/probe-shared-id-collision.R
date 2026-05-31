#!/usr/bin/env Rscript
# Probe: DOM-id collision in ptr_setup_panel_sources()
#
# Two ptr_shared(..., id = "left" | "right") coordinators on one page,
# both with a panel-owned ppUpload source. Per handoff §3, both modules'
# rendered fileInput widgets bind to bare id="shared_upload" instead of
# the namespaced "left-shared_upload" / "right-shared_upload". This probe
# renders the host UI + the panel renderUI output and dumps the resulting
# HTML so we can grep the collision directly.
#
# Run from worktree root:
#   Rscript dev/scripts/probe-shared-id-collision.R
suppressMessages(devtools::load_all(".", quiet = TRUE))

# Two formulas with a panel-owned ppUpload key "ds". Each gets its own
# coordinator with its own id.
f <- "ppUpload(shared = 'ds') |> ggplot(aes(x = ppVar('mpg'))) + geom_point()"

left  <- ptr_shared(formulas = f, id = "left")
right <- ptr_shared(formulas = f, id = "right")

# === 1. Static-panel UI: does ptr_shared_panel(obj) emit collisions? ====
left_panel_html  <- paste(as.character(htmltools::renderTags(ptr_shared_panel(left ))$html), collapse = "\n")
right_panel_html <- paste(as.character(htmltools::renderTags(ptr_shared_panel(right))$html), collapse = "\n")

cat("\n=== STATIC PANEL HTML (left) — relevant ids ===\n")
ids_left  <- regmatches(left_panel_html,  gregexpr("id=\"[^\"]+\"",  left_panel_html))[[1]]
cat(paste(ids_left, collapse = "\n"), "\n")

cat("\n=== STATIC PANEL HTML (right) — relevant ids ===\n")
ids_right <- regmatches(right_panel_html, gregexpr("id=\"[^\"]+\"", right_panel_html))[[1]]
cat(paste(ids_right, collapse = "\n"), "\n")

# === 2. testServer drive ptr_shared_server() to fire the renderUI ======
# The panel-source renderUI is mounted inside ptr_setup_panel_sources()
# (R/paintr-shared-ui.R ~line 532). To trigger it under testServer we need
# to observe the output. shiny::testServer renders the moduleServer body
# but lazy outputs only realize on request. We call observe -> output$xxx
# explicitly via session$getOutput. Simpler: capture the HTML by inspecting
# `output$<id>` via the recorded reactive log after the renderUI fires.
#
# Actually the cleanest route is to construct the rendered tag manually by
# walking what ptr_setup_panel_sources() does. But the bug is in the call
# site itself, so we want the real renderUI. Let's just probe via
# shinytest2-style: spin up shiny::shinyApp inside this process and use
# shiny::runApp(test.mode=TRUE) with an httpuv probe. Too heavy.
#
# Fastest faithful path: directly invoke the buggy code path by replicating
# what the closure does at line 532-568. The relevant inputs are the
# `node`, `entry`, `ns`, `ui_text`. The output `entry$build_ui(rendered_node,
# ...)` is the rendered widget tag. If we synthesise the node the way
# `ptr_setup_panel_sources()` synthesises it, we get the same bug.

cat("\n=== Direct reconstruction of the buggy build_ui call ===\n")
inspect_path <- function(coord_id) {
  obj <- ptr_shared(formulas = f, id = coord_id)
  key <- "ds"
  node <- obj$firsts$nodes[[key]]
  # Mirror lines 511-515 of R/paintr-shared-ui.R verbatim:
  canonical <- ggpaintr:::canonical_shared_id(key)
  node$id <- canonical                                # ← BUG: bare
  entry <- ggpaintr:::ptr_registry_lookup(node$keyword)
  if (!is.null(entry) && isTRUE(entry$shortcut)) {
    node$shortcut_id <- paste0(canonical, "_shortcut")  # ← BUG: bare
  }
  ns <- shiny::NS(coord_id)
  # Mirror line 533: rendered_node <- node (no stamp!)
  rendered_node <- node
  # Mirror lines 566-567:
  copy <- list(label = "Upload")
  fmls <- names(formals(entry$build_ui))
  extra_named <- list()
  if ("file_copy" %in% fmls) extra_named$file_copy <- list(label = "Choose file", button_label = "Browse...", placeholder = "No file selected")
  if ("name_copy" %in% fmls) extra_named$name_copy <- list(label = "Or use existing R object", placeholder = "object name")
  if ("named_args" %in% fmls) extra_named$named_args <- list()
  tag <- do.call(entry$build_ui, c(list(rendered_node, label = copy$label), extra_named))
  html <- paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")
  ids <- regmatches(html, gregexpr("id=\"[^\"]+\"", html))[[1]]
  list(html = html, ids = ids, rendered_node = rendered_node)
}

L <- inspect_path("left")
R <- inspect_path("right")

cat("LEFT widget ids:  ", paste(L$ids, collapse = " | "), "\n")
cat("RIGHT widget ids: ", paste(R$ids, collapse = " | "), "\n")

cat("\n=== Collision detection ===\n")
shared <- intersect(L$ids, R$ids)
if (length(shared) > 0L) {
  cat("BUG CONFIRMED — duplicate ids across coordinators:\n",
      paste(shared, collapse = "\n  "), "\n")
} else {
  cat("No duplicate ids — bug NOT reproduced under this path.\n")
}

# Show what the fix (stamp rendered_node$id with ns(node$id)) would produce:
cat("\n=== Counter-factual: with Option A stamp applied ===\n")
inspect_fixed <- function(coord_id) {
  obj <- ptr_shared(formulas = f, id = coord_id)
  key <- "ds"
  node <- obj$firsts$nodes[[key]]
  canonical <- ggpaintr:::canonical_shared_id(key)
  node$id <- canonical
  entry <- ggpaintr:::ptr_registry_lookup(node$keyword)
  if (!is.null(entry) && isTRUE(entry$shortcut)) {
    node$shortcut_id <- paste0(canonical, "_shortcut")
  }
  ns <- shiny::NS(coord_id)
  rendered_node <- node
  rendered_node$id <- ns(node$id)                              # ← FIX
  if (!is.null(node$shortcut_id)) {
    rendered_node$shortcut_id <- ns(node$shortcut_id)          # ← FIX
  }
  copy <- list(label = "Upload")
  fmls <- names(formals(entry$build_ui))
  extra_named <- list()
  if ("file_copy" %in% fmls) extra_named$file_copy <- list(label = "Choose file", button_label = "Browse...", placeholder = "No file selected")
  if ("name_copy" %in% fmls) extra_named$name_copy <- list(label = "Or use existing R object", placeholder = "object name")
  if ("named_args" %in% fmls) extra_named$named_args <- list()
  tag <- do.call(entry$build_ui, c(list(rendered_node, label = copy$label), extra_named))
  html <- paste(as.character(htmltools::renderTags(tag)$html), collapse = "\n")
  ids <- regmatches(html, gregexpr("id=\"[^\"]+\"", html))[[1]]
  list(html = html, ids = ids)
}

Lf <- inspect_fixed("left")
Rf <- inspect_fixed("right")
cat("LEFT (fixed):  ", paste(Lf$ids, collapse = " | "), "\n")
cat("RIGHT (fixed): ", paste(Rf$ids, collapse = " | "), "\n")
sharedf <- intersect(Lf$ids, Rf$ids)
cat("Collisions after fix: ", length(sharedf), "\n")
