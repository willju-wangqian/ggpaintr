# Probe app for ppUpload #7 follow-up: host-scope shared consumer clear.
# Boots dev source, defines an env dataset `typed_ds` (so the env-shortcut
# resolves), and instruments two ggpaintr internals to message() their state
# into the child stderr (captured by AppDriver$get_logs()).
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE,
                  attach_testthat = FALSE)
library(shiny)

# columns chosen to overlap penguins.csv on body_mass_g + flipper_length_mm,
# and to be unique on `grp`.
typed_ds <- data.frame(
  grp               = rep(c("a", "b"), 10),
  body_mass_g       = rnorm(20, 4000, 500),
  flipper_length_mm = rnorm(20, 200, 10)
)

# ---- instrumentation (child-process; written into app.R per serena rule) ----
.orig_cus <- ggpaintr:::consumer_upstream_source_state
.wrap_cus <- function(src_nodes, read_input, state, layer_name = NULL) {
  res <- .orig_cus(src_nodes, read_input, state, layer_name = layer_name)
  ids <- vapply(src_nodes, function(n) n$id %||% "<noid>", character(1))
  reads <- vapply(ids, function(id) {
    v <- tryCatch(read_input(id), error = function(e) NULL)
    dp <- if (is.list(v) && !is.null(v$datapath)) paste(v$datapath, collapse="|") else ""
    paste0(id, "=>dp:[", dp, "]")
  }, character(1))
  message("PROBE_CUS state_null=", is.null(state),
          " layer=", layer_name %||% "<null>",
          " node_ids=[", paste(ids, collapse=","), "]",
          " reads=[", paste(reads, collapse=" "), "]",
          " identity=[", res$identity, "]",
          " user_supplied=", res$user_supplied,
          " uploaded=", res$uploaded)
  res
}
assignInNamespace("consumer_upstream_source_state", .wrap_cus, ns = "ggpaintr")

.orig_clear <- ggpaintr:::consumer_clear_for_new_source
.wrap_clear <- function(src_seen, identity, last_identity, user_supplied,
                        uploaded, seed) {
  out <- .orig_clear(src_seen, identity, last_identity, user_supplied,
                     uploaded, seed)
  message("PROBE_CLEAR src_seen=", src_seen,
          " changed=", !identical(identity, last_identity),
          " identity=[", identity, "] last=[", last_identity %||% "<null>", "]",
          " user_supplied=", user_supplied, " uploaded=", uploaded,
          " seed_null=", is.null(seed), " => CLEAR=", out)
  out
}
assignInNamespace("consumer_clear_for_new_source", .wrap_clear, ns = "ggpaintr")
# -----------------------------------------------------------------------------

plots <- c(
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = body_mass_g)) + geom_point()",
  "ggplot(ppUpload(shared = 'ds'), aes(x = ppVar(shared = 'col'), y = flipper_length_mm)) + geom_point()"
)
panel <- ptr_shared(plots)

ui <- fluidPage(
  ptr_shared_panel(panel),
  fluidRow(
    column(6, ptr_ui(panel$formulas[[1L]], "p1", shared = panel)),
    column(6, ptr_ui(panel$formulas[[2L]], "p2", shared = panel))
  )
)
server <- function(input, output, session) {
  s <- ptr_shared_server(panel)
  ptr_server(panel$formulas[[1L]], "p1", shared_state = s)
  ptr_server(panel$formulas[[2L]], "p2", shared_state = s)
}
shinyApp(ui, server)
