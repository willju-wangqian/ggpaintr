pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE,
                  attach_testthat = FALSE)
library(shiny)

typed_ds <- data.frame(
  grp               = rep(c("a", "b"), 10),
  body_mass_g       = rnorm(20, 4000, 500),
  flipper_length_mm = rnorm(20, 200, 10)
)

.orig_cus <- ggpaintr:::consumer_upstream_source_state
assignInNamespace("consumer_upstream_source_state",
  function(src_nodes, read_input, state, layer_name = NULL) {
    res <- .orig_cus(src_nodes, read_input, state, layer_name = layer_name)
    ids <- vapply(src_nodes, function(n) n$id %||% "<noid>", character(1))
    reads <- vapply(ids, function(id) {
      v <- tryCatch(read_input(id), error = function(e) NULL)
      dp <- if (is.list(v) && !is.null(v$datapath)) paste(v$datapath, collapse="|") else ""
      paste0(id, "=>dp:[", dp, "]")
    }, character(1))
    message("PROBE_CUS state_null=", is.null(state), " layer=", layer_name %||% "<null>",
            " node_ids=[", paste(ids, collapse=","), "] reads=[", paste(reads, collapse=" "),
            "] identity=[", res$identity, "] user_supplied=", res$user_supplied,
            " uploaded=", res$uploaded)
    res
  }, ns = "ggpaintr")

.orig_clear <- ggpaintr:::consumer_clear_for_new_source
assignInNamespace("consumer_clear_for_new_source",
  function(src_seen, identity, last_identity, user_supplied, uploaded, seed) {
    out <- .orig_clear(src_seen, identity, last_identity, user_supplied, uploaded, seed)
    message("PROBE_CLEAR src_seen=", src_seen, " changed=", !identical(identity, last_identity),
            " identity=[", identity, "] last=[", last_identity %||% "<null>", "] => CLEAR=", out)
    out
  }, ns = "ggpaintr")

ptr_app("ggplot(ppUpload(), aes(x = ppVar(), y = body_mass_g)) + geom_point()")
