# Boot scaffolding (NOT vignette code). Fixture for Cut 1 of the
# shared-source end-to-end fix.
#
# Bug 2 reproducer: a single-instance app with `ppUpload(shared='ds')` —
# the upload widget never rendered because the per-instance source-UI
# setup `ptr_setup_source_uis()` (R/paintr-server.R) had a blanket
# partition-blind skip on any node carrying `s$shared`. After Cut 1
# (drop the skip), the rewritten source node with canonical id
# `shared_ds` flows through the normal per-instance renderUI path and
# the fileInput appears in the DOM.
pkgload::load_all(Sys.getenv("GGP_PKG"), quiet = TRUE, helpers = FALSE, attach_testthat = FALSE)
library(shiny)

ptr_app(
  ppUpload(shared = "ds") |>
    ggplot(aes(x = ppVar(shared = "a"), y = ppVar(shared = "a"))) +
    geom_point()
)
