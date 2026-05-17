# Shared hygiene for shinytest2 (AppDriver) tests.
#
# `ptr_user_css_assets()` registers each user stylesheet's parent dir via
# `shiny::addResourcePath("ggpaintr-user-<hash>", dir)` and never removes
# it (correct for a real, long-lived app). Under the full test suite a
# `css =` test registers such a prefix into a `withr` tempdir, the tempdir
# is later torn down at scope exit, but the *process-global* resource-path
# registration lingers. `shinytest2::AppDriver$new()` snapshots the parent
# process's `shiny::resourcePaths()` and replays every entry into the app
# subprocess at startup; a replayed entry whose directory no longer exists
# makes `addResourcePath()` abort ("Couldn't normalize path"), so the app
# fails to boot and the test errors -- but only when an earlier css test
# ran first, i.e. order-dependently in the whole suite.
#
# Dropping a resource path whose directory is gone is semantically a
# no-op (it can serve nothing), so pruning the dead `ggpaintr-user-*`
# entries before booting an AppDriver is a safe, targeted isolation fix
# that touches no product behaviour.
prune_dead_ggpaintr_resource_paths <- function() {
  rp <- shiny::resourcePaths()
  for (prefix in names(rp)) {
    if (startsWith(prefix, "ggpaintr-user-") && !dir.exists(rp[[prefix]])) {
      shiny::removeResourcePath(prefix)
    }
  }
  invisible(NULL)
}
