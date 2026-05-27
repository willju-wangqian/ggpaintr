# PLAN-06 / ADR 0025 §6 — panel-shared sources also round-trip via
# `state$bound_names`. The per-instance state for a panel formula has
# `bound_names[[shared_<key>]]` populated (`bind_source_value()` writes
# the auto-name `"<coord_id>_<key>"` post-PLAN-02). The snapshot-fallback
# in `ptr_setup_runtime()` therefore produces a spec entry at the
# panel-namespaced shortcut id equal to that auto-name.

write_bound_name <- function(state, key, name) {
  if (!key %in% names(state$bound_names)) {
    rlang::abort(sprintf("test setup: no bound_names slot for %s", key))
  }
  state$bound_names[[key]](name)
}

test_that("panel-shared source: spec value at the shortcut id equals the auto-name", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  formula <- paste(
    "ggplot(ppUpload(shared = 'ds'),",
    "aes(x = ppVar(mpg), y = ppVar(hp))) + geom_point()"
  )

  server <- function(input, output, session) {
    session$userData$state <- ggpaintr:::ptr_server_internal(
      input, output, session, formula, envir = e,
      auto_bind_shared = TRUE
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    sd <- state$input_spec
    cmp_row <- which(!is.na(sd$role) & sd$role == "source_companion")
    expect_length(cmp_row, 1L)
    shortcut_id <- sd$input_id[cmp_row]
    src_id <- sd$source_id[cmp_row]

    # Picks for the two ppVar consumers; leave shortcut empty.
    x_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "x"][1L]
    y_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "y"][1L]
    args <- stats::setNames(
      list("mpg", "hp"), c(x_id, y_id)
    )
    do.call(session$setInputs, args)

    # `auto_bind_shared = TRUE` primes the shared source to `mtcars`
    # (node$default), so `state$bound_names[[<key>]]` is populated at
    # boot via `try_bind_source_default()` → `bind_source_value()`.
    # The bound_names key for a bare-data panel-shared ppUpload is the
    # layer_name ("ggplot"); the fallback in `ptr_setup_runtime()`
    # checks both source_id and layer_name. Write the auto-name
    # directly under whichever slot exists so the test is independent
    # of the upload binding chain.
    bn_key <- if (src_id %in% names(state$bound_names)) src_id else
              if ("ggplot" %in% names(state$bound_names)) "ggplot" else
              names(state$bound_names)[1L]
    write_bound_name(state, bn_key, "shared_ds_autoname")

    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()

    spec <- state$spec()
    ns_shortcut <- state$server_ns_fn(shortcut_id)

    expect_true(
      ns_shortcut %in% names(spec),
      label = sprintf("spec contains entry for %s", ns_shortcut)
    )
    expect_equal(
      spec[[ns_shortcut]], "shared_ds_autoname",
      label = "spec at the panel-shared shortcut id equals the bound auto-name"
    )
  })
})
