# PLAN-06 / ADR 0025 §6 — spec round-trip fallback to state$bound_names
# for source_companion rows. After a user uploads a file (no text in the
# shortcut textbox), the snapshot-write loop in `ptr_setup_runtime()`
# overwrites the empty `snapshot[[shortcut_id]]` with
# `state$bound_names[[source_id]]()` (the auto-name Plan 02 stamped).
# `state$spec()` then carries `<ns(shortcut_id)> = <auto-name>` so a
# boot-2 with `spec=` can seed the textbox and reproduce the session.
#
# Tested in-process via `shiny::testServer` (mirrors the closed-loop
# pattern in test-spec-roundtrip-closed-loop.R). The browser
# end-to-end is covered separately in
# test-spec-roundtrip-end-to-end.R.

# Drive a `ppUpload` source's `bound_names[[id]]` directly via a small
# reactive-write helper, so we don't need a file-picker child process.
# This isolates the snapshot-fallback logic (the unit under test) from
# the upstream upload/bind chain (covered by other suites).
write_bound_name <- function(state, key, name) {
  if (!key %in% names(state$bound_names)) {
    rlang::abort(sprintf("test setup: no bound_names slot for %s", key))
  }
  state$bound_names[[key]](name)
}

test_that("empty shortcut + bound_names => spec contains the auto-name", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  formula <- "ppUpload |> ggplot(aes(x = ppVar(mpg), y = ppVar(hp))) + geom_point()"

  captured <- list()
  server <- function(input, output, session) {
    st <- ggpaintr:::ptr_server_internal(input, output, session, formula,
                                         envir = e)
    session$userData$state <- st
  }
  shiny::testServer(server, {
    state <- session$userData$state
    # Find the source id (e.g. "ggplot_1_ppUpload_NA") and its
    # companion shortcut id from the input_spec — robust to formula-
    # stage renumbering.
    sd <- state$input_spec
    src_row <- which(!is.na(sd$role) & sd$role == "placeholder" &
                       !is.na(sd$keyword) & sd$keyword == "ppUpload")
    expect_length(src_row, 1L)
    src_id <- sd$source_id[src_row]
    cmp_row <- which(!is.na(sd$role) & sd$role == "source_companion" &
                       !is.na(sd$source_id) & sd$source_id == src_id)
    expect_length(cmp_row, 1L)
    shortcut_id <- sd$input_id[cmp_row]

    # Set ppVar picks; leave the shortcut textbox at its default empty
    # value (no `session$setInputs(<shortcut_id> = …)` call).
    x_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "x"][1L]
    y_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "y"][1L]
    args <- stats::setNames(list("mpg", "hp"), c(x_id, y_id))
    do.call(session$setInputs, args)

    # Simulate the auto-name bind that would happen on a real upload
    # (Plan 02's `resolve_upload_source` writes
    # `state$bound_names[[<key>]](node$auto_name)` after assigning to
    # eval_env). For bare-data ppUpload (`ppUpload |> ggplot(...)`),
    # the key in `state$bound_names` is the layer_name (e.g. "ggplot"),
    # not the source_id; the fallback in `ptr_setup_runtime()` tries
    # both keys. Pick the slot that exists, write the auto-name.
    auto_name_value <- "uploaded_df"
    bn_key <- if (src_id %in% names(state$bound_names)) src_id else
              if ("ggplot" %in% names(state$bound_names)) "ggplot" else
              names(state$bound_names)[1L]
    write_bound_name(state, bn_key, auto_name_value)

    session$setInputs(ptr_update_plot = 1L)
    session$flushReact()

    spec <- state$spec()
    ns_shortcut <- state$server_ns_fn(shortcut_id)
    captured$spec <<- spec
    captured$ns_shortcut <<- ns_shortcut

    # CORE ASSERTION (Scenario "upload with empty textbox dumps the
    # auto-name into spec"): the spec carries the auto-name at the
    # namespaced shortcut id.
    expect_true(
      ns_shortcut %in% names(spec),
      label = sprintf("spec contains entry for %s", ns_shortcut)
    )
    expect_equal(
      spec[[ns_shortcut]], auto_name_value,
      label = "spec value at the shortcut id equals the bound auto-name"
    )
  })
})

test_that("typed shortcut wins over bound_names fallback", {
  e <- list2env(list(mtcars = mtcars), parent = globalenv())
  formula <- "ppUpload |> ggplot(aes(x = ppVar(mpg), y = ppVar(hp))) + geom_point()"

  server <- function(input, output, session) {
    session$userData$state <- ggpaintr:::ptr_server_internal(
      input, output, session, formula, envir = e
    )
  }
  shiny::testServer(server, {
    state <- session$userData$state
    sd <- state$input_spec
    src_id <- sd$source_id[!is.na(sd$role) &
                             sd$role == "source_companion"][1L]
    shortcut_id <- sd$input_id[!is.na(sd$role) &
                                 sd$role == "source_companion"][1L]
    x_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "x"][1L]
    y_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "y"][1L]

    # Both: typed shortcut AND a prior upload's bound name. The
    # bound_names key for this bare-data ppUpload is the layer_name
    # ("ggplot"), not the source_id (see test above for details).
    args <- stats::setNames(
      list("mtcars", "mpg", "hp", 1L),
      c(shortcut_id, x_id, y_id, "ptr_update_plot")
    )
    do.call(session$setInputs, args)
    bn_key <- if (src_id %in% names(state$bound_names)) src_id else
              if ("ggplot" %in% names(state$bound_names)) "ggplot" else
              names(state$bound_names)[1L]
    write_bound_name(state, bn_key, "leftover_auto_name")
    session$flushReact()

    spec <- state$spec()
    ns_shortcut <- state$server_ns_fn(shortcut_id)
    expect_equal(
      spec[[ns_shortcut]], "mtcars",
      label = "typed textbox wins (NOT 'leftover_auto_name')"
    )
  })
})
