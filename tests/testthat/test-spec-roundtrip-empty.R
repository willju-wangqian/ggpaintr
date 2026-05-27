# PLAN-06 / ADR 0025 §6 — when there is no upload AND no typed shortcut
# AND no bound name, the spec round-trip stays as-is (the source-
# companion shortcut entry is either "" — which the sparse-diff drops —
# OR absent). The fallback in `ptr_setup_runtime()` is gated on a
# non-NULL `state$bound_names[[key]]()`; without an upload, that read
# returns NULL and the snapshot value stays at the empty default.

test_that("no upload + empty textbox => no shortcut entry in spec (or '')", {
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
    shortcut_id <- sd$input_id[!is.na(sd$role) &
                                 sd$role == "source_companion"][1L]
    x_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "x"][1L]
    y_id <- sd$input_id[!is.na(sd$keyword) & sd$keyword == "ppVar" &
                          !is.na(sd$param_key) & sd$param_key == "y"][1L]

    # Drive only the ppVar pickers + click Update. Leave the shortcut
    # textbox at default "" and do NOT write any bound_names entry.
    args <- stats::setNames(
      list("mpg", "hp", 1L),
      c(x_id, y_id, "ptr_update_plot")
    )
    do.call(session$setInputs, args)
    session$flushReact()

    spec <- state$spec()
    ns_shortcut <- state$server_ns_fn(shortcut_id)

    # Either the entry is absent (dropped by the sparse diff because the
    # value matches the default), OR present with empty-string "".
    if (ns_shortcut %in% names(spec)) {
      expect_equal(
        spec[[ns_shortcut]], "",
        label = "if present, the shortcut entry equals empty string"
      )
    } else {
      expect_true(
        !(ns_shortcut %in% names(spec)),
        label = "shortcut entry dropped by sparse diff"
      )
    }
  })
})
