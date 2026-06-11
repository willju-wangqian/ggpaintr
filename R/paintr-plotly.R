# paintr-plotly.R -------------------------------------------------------------
# Thin, opt-in plotly interop helpers on top of the locked L3 model (ADR 0006,
# ADR 0028). Custom render extracts from `state`; core learns nothing about
# plotly or selection. plotly stays in Suggests (DESCRIPTION) with a call-time
# guard, mirroring the optional-bslib pattern of `ptr_app_bslib()`.
#
# This file ships PLAN-01: `ptr_ggplotly()` plus the four pure-core /
# guard / source-id / store seams it is factored into (ADR Decision 8: pure
# core + thin reactive shell). The selection helper `ptr_plotly_selection()`
# is PLAN-02 and lives in this same file once landed.

# ---- internal: installed guard (mock seam) ----------------------------------

# Trivial wrapper around the installed-check so the not-installed branch is
# interceptable via testthat::local_mocked_bindings(plotly_installed =
# function(...) FALSE) on the ggpaintr namespace. A namespaced
# rlang::check_installed() call alone is not mockable that way.
plotly_installed <- function() {
  requireNamespace("plotly", quietly = TRUE)
}

# ---- internal: per-draw key minting (pure core) -----------------------------

# Mint per-draw row keys on a *copy* of the plot for the widget only. The key
# must physically travel inside the widget to the browser and back, so it is
# minted on the widget copy; the user's drawn data is never mutated.
#
# `plot`      a built ggplot object whose data is a data frame.
# `key_warned` whether this instance has already emitted the key-override
#             warning (the shell threads the stored per-instance flag here to
#             get warn-once, ADR Decision 5).
#
# Returns list(plot = <widget-ready copy>, key_overridden = <logical>).
# `key_overridden` is TRUE iff the incoming plot mapped a foreign `key`
# aesthetic (ADR Decision 5: ggpaintr owns the key channel).
plotly_mint_keys <- function(plot, key_warned = FALSE) {
  if (!ggplot2::is_ggplot(plot)) {
    rlang::abort("`plot` must be a ggplot object.")
  }

  key_overridden <- "key" %in% names(plot$mapping)
  if (key_overridden && !isTRUE(key_warned)) {
    cli::cli_warn(c(
      "!" = "ggpaintr owns the plotly {.code key} aesthetic for linked selection.",
      "i" = "The formula's own {.code aes(key = ...)} is replaced by the per-row key {.code .ptr_row}."
    ))
  }

  # Copy-on-mint: never mutate the caller's data frame. Overwrite any stale
  # reserved `.ptr_row` column silently (ADR Decision 5 collision rule).
  out <- plot
  out$data <- out$data
  out$data$.ptr_row <- seq_len(nrow(out$data))
  # Bare symbol (not `.data$.ptr_row`) so the mapped key expr is exactly
  # `.ptr_row` — the contract's frozen observable. The column is on the
  # plot's own data, so column-scoped lookup resolves it.
  out <- out + ggplot2::aes(key = .ptr_row)

  list(plot = out, key_overridden = key_overridden)
}

# ---- internal: source id + per-session store --------------------------------

# Deterministic, instance-distinct source string for plotly's `source =`
# channel. Derived from the instance's namespace so two ptr_server instances
# ("p1", "p2") in one session get distinct strings without the user wiring a
# literal. `server_ns_fn` collapses to identity under moduleServer (so it does
# NOT carry the module id); `ui_ns_fn` is the namespaced session$ns and is
# what actually distinguishes instances. Both are on the state handle and both
# derive from "the instance's namespace".
plotly_source_id <- function(state) {
  ptr_validate_state(state)
  id <- tryCatch(state$ui_ns_fn("ptr_plotly"), error = function(e) "ptr_plotly")
  if (is.null(id) || !nzchar(id)) id <- "ptr_plotly"
  id
}

# Per-session store hung on session$userData (created on first call, reused
# after). Per-instance entries hold the source string, the per-draw snapshot
# of the drawn data, and (from PLAN-02) the selection state. Never a
# package-global environment (ADR Decision 6).
plotly_store <- function(session) {
  if (is.null(session$userData$.ptr_plotly_store)) {
    session$userData$.ptr_plotly_store <- new.env(parent = emptyenv())
  }
  session$userData$.ptr_plotly_store
}

# Fetch (creating if absent) the per-instance entry environment in the store,
# keyed by instance id.
plotly_store_entry <- function(store, instance_id) {
  if (is.null(store[[instance_id]])) {
    store[[instance_id]] <- new.env(parent = emptyenv())
  }
  store[[instance_id]]
}

# Instance id used as the store key: the module id the widget belongs to.
# `ui_ns_fn("")` yields "<id>-" under moduleServer; strip the trailing dash so
# the key matches the user-supplied module id ("p1", "p2").
plotly_instance_id <- function(state) {
  prefix <- tryCatch(state$ui_ns_fn(""), error = function(e) "")
  if (is.null(prefix)) prefix <- ""
  sub("-$", "", prefix)
}

# ---- exported: state-first plotly widget builder ----------------------------

#' Build a plotly Widget From a `ggpaintr` Instance, Wired for Linked Selection
#'
#' `ptr_ggplotly()` is the state-first counterpart to [plotly::ggplotly()] for
#' the supported L3 custom-render pattern (see [ptr_server()], section "Custom
#' render (L3)"). Call it from inside [plotly::renderPlotly()] on the live
#' instance returned by [ptr_server()]: it reads the drawn plot off
#' `state$runtime()$plot`, mints a per-draw row key (the plotly `key`
#' aesthetic, on a widget-only copy of the data), sets `dragmode = "select"`,
#' registers the `plotly_selected` / `plotly_deselect` events, and returns a
#' plain plotly object so your own plotly verbs stay composable after it.
#'
#' The helper owns the [shiny::req()] pre-draw guard (it raises Shiny's silent
#' pre-draw condition before the first draw, exactly like a bare `req(p)`) and
#' derives plotly's `source =` channel from the instance namespace, so the
#' companion selection reader coordinates without extra wiring. The user's
#' drawn data is never mutated: keys are minted per draw on the widget copy
#' only.
#'
#' plotly is an optional dependency (in `Suggests`). When it is not installed,
#' this function aborts, mirroring [ptr_app_bslib()]'s bslib guard.
#'
#' @param state A `ptr_state` handle as returned by [ptr_server()].
#' @param ... Forwarded verbatim to [plotly::ggplotly()] (e.g.
#'   `tooltip = "all"`).
#' @param source Character scalar for plotly's `source =` channel, or `NULL`
#'   (default) to derive a distinct string from the instance namespace.
#'
#' @return A plain plotly htmlwidget object (inherits class `"plotly"`), with
#'   `dragmode = "select"` set and the `plotly_selected` / `plotly_deselect`
#'   events registered.
#'
#' @seealso [ptr_server()] for the L3 custom-render contract;
#'   [plotly::ggplotly()].
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(plotly)
#' f <- rlang::expr(
#'   ggplot(mtcars, aes(x = ppVar(wt), y = ppVar(mpg))) + geom_point()
#' )
#' ui <- ptr_ui_page(
#'   ptr_ui_controls(formula = f, id = "p"),
#'   plotly::plotlyOutput("plt")
#' )
#' server <- function(input, output, session) {
#'   state <- ptr_server(f, "p")
#'   output$plt <- plotly::renderPlotly({
#'     # state-first: req() guard + key minting + select wiring are internal
#'     ptr_ggplotly(state, tooltip = "all") |>
#'       plotly::layout(legend = list(orientation = "h"))
#'   })
#' }
#' shinyApp(ui, server)
#' }
#'
#' @export
ptr_ggplotly <- function(state, ..., source = NULL) {
  if (!plotly_installed()) {
    rlang::abort(
      "Package 'plotly' is required for ptr_ggplotly(). Install it with install.packages(\"plotly\")."
    )
  }
  ptr_validate_state(state)

  # Pre-draw guard: before the first draw `state$runtime()` is NULL (and even
  # post-draw its `$plot` may be absent). Mirror the demo's `req(p)`: a silent
  # Shiny pre-draw condition, never a hard error.
  rt <- state$runtime()
  plot <- rt$plot
  shiny::req(plot)

  # Source id: explicit `source =` wins; else derive from the namespace.
  src <- source %||% plotly_source_id(state)

  # Per-instance warn-once flag, threaded through the pure core.
  session <- shiny::getDefaultReactiveDomain()
  entry <- NULL
  if (!is.null(session)) {
    store <- plotly_store(session)
    entry <- plotly_store_entry(store, plotly_instance_id(state))
  }
  key_warned <- isTRUE(entry$key_warned)

  minted <- plotly_mint_keys(plot, key_warned = key_warned)

  # Record server-side state for this draw (source string + per-draw snapshot
  # of the drawn data), keyed by instance id (ADR Decision 6). Never mutates
  # the runtime's data; the snapshot is the copy carrying the keys.
  if (!is.null(entry)) {
    entry$source <- src
    entry$snapshot <- minted$plot$data
    if (minted$key_overridden) entry$key_warned <- TRUE
  }

  plotly::ggplotly(minted$plot, source = src, ...) |>
    plotly::layout(dragmode = "select") |>
    plotly::event_register("plotly_selected") |>
    plotly::event_register("plotly_deselect")
}
