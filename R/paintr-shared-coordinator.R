# Shared coordinator (multi-instance API). `ptr_shared()` builds one pure,
# non-reactive `ptr_shared_spec` from the full formula set; it is the single
# source of truth so the UI and server can never disagree about which keys
# are formula-local versus cross-formula.
#
# Partition rule (ADR 0005 / CONTEXT.md "Shared coordinator"):
#   a shared key referenced in exactly one formula  -> that formula's inline
#     shared section (rendered later by ptr_ui_controls; Step 04)
#   a shared key referenced in >= 2 formulas         -> the one standalone
#     shared panel (`ptr_shared_panel` / `ptr_ui_shared_panel`)
#
# The reactive bundle (`ptr_shared_server(obj)`) lives in paintr-shared-ui.R
# alongside the plumbing it shares; its partition-aware wiring is Step 02.


# ---- partition -------------------------------------------------------------

# Pure: given the per-formula typed ASTs, return the cross-formula partition.
# `panel_keys`            -- keys referenced by >= 2 formulas, in discovery
#                            order, destined for the standalone panel.
# `local_keys_by_formula` -- list parallel to `trees`; element i holds the
#                            keys that occur in exactly one formula and that
#                            formula is i (the inline-section keys).
# `keys_by_formula`       -- list parallel to `trees`; element i is every
#                            shared key that formula i references.
# No reactivity, no Shiny. The count is per-formula occurrence: a key used
# twice inside one formula still counts as one formula (=> formula-local).
shared_partition <- function(trees) {
  keys_by_formula <- lapply(trees, function(tr) {
    one <- list(tr)
    value_keys <- names(shared_first_nodes(one)$nodes)
    consumer_keys <- names(shared_consumer_representatives(one))
    unique(c(value_keys, consumer_keys))
  })
  all_keys <- unique(unlist(keys_by_formula, use.names = FALSE))
  formula_counts <- stats::setNames(
    vapply(all_keys, function(k) {
      sum(vapply(keys_by_formula, function(ks) k %in% ks, logical(1)))
    }, integer(1)),
    all_keys
  )
  panel_keys <- all_keys[formula_counts >= 2L]
  local_keys_by_formula <- lapply(keys_by_formula, function(ks) {
    ks[vapply(ks, function(k) formula_counts[[k]] == 1L, logical(1))]
  })
  list(
    panel_keys = panel_keys,
    local_keys_by_formula = local_keys_by_formula,
    keys_by_formula = keys_by_formula
  )
}


# Compute the "source signature" of one consumer's `$upstream` subtree under
# the active partition. Returns a list with `kind` in
# c("none", "panel", "formula_local_source", "bare") plus
# `desc` (verbatim, human-readable source expression for the abort message)
# and `cmp` (the comparable key used to decide signature equality across
# formulas — same `cmp` across formulas => panel-owned consumer is
# coordinated).
#
# - "none"                  : upstream is NULL (consumer has no upstream)
# - "panel"                 : upstream contains a `ptr_ph_data_source` whose
#                             `$shared` is in `panel_keys`
# - "formula_local_source"  : upstream contains a `ptr_ph_data_source` that is
#                             NOT panel-owned (no `$shared`, or `$shared` not in
#                             `panel_keys`). This kind always fails the
#                             invariant — independent widgets in each formula
#                             cannot collaboratively drive a single
#                             panel-scope picker.
# - "bare"                  : upstream is a non-source subtree (typically a
#                             `ptr_literal` like `mtcars`); `cmp` is its
#                             deparsed AST so identical bare-data passes.
shared_upstream_source_signature <- function(upstream, panel_keys) {
  if (is.null(upstream)) {
    return(list(kind = "none", desc = "(no upstream source)", cmp = "none"))
  }
  sources <- find_nodes(upstream, is_ptr_ph_data_source)
  if (length(sources) > 0L) {
    s <- sources[[1L]]
    if (!is.null(s$shared) && s$shared %in% panel_keys) {
      return(list(
        kind = "panel",
        desc = paste0("panel-owned source \"", s$shared, "\""),
        cmp  = paste0("panel", s$shared)
      ))
    }
    desc <- paste(deparse(s$expr), collapse = " ")
    return(list(
      kind = "formula_local_source",
      desc = desc,
      cmp  = paste0("formula_local", desc)
    ))
  }
  ast <- upstream$expr %||% upstream
  desc <- paste(deparse(ast), collapse = " ")
  list(kind = "bare", desc = desc, cmp = paste0("bare", desc))
}

# ADR 0023 §6 — parse-time invariant. For each panel-owned consumer key,
# walk every formula it appears in and abort if any formula's upstream
# source is formula-local OR if the per-formula source signatures differ.
# Reads the partition + trees already in scope; no fresh AST walk added.
shared_assert_panel_consumer_sources <- function(trees, panel_keys, formulas) {
  if (length(panel_keys) == 0L) return(invisible(NULL))
  for (key in panel_keys) {
    formula_idxs <- integer(0)
    sigs <- list()
    for (i in seq_along(trees)) {
      occ <- find_nodes(trees[[i]], function(x) {
        is_ptr_ph_data_consumer(x) && identical(x$shared, key)
      })
      if (length(occ) == 0L) next
      formula_idxs <- c(formula_idxs, i)
      sigs[[length(sigs) + 1L]] <- shared_upstream_source_signature(
        occ[[1L]]$upstream, panel_keys
      )
    }
    if (length(formula_idxs) < 2L) next
    any_local_src <- any(vapply(sigs, function(s) {
      identical(s$kind, "formula_local_source")
    }, logical(1)))
    cmps <- vapply(sigs, function(s) s$cmp, character(1))
    sigs_differ <- length(unique(cmps)) > 1L
    if (!any_local_src && !sigs_differ) next

    formula_lines <- vapply(seq_along(formula_idxs), function(j) {
      paste0("formula ", formula_idxs[[j]], ": ", sigs[[j]]$desc)
    }, character(1))
    body <- if (any_local_src) {
      paste0(
        "Panel-owned shared consumer \"", key,
        "\" requires every source it depends on to be panel-owned, but the ",
        "upstream sources are formula-local (each widget is independent and ",
        "cannot collaboratively drive a single panel-scope picker)."
      )
    } else {
      paste0(
        "Panel-owned shared consumer \"", key,
        "\" requires every source it depends on to be panel-owned, but the ",
        "upstream sources differ across formulas."
      )
    }
    hint <- paste0(
      "Make the upstream source panel-owned by giving it the same ",
      "`shared = \"...\"` annotation across formulas (e.g. ",
      "`ppUpload(shared = \"ds\")`), or drop `shared = \"", key,
      "\"` on the consumer so it becomes formula-local."
    )
    msg <- c(body, stats::setNames(formula_lines, rep("*", length(formula_lines))),
             i = hint)
    rlang::abort(
      message  = msg,
      class    = "ptr_panel_consumer_source_mismatch",
      key      = key,
      formulas = as.character(formulas)[formula_idxs],
      sources  = vapply(sigs, function(s) s$desc, character(1))
    )
  }
  invisible(NULL)
}

# ---- ptr_shared (coordinator) ---------------------------------------------

#' Build the Shared Coordinator for a Multi-Instance Embedding
#'
#' Constructs a single pure, non-reactive coordinator object from the full
#' set of plot formulas. The coordinator computes the cross-formula
#' **partition** -- a shared key used by exactly one formula renders in that
#' formula's inline shared section; a key used by two or more formulas
#' renders in the one standalone [`ptr_shared_panel()`]. Because every
#' consumer derives its view from this one object, the UI and server can
#' never disagree about the partition.
#'
#' This is strictly multi-instance API: with a single ggpaintr instance all
#' shared widgets auto-render inline and no coordinator is needed. The
#' coordinator is consumed by exactly three functions, each taking only the
#' object: [`ptr_shared_panel()`], [`ptr_ui_shared_panel()`], and
#' [`ptr_shared_server()`].
#'
#' Errors when no formula declares a `shared = "..."` annotation -- building
#' the coordinator is a declaration of intent.
#'
#' @param formulas A character vector or list of formula strings, one per
#'   embedded [`ptr_ui()`] instance.
#' @param id Optional character scalar that namespaces every id this
#'   coordinator emits, so two or more coordinators can coexist in one
#'   Shiny session without colliding on shared `input` slots. When
#'   non-`NULL`, every id (`shared_<key>`, `shared_<key>_stage_enabled`,
#'   `ptr_shared_draw_all`, `ptr_shared_errors`, the stage-block DOM id,
#'   and each consumer `renderUI` container) is prefixed `<id>-` using
#'   Shiny `NS()`'s separator. The default `NULL` preserves today's flat
#'   ids byte-for-byte -- single-panel apps need no change. Must be `NULL`
#'   or a non-empty string matching `^[A-Za-z][A-Za-z0-9_]*$`;
#'   [`ptr_shared_panel()`], [`ptr_ui_shared_panel()`], and
#'   [`ptr_shared_server()`] all read it off `obj$id` -- their signatures
#'   do not change.
#' @param shared_ui Named list of `function(id) -> shiny.tag` builders, one
#'   per shared key the embedder wants to customise. Unsupplied keys are
#'   auto-rendered from the first formula that mentions them.
#' @param ui_text Optional copy overrides forwarded to the auto-built
#'   widgets (see [`ptr_app()`]'s `ui_text` argument).
#' @param expr_check Whether to validate `ppExpr` placeholders during formula
#'   translation. Default `TRUE`.
#' @param draw_all_label Label for the "Draw all" button rendered in the
#'   panel when two or more formulas are supplied.
#'
#' @return A `ptr_shared_spec` S3 object. Public fields: `panel_keys`
#'   (cross-formula keys), `local_keys_by_formula` (per-formula inline
#'   keys). Deterministic and idempotent for a given `formulas`.
#' @seealso [`ptr_shared_panel()`], [`ptr_ui_shared_panel()`],
#'   [`ptr_shared_server()`], [`ptr_ui()`].
#' @examples
#' obj <- ptr_shared(c(
#'   "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
#'   "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
#' ))
#' obj$panel_keys   # "x" — shared by both formulas
#' @export
ptr_shared <- function(formulas,
                       id = NULL,
                       shared_ui = list(),
                       ui_text = NULL,
                       expr_check = TRUE,
                       draw_all_label = "Draw all") {
  if (!(is.null(id) ||
        (is.character(id) && length(id) == 1L && !is.na(id) &&
         nzchar(id) && grepl("^[A-Za-z][A-Za-z0-9_]*$", id)))) {
    rlang::abort(paste0(
      "`id` must be NULL or a single non-empty string matching ",
      "\"^[A-Za-z][A-Za-z0-9_]*$\"; got ",
      paste0(utils::capture.output(print(id)), collapse = " "), "."
    ))
  }
  assertthat::assert_that(is.list(shared_ui))
  trees <- shared_translate_formulas(formulas, expr_check = expr_check)

  firsts <- shared_first_nodes(trees)
  value_keys <- names(firsts$nodes)
  consumer_reps <- shared_consumer_representatives(trees)
  consumer_keys <- names(consumer_reps)
  # A key lives in exactly one bucket: `var(shared=)` consumers in
  # `consumer_reps`, value placeholders (text/num/expr/upload) in `firsts`.
  formula_keys <- unique(c(value_keys, consumer_keys))

  if (length(formula_keys) == 0L) {
    rlang::abort(
      "`ptr_shared()` was called with formulas that declare no `shared = \"...\"` placeholders. Drop the call, or annotate at least one placeholder in `formulas`."
    )
  }

  if (length(shared_ui) > 0L) {
    nms <- names(shared_ui)
    if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
      rlang::abort(
        "`shared_ui` must have unique non-empty names matching the `shared` annotations in `formulas`."
      )
    }
    if (!all(vapply(shared_ui, is.function, logical(1)))) {
      rlang::abort(
        "Every entry of `shared_ui` must be a function `function(id) -> shiny.tag`."
      )
    }
    extra <- setdiff(nms, formula_keys)
    if (length(extra) > 0L) {
      rlang::abort(paste0(
        "`shared_ui` references key ",
        paste0("\"", extra, "\"", collapse = ", "),
        " which is not used in any plot formula. Available formula keys: ",
        paste0("\"", formula_keys, "\"", collapse = ", "), "."
      ))
    }
  }

  part <- shared_partition(trees)

  # PLAN-03 / ADR 0023 §6: a panel-owned consumer key must be driven by a
  # panel-owned source. A single panel-scope picker cannot list columns from
  # two formula-local datasets without inventing arbitrary semantics
  # (union/intersection/per-instance widgets). Fail fast here, naming the
  # consumer key, the offending formulas (1-indexed), and the verbatim source
  # expressions; the embedder can `tryCatch()` it via class
  # "ptr_panel_consumer_source_mismatch".
  shared_assert_panel_consumer_sources(
    trees       = trees,
    panel_keys  = part$panel_keys,
    formulas    = formulas
  )

  structure(
    list(
      id = id,
      formulas = formulas,
      trees = trees,
      formula_count = length(trees),
      formula_keys = formula_keys,
      panel_keys = part$panel_keys,
      local_keys_by_formula = part$local_keys_by_formula,
      keys_by_formula = part$keys_by_formula,
      firsts = firsts,
      consumer_reps = consumer_reps,
      shared_ui = shared_ui,
      ui_text = ui_text,
      expr_check = expr_check,
      draw_all_label = draw_all_label
    ),
    class = c("ptr_shared_spec", "list")
  )
}


# ---- panel renderers -------------------------------------------------------

# Build the wellPanel body for the given key set, reusing the per-key widget
# resolution that previously lived inline in `ptr_shared_ui()`. `keys` is
# always a subset of `obj$formula_keys` (the panel passes `obj$panel_keys`).
shared_panel_body_tag <- function(obj, keys) {
  firsts <- obj$firsts
  consumer_reps <- obj$consumer_reps
  consumer_keys <- names(consumer_reps)
  shared_ui <- obj$shared_ui
  ui_text <- obj$ui_text
  ns <- shared_ns(obj)

  shared_label_override <- lapply(firsts$occurrences, shared_widget_label)
  # PLAN-07: first-occurrence default for each shared bucket. Seeded onto
  # `node$default` below so `invoke_build_ui` picks it up via the same
  # path as per-layer widgets.
  shared_default_override <- lapply(firsts$occurrences, shared_widget_default)

  shared_widgets <- lapply(keys, function(k) {
    canonical <- canonical_shared_id(k)
    if (k %in% names(shared_ui)) {
      shared_ui[[k]](ns(canonical))
    } else if (k %in% consumer_keys) {
      node <- consumer_reps[[k]]
      build_ui_for(
        node,
        ns_fn = ns,
        label_override = node$shared_label
      )
    } else {
      node <- firsts$nodes[[k]]
      node$id <- canonical
      node$default <- shared_default_override[[k]]
      build_ui_for(
        node,
        ui_text = ui_text,
        ns_fn = ns,
        label_override = shared_label_override[[k]]
      )
    }
  })
  kept_mask <- !vapply(shared_widgets, is.null, logical(1))
  shared_widgets <- shared_widgets[kept_mask]
  rendered_keys <- keys[kept_mask]

  if (length(shared_widgets) == 0L) return(NULL)

  # Wrap any rendered key that hosts an orphan pipeline stage (a stage whose
  # only placeholders are shared) in a `.ptr-stage` block; the head checkbox
  # uses the synthetic `shared_stage_input_id(k)` that `ptr_shared_server()`
  # mirrors into each module's `state$stage_enabled`.
  orphan_info <- collect_shared_stage_keys(obj$trees)
  if (length(orphan_info) > 0L && length(shared_widgets) > 0L) {
    shared_widgets <- lapply(seq_along(shared_widgets), function(i) {
      k <- rendered_keys[[i]]
      w <- shared_widgets[[i]]
      info <- orphan_info[[k]]
      if (is.null(info)) return(w)
      verbs <- info$verbs
      head_label <- if (length(verbs) > 0L) {
        shiny::tags$code(paste0(paste(verbs, collapse = "/"), "()"))
      } else NULL
      controllable_region(
        shared_stage_input_id(k), head_label, w, ns_fn = ns
      )
    })
  }

  shell_copy <- ptr_ui_text(ui_text)$shell
  body <- c(
    list(
      shared_panel_header(shell_copy$shared_panel_title, shell_copy$shared_panel_hint)
    ),
    shared_widgets,
    if (obj$formula_count >= 2L) {
      list(shiny::actionButton(ns("ptr_shared_draw_all"), obj$draw_all_label))
    } else list(),
    list(shiny::uiOutput(ns("ptr_shared_errors")))
  )

  shiny::wellPanel(
    do.call(shiny::div, c(list(class = "ptr-shared-panel"), body))
  )
}

#' Render the Standalone Shared Panel (L2, Self-Contained)
#'
#' Renders the single page-level [`shiny::wellPanel()`] holding exactly the
#' coordinator's cross-formula keys (`obj$panel_keys`, referenced by two or
#' more formulas). The panel is self-contained: it owns its `.ptr-app`
#' theming scope and the bundled ggpaintr asset dependency, so it can be
#' dropped straight into a host layout. Its server counterpart
#' [`ptr_shared_server()`] must run at the top level.
#'
#' Namespacing is inherited from `obj$id`; supply it to [`ptr_shared()`].
#'
#' @param obj A `ptr_shared_spec` from [`ptr_shared()`].
#' @param css Optional character vector of paths to additional CSS files;
#'   linked after `ggpaintr`'s bundled stylesheet so its rules win. See
#'   [ptr_app()] for the full semantics. Defaults to `NULL`.
#'
#' @return A `shiny.tag` `div.ptr-app` wrapping the wellPanel and the asset
#'   bundle, suitable for direct placement in the embedder's UI, or `NULL`
#'   when no panel keys exist.
#' @seealso [`ptr_shared()`], [`ptr_ui_shared_panel()`],
#'   [`ptr_shared_server()`].
#' @examples
#' obj <- ptr_shared(c(
#'   "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
#'   "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
#' ))
#' ptr_shared_panel(obj)
#' @export
ptr_shared_panel <- function(obj, css = NULL) {
  if (!inherits(obj, "ptr_shared_spec")) {
    rlang::abort("`ptr_shared_panel()` requires a `ptr_shared_spec` from `ptr_shared()`.")
  }
  # Bare `.ptr-app`: themed scope only, no `--page` canvas. The panel is a
  # region fragment dropped into a host layout (its own row above the
  # plots), so it sizes to its content, not the full viewport.
  body <- shared_panel_body_tag(obj, obj$panel_keys)
  if (is.null(body)) return(NULL)
  shiny::tags$div(
    class = "ptr-app",
    ptr_assets(css = css),
    body
  )
}

#' Render the Standalone Shared Panel (L3, Bare)
#'
#' The bare counterpart to [`ptr_shared_panel()`]: identical inner markup
#' (the wellPanel holding `obj$panel_keys`) with **no** `.ptr-app` shell and
#' **no** asset bundle. The L3 user supplies their own shell / assets (e.g.
#' via [`ptr_ui_page()`]).
#'
#' Namespacing is inherited from `obj$id`; supply it to [`ptr_shared()`].
#'
#' @param obj A `ptr_shared_spec` from [`ptr_shared()`].
#'
#' @return A `shiny.tag` wellPanel with no wrapper and no injected assets, or
#'   `NULL` when no panel keys exist.
#' @seealso [`ptr_shared()`], [`ptr_shared_panel()`],
#'   [`ptr_shared_server()`].
#' @examples
#' obj <- ptr_shared(c(
#'   "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_point()",
#'   "ggplot(mtcars, aes(x = ppVar(shared='x'), y = ppVar)) + geom_bar()"
#' ))
#' ptr_ui_shared_panel(obj)
#' @export
ptr_ui_shared_panel <- function(obj) {
  if (!inherits(obj, "ptr_shared_spec")) {
    rlang::abort("`ptr_ui_shared_panel()` requires a `ptr_shared_spec` from `ptr_shared()`.")
  }
  shared_panel_body_tag(obj, obj$panel_keys)
}
