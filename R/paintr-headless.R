# Headless runtime — INTERNAL ONLY.
#
# These helpers run the formula -> plot pipeline without a Shiny session. They
# back two things: the reactive runtime observer (`ptr_setup_runtime()` calls
# `ptr_exec_headless()`), and out-of-Shiny uses (batch report generation,
# runtime regression tests via `ptr_run_formula()`). Nothing here is exported
# or documented as public API; if a public headless surface is ever needed it
# should be added then, with a settled name and a man/ page.

# Pure core of the runtime: (tree, input snapshot, ...) -> detached runtime
# list (`ok`, `stage`, `error`, `plot`, `code_text`, `pruned`, ...). The
# computation and call order match `ptr_setup_runtime()` exactly:
#   ptr_complete_expr_safe -> inject_resolved_data_list -> ptr_assemble_plot_safe
#   -> (+ extras) -> ptr_validate_plot_render_safe
#
# `resolved_data`: plain named list (layer name -> data.frame or NULL), already
#   de-reactived by the caller. `upstream_cols`: the value the caller has
#   already computed (the reactive caller via `runtime_upstream_cols(state,
#   snapshot)`, the headless caller via `ptr_headless_upstream_cols()`).
#   `stage_enabled`: named list (stage id -> logical); when NULL the tree is
#   used as-is (all stages enabled).
ptr_exec_headless <- function(tree, snapshot,
                              shared_bindings = list(),
                              eval_env       = parent.frame(),
                              safe_to_remove = character(),
                              expr_check     = TRUE,
                              extras         = list(),
                              stage_enabled  = NULL,
                              resolved_data  = list(),
                              upstream_cols  = NULL,
                              upstream_data  = NULL) {
  if (!is.null(stage_enabled)) tree <- disable_walk(tree, stage_enabled)

  res <- ptr_complete_expr_safe(
    tree,
    snapshot = snapshot,
    shared_bindings = shared_bindings,
    eval_env = eval_env,
    safe_to_remove = safe_to_remove,
    upstream_cols = upstream_cols,
    upstream_data = upstream_data %||% list()
  )
  res$pruned <- inject_resolved_data_list(res$pruned, resolved_data)
  res <- ptr_assemble_plot_safe(res, expr_check = expr_check)

  if (isTRUE(res$ok) && length(extras) > 0L) {
    res$plot <- Reduce(`+`, extras, res$plot)
  }
  ptr_validate_plot_render_safe(res)
}

# No-Shiny analog of `runtime_upstream_cols()`: resolve each data-consumer
# node's `$upstream` directly via the already-pure `ptr_resolve_upstream()`
# and return a list keyed by consumer id -> column names. Unlike the reactive
# version it does not consult any `state` (no resolved-data cache, no
# upstream-cache) and ignores the bare-data-source-layer shortcut — it always
# resolves from `$upstream`. That is enough for completion to validate `var`
# selections against the upstream columns in headless mode.
ptr_headless_upstream_cols <- function(tree, snapshot = list(),
                                       shared_bindings = list(),
                                       eval_env = parent.frame(),
                                       expr_check = TRUE) {
  out <- list()
  consumers <- find_nodes(tree, is_ptr_ph_data_consumer)
  for (node in consumers) {
    if (is.null(node$id)) next
    df <- ptr_resolve_upstream(
      node$upstream,
      snapshot = snapshot,
      shared_bindings = shared_bindings,
      eval_env = eval_env,
      cache = NULL,
      expr_check = expr_check,
      stage_enabled = list()
    )
    if (!is.null(df)) out[[node$id]] <- names(df)
  }
  out
}

# Default input snapshot for a runtime input spec: layer-include checkboxes
# (`role == "layer_checkbox"`) and stage toggles (`role == "stage_enabled"`)
# start checked (`TRUE`) unless ADR 0020 has stamped the carrier with
# `default_active = FALSE` (layer_checkbox) or `default_stage_enabled = FALSE`
# (stage_enabled); everything else is NULL. The carrier-node lookup goes
# through `find_layer_by_name()` / `find_stage_call_by_id()` so this site
# and `ptr_spec_defaults_from_state()` consult the same field, the same way.
# Keys are RAW input ids (the `input_id` column), not namespaced.
ptr_default_snapshot <- function(spec, tree) {
  snapshot <- list()
  if (nrow(spec) == 0L) return(snapshot)

  for (i in seq_len(nrow(spec))) {
    raw_id <- spec$input_id[i]
    role   <- spec$role[i]
    if (identical(role, "layer_checkbox")) {
      layer_name <- spec$layer_name[i]
      carrier <- find_layer_by_name(tree, layer_name)
      snapshot[[raw_id]] <- isTRUE(carrier$default_active %||% TRUE)
    } else if (identical(role, "stage_enabled")) {
      carrier <- find_stage_call_by_id(tree, raw_id)
      snapshot[[raw_id]] <- isTRUE(carrier$default_stage_enabled %||% TRUE)
    } else {
      snapshot[raw_id] <- list(NULL)
    }
  }
  snapshot
}

# Internal convenience wrapper for the common headless case: run a formula with
# an optional override of input values (named by RAW input id). Anything not
# supplied falls back to `ptr_default_snapshot()`'s defaults.
ptr_run_formula <- function(formula, inputs = list(), envir = parent.frame(),
                            expr_check = TRUE, safe_to_remove = character()) {
  tree     <- ptr_translate(formula, expr_check = expr_check)
  spec     <- ptr_runtime_input_spec(tree)
  snapshot <- ptr_default_snapshot(spec, tree)
  if (length(inputs) > 0L) snapshot[names(inputs)] <- inputs

  upstream_cols <- ptr_headless_upstream_cols(
    tree, snapshot = snapshot, eval_env = envir, expr_check = expr_check
  )
  ptr_exec_headless(
    tree, snapshot,
    eval_env       = envir,
    safe_to_remove = validate_safe_to_remove(safe_to_remove),
    expr_check     = expr_check,
    upstream_cols  = upstream_cols
  )
}
