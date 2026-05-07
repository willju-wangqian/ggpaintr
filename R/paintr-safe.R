# P12 safe wrappers — three-stage chained pipeline whose failures are
# captured as fields on a result list rather than propagated as conditions.
# This lets the Shiny observer always update the code panel and surface
# eval/render errors as inline UI without crashing the reactive graph.
#
# The shape of `result`:
#   ok        TRUE/FALSE
#   stage     "complete" | "plot"  (where the failure occurred)
#   code_text rendered code text (empty on `complete` failure)
#   pruned    post-substitute, post-prune typed tree (NULL on failure)
#   eval_env  the env to pass to ptr_eval (carried through so resolve hooks
#             that bind names — e.g., upload — are visible)
#   plot      the assembled ggplot (NULL until ptr_assemble_plot_safe)
#   error     conditionMessage(e) when ok=FALSE
#   condition the original condition object when ok=FALSE

ptr_complete_expr_safe <- function(node,
                                   snapshot = list(),
                                   shared_bindings = list(),
                                   eval_env = parent.frame(),
                                   safe_to_remove = NULL,
                                   is_standalone = NULL,
                                   upstream_cols = list()) {
  tryCatch({
    subbed <- ptr_substitute(
      node,
      input_snapshot = snapshot,
      shared_bindings = shared_bindings,
      eval_env = eval_env,
      upstream_cols = upstream_cols
    )
    pruned <- ptr_prune(
      subbed,
      safe_to_remove = safe_to_remove,
      is_standalone = is_standalone
    )
    code_text <- if (is_ptr_missing(pruned) ||
      (is_ptr_root(pruned) && length(pruned$layers) == 0L)) {
      ""
    } else {
      ptr_render(pruned)
    }
    list(
      ok = TRUE,
      stage = "complete",
      code_text = code_text,
      pruned = pruned,
      eval_env = eval_env,
      plot = NULL,
      error = NULL,
      condition = NULL
    )
  }, error = function(e) {
    list(
      ok = FALSE,
      stage = "complete",
      code_text = "",
      pruned = NULL,
      eval_env = eval_env,
      plot = NULL,
      error = conditionMessage(e),
      condition = e
    )
  })
}

ptr_assemble_plot_safe <- function(result, expr_check = TRUE) {
  if (!isTRUE(result$ok)) return(result)

  pruned <- result$pruned
  if (!is_ptr_root(pruned) || length(pruned$layers) == 0L) {
    result$ok <- FALSE
    result$stage <- "plot"
    result$error <- "No layers to render (after pruning)."
    return(result)
  }

  tryCatch({
    result$plot <- ptr_eval(
      pruned,
      eval_env = result$eval_env,
      expr_check = expr_check
    )
    result
  }, error = function(e) {
    result$ok <- FALSE
    result$stage <- "plot"
    result$error <- conditionMessage(e)
    result$condition <- e
    result$plot <- NULL
    result
  })
}

ptr_validate_plot_render_safe <- function(result) {
  if (!isTRUE(result$ok)) return(result)
  if (is.null(result$plot)) return(result)

  tryCatch({
    ggplot2::ggplot_build(result$plot)
    result
  }, error = function(e) {
    result$ok <- FALSE
    result$stage <- "plot"
    result$error <- conditionMessage(e)
    result$condition <- e
    result
  })
}
