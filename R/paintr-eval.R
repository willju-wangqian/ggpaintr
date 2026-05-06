# P11 — eval. Typed tree -> ggplot object.
# For each surviving layer, build an evaluable R `language` object: pipelines
# fold via `desugar_pipe_to_call` (so neither `|>` nor `%>%` requires runtime
# magrittr); other nodes reconstitute directly. Each layer expression is
# re-validated by P5 (post-substitution structure may differ) before eval.
# Layer results accumulate with `+`. No-layers errors out distinctly from
# safety errors (BDD P5.18 / P11.3).

ptr_eval <- function(node, eval_env = parent.frame(), expr_check = TRUE) {
  if (!is_ptr_root(node)) {
    rlang::abort("ptr_eval expects a ptr_root.")
  }
  if (length(node$layers) == 0L) {
    rlang::abort("No layers to evaluate (after pruning).")
  }
  exprs <- lapply(node$layers, layer_to_eval_expr)
  ph_names <- ptr_registry_v2_keywords()
  for (e in exprs) {
    validate_expr_safety(e, expr_check = expr_check, placeholder_names = ph_names)
  }
  results <- lapply(exprs, function(e) eval(e, envir = eval_env))
  Reduce(`+`, results[-1L], results[[1L]])
}

layer_to_eval_expr <- function(layer) {
  if (is_ptr_user_expr(layer)) return(layer$inner)
  if (is_ptr_placeholder(layer)) {
    rlang::abort(paste0(
      "Unsubstituted placeholder layer reached eval (keyword=",
      layer$keyword %||% "?", ")."
    ))
  }
  head_expr <- layer_head_lang(layer)
  is_piped <- layer_is_piped(layer)
  child_args <- node_list_to_lang(layer$children)
  if (is_piped && is_ptr_pipeline(layer$data_arg)) {
    upstream <- pipeline_to_eval_expr(layer$data_arg)
    rhs <- as.call(c(list(head_expr), child_args))
    return(desugar_pipe_to_call(upstream, rhs))
  }
  if (is.null(layer$data_arg)) {
    return(as.call(c(list(head_expr), child_args)))
  }
  data_expr <- node_to_lang(layer$data_arg)
  as.call(c(list(head_expr), list(data_expr), child_args))
}

layer_head_lang <- function(layer) {
  e <- layer$expr
  if (is.call(e)) {
    while (is.call(e) && is.symbol(e[[1L]]) &&
           as.character(e[[1L]]) %in% c("%>%", "%ptrPipeNative%", "|>")) {
      e <- e[[3L]]
    }
    if (is.call(e)) return(e[[1L]])
  }
  if (!is.null(layer$name)) return(as.name(layer$name))
  rlang::abort("Layer has no extractable head.")
}

pipeline_to_eval_expr <- function(node) {
  if (length(node$stages) == 0L) {
    rlang::abort("Empty pipeline reached eval.")
  }
  if (length(node$stages) == 1L) return(node_to_lang(node$stages[[1L]]))
  acc <- node_to_lang(node$stages[[1L]])
  for (i in seq(2L, length(node$stages))) {
    stage <- node_to_lang(node$stages[[i]])
    acc <- desugar_pipe_to_call(acc, stage)
  }
  acc
}

# `lhs |> rhs(args)` -> `rhs(lhs, args)`. For bare-symbol rhs (`lhs |> f`)
# -> `f(lhs)`. Both pipe ops desugar identically — we honor surface only at
# render time, never at eval time.
desugar_pipe_to_call <- function(lhs, rhs) {
  if (is.call(rhs)) {
    return(as.call(c(list(rhs[[1L]]), list(lhs), as.list(rhs[-1L]))))
  }
  rlang::call2(rhs, lhs)
}

node_to_lang <- function(node) {
  if (!is_ptr_node(node)) return(node)
  if (is_ptr_literal(node)) return(node$expr)
  if (is_ptr_user_expr(node)) return(node$inner)
  if (is_ptr_call(node)) {
    head <- node$fun
    args <- node_list_to_lang(node$args)
    return(as.call(c(list(head), args)))
  }
  if (is_ptr_pipeline(node)) return(pipeline_to_eval_expr(node))
  if (is_ptr_missing(node)) {
    rlang::abort("ptr_missing reached eval; should have been pruned by P9.")
  }
  if (is_ptr_placeholder(node)) {
    rlang::abort(paste0(
      "Unsubstituted placeholder reached eval (id=", node$id %||% "?", ")."
    ))
  }
  rlang::abort(paste0("Unknown node class for eval: ", class(node)[1L]))
}

node_list_to_lang <- function(args) {
  if (length(args) == 0L) return(list())
  out <- vector("list", length(args))
  for (i in seq_along(args)) out[[i]] <- node_to_lang(args[[i]])
  names(out) <- names(args)
  out
}
