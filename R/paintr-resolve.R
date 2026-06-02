# P12 helper — resolve a typed-tree subtree to a data frame.
#
# Substitute placeholders inside the subtree using the current input snapshot,
# prune missings, eval the resulting expression in `eval_env`, and (optionally)
# normalize reserved-word column names. Memoize by post-substitute expression
# text when `cache` is supplied. Eval failures return NULL so the caller can
# leave the snapshot untouched and surface an inline error elsewhere.
#
# `safe_to_remove` / `is_standalone` MUST mirror the render-path prune
# (`ptr_complete_expr_safe`): the prune policy is a single invariant, so this
# upstream-resolution prune has to apply the *same* extended remove-set and
# standalone predicate. Omitting them here silently diverges the two prune
# sites — e.g. a deselected consumer inside `!is.na(ppVar(...))` leaves a
# broken `is.na()` that the render prune drops (via the caller's
# `safe_to_remove`) but this prune kept, erroring on eval and starving every
# downstream consumer's column scope.

ptr_resolve_upstream <- function(subtree,
                                 snapshot = list(),
                                 shared_bindings = list(),
                                 eval_env = parent.frame(),
                                 cache = NULL,
                                 expr_check = TRUE,
                                 normalize_columns = TRUE,
                                 stage_enabled = list(),
                                 safe_to_remove = NULL,
                                 is_standalone = NULL) {
  if (is.null(subtree)) return(NULL)
  if (!is_ptr_node(subtree)) {
    rlang::abort("`subtree` must be a typed AST node or NULL.")
  }

  subtree <- disable_walk(subtree, stage_enabled)

  subbed <- ptr_substitute(
    subtree,
    input_snapshot = snapshot,
    shared_bindings = shared_bindings,
    eval_env = eval_env
  )
  pruned <- ptr_prune(subbed, safe_to_remove = safe_to_remove,
                      is_standalone = is_standalone)
  if (is_ptr_missing(pruned)) return(NULL)

  expr <- node_to_lang(pruned)

  if (!is.null(cache)) {
    key <- paste0(deparse(expr), collapse = "\n")
    if (exists(key, envir = cache, inherits = FALSE)) {
      return(get(key, envir = cache, inherits = FALSE))
    }
  }

  ph_names <- ptr_registry_keywords()
  validate_expr_safety(expr, expr_check = expr_check,
                       placeholder_names = ph_names)

  result <- tryCatch(
    eval(expr, envir = eval_env),
    error = function(e) NULL
  )
  if (is.null(result)) return(NULL)

  if (!is.data.frame(result)) {
    if (is.matrix(result)) {
      result <- tryCatch(as.data.frame(result), error = function(e) NULL)
      if (is.null(result)) return(NULL)
    } else {
      return(NULL)
    }
  }

  if (normalize_columns) {
    result <- ptr_normalize_columns(result)
  }

  if (!is.null(cache)) {
    assign(key, result, envir = cache)
  }

  result
}

# Rename reserved-word columns deterministically (P11.5/P11.6).
# A reserved column gets an underscore suffix; if the candidate already
# exists in the original frame, append a numeric suffix instead.
ptr_normalize_columns <- function(df) {
  if (!is.data.frame(df)) return(df)
  nm <- names(df)
  if (is.null(nm) || !length(nm)) return(df)

  reserved <- c(
    "if", "else", "repeat", "while", "function", "for", "in",
    "next", "break", "TRUE", "FALSE", "NULL", "Inf", "NaN", "NA",
    "NA_integer_", "NA_real_", "NA_complex_", "NA_character_"
  )

  new_nm <- nm
  for (i in seq_along(nm)) {
    if (!nm[i] %in% reserved) next
    candidate <- paste0(nm[i], "_")
    j <- 2L
    while (candidate %in% c(nm, new_nm[seq_len(i - 1L)])) {
      candidate <- paste0(nm[i], "_", j)
      j <- j + 1L
    }
    new_nm[i] <- candidate
  }

  names(df) <- new_nm
  df
}
