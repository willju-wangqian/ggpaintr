#' Determine an Expression's Structural Type
#'
#' Internal helper used while traversing ggplot-like calls.
#'
#' @param x An R expression object.
#'
#' @return A single string describing the expression type.
#' @noRd
expr_type <- function(x) {
  if (rlang::is_syntactic_literal(x)) {
    "constant"
  } else if (is.symbol(x)) {
    "symbol"
  } else if (is.call(x)) {
    "call"
  } else if (is.pairlist(x)) {
    "pairlist"
  } else {
    typeof(x)
  }
}

#' Flatten `+`-Separated Plot Layers
#'
#' @param x An expression or call.
#'
#' @return A nested structure with plot layers separated.
#' @noRd
handle_call_break_sum <- function(x, .depth = 0L, max_depth = 100L) {
  if (rlang::is_call(x, "+")) {
    lapply(x[-1], break_sum, .depth = .depth + 1L, max_depth = max_depth)
  } else {
    x
  }
}

#' Recursively Split Plot Expressions
#'
#' @param x An expression or call.
#'
#' @return A split representation of the expression.
#' @noRd
break_sum <- function(x, .depth = 0L, max_depth = 100L) {
  if (.depth > max_depth) {
    rlang::abort("Formula nesting exceeds maximum depth.")
  }
  switch(
    expr_type(x),
    symbol = x,
    constant = x,
    call = handle_call_break_sum(x, .depth, max_depth),
    pairlist = as.pairlist(lapply(x, break_sum, .depth = .depth + 1L,
                                  max_depth = max_depth))
  )
}

#' Get a Call or Symbol Name
#'
#' @param x An expression object.
#'
#' @return A function or symbol name when available.
#' @noRd
get_fun_names <- function(x) {
  if (rlang::is_call(x)) {
    return(rlang::as_string(x[[1]]))
  }

  if (rlang::is_symbol(x)) {
    return(rlang::as_string(x))
  }

  deparse(x)
}

#' Pluck an Expression by Index Path
#'
#' @param .x An expression-like object.
#' @param index_path An index vector.
#'
#' @return The plucked expression or `NULL`.
#' @noRd
expr_pluck <- function(.x, index_path) {
  if (length(index_path) == 0L) return(.x)
  tryCatch(.x[[index_path]], error = function(e) NULL)
}

#' Replace an Expression by Index Path
#'
#' @param .x An expression-like object.
#' @param index_path An index vector.
#' @param value A replacement value.
#'
#' @return The updated expression object.
#' @noRd
`expr_pluck<-` <- function(.x, index_path, value) {
  if (length(index_path) == 0L) return(value)
  tryCatch(
    .x[[index_path]] <- value,
    error = function(e) {
      rlang::abort(
        paste0(
          "Failed to substitute expression at index path [",
          paste(index_path, collapse = ", "),
          "]: ", e$message
        ),
        parent = e
      )
    }
  )

  .x
}

#' Locate Placeholder Paths Inside an Expression
#'
#' @param x An expression to inspect.
#' @param target Placeholder symbols to detect.
#' @param current_path Internal recursion path.
#' @param result Internal accumulator.
#'
#' @return A list of index paths.
#' @noRd
get_index_path <- function(x,
                           target = c("var", "text", "num", "expr", "upload"),
                           current_path = numeric(),
                           result = list(),
                           max_depth = 100L) {
  if (length(current_path) > max_depth) {
    rlang::abort(paste0(
      "Formula nesting exceeds maximum depth (", max_depth, "). ",
      "Check for excessively nested expressions."
    ))
  }
  # Bare-symbol layer: the whole expression IS the placeholder.
  if (length(current_path) == 0L && is.symbol(x)) {
    if (rlang::as_string(x) %in% target) {
      return(list(integer(0)))
    }
    return(result)
  }
  for (i in seq_along(x)) {
    new_path <- c(current_path, i)
    if (is.call(x[[i]])) {
      result <- get_index_path(x[[i]], target, new_path, result, max_depth)
    } else if (is.symbol(x[[i]])) {
      if (rlang::as_string(x[[i]]) %in% target) {
        result <- c(result, list(new_path))
      }
    }
  }

  result
}

#' Suffix Duplicate Layer Names
#'
#' @param x A character vector of names.
#'
#' @return A character vector with duplicates made unique.
#' @noRd
handle_duplicate_names <- function(x) {
  if (length(unique(x)) == length(x)) return(x)
  counts <- new.env(parent = emptyenv())
  for (i in seq_along(x)) {
    nm <- x[i]
    if (is.null(counts[[nm]])) {
      counts[[nm]] <- 1L
    } else {
      counts[[nm]] <- counts[[nm]] + 1L
      x[i] <- paste0(nm, "-", counts[[nm]])
    }
  }
  x
}

#' Build a Stable Placeholder Id
#'
#' @param index_path An index path.
#' @param func_name A call name.
#'
#' @return A single encoded id string.
#' @noRd
encode_id <- function(index_path, func_name) {
  paste(c(func_name, index_path), collapse = "_")
}

#' Read a Parameter Name from an Expression Path
#'
#' @param .expr A call expression.
#' @param .path An index path.
#'
#' @return The parameter name or `NULL`.
#' @noRd
get_expr_param <- function(.expr, .path) {
  if (length(.path) == 0L) return(NULL)
  if (length(.path) > 1) {
    current_index <- .path[1]
    current_expr <- .expr[[current_index]]
    current_names <- names(current_expr)

    if (is.call(current_expr) && is.null(current_names)) {
      return(get_expr_param(current_expr, .path[-1]))
    }

    if (!is.null(current_names)) {
      return(current_names[.path[2]])
    }
  }

  expr_names <- names(.expr)
  if (!is.null(expr_names)) {
    return(expr_names[.path])
  }

  NULL
}

#' Remove Placeholder Marker Symbols
#'
#' @param .expr An expression object.
#' @param target The placeholder marker symbol.
#' @param current_path Internal recursion path.
#'
#' @return The cleaned expression.
#' @noRd
expr_remove_null <- function(.expr,
                             target = rlang::sym("_NULL_PLACEHOLDER"),
                             current_path = numeric(),
                             max_depth = 100L) {
  if (length(current_path) > max_depth) {
    rlang::abort("Expression nesting exceeds maximum depth.")
  }
  if (length(.expr) == 0L) return(.expr)
  # Bare-symbol layer: nothing to recurse into — drop if it matches the target.
  if (length(current_path) == 0L && is.symbol(.expr)) {
    if (identical(.expr, target)) return(NULL)
    return(.expr)
  }
  for (i in length(.expr):1) {
    new_path <- c(current_path, i)
    if (is.call(.expr[[i]])) {
      .expr[[i]] <- expr_remove_null(.expr[[i]], target, new_path, max_depth)
    } else if (is.symbol(.expr[[i]]) && identical(.expr[[i]], target)) {
      .expr[[i]] <- NULL
    }
  }

  .expr
}

#' Check Whether a Layer Can Stand Alone Without Arguments
#'
#' Layers like `geom_*()` and `stat_*()` can inherit aesthetics from
#' the base `ggplot()` call, so they are valid even with no arguments.
#'
#' @param fn_name A single function name string.
#'
#' @return `TRUE` if the layer can be called with zero arguments.
#' @noRd
ptr_can_stand_alone <- function(fn_name) {
  if (is.null(fn_name)) return(FALSE)
  standalone_prefixes <- c("geom_", "stat_")
  any(startsWith(fn_name, standalone_prefixes))
}

#' Curated Default `safe_to_remove` Set
#'
#' Stock ggplot2 names where a zero-argument call after placeholder
#' substitution is provably a no-op or an error and is therefore safe
#' to drop. Extends via the user-supplied `safe_to_remove` argument.
#'
#' @return A character vector of bare function names.
#' @noRd
default_safe_to_remove <- function() {
  c(
    "theme", "labs", "xlab", "ylab", "ggtitle",
    "facet_wrap", "facet_grid", "facet_null",
    "xlim", "ylim", "lims", "expand_limits",
    "guides", "annotate"
  )
}

#' Validate a User-Supplied `safe_to_remove` Vector
#'
#' Accepts `NULL` and `character(0)` as "no extras". Rejects non-character
#' input, `NA`, empty strings, namespaced entries (`pkg::fn`), and entries
#' that are not valid R names. Returns the validated character vector.
#'
#' @param safe_to_remove The argument supplied by the caller.
#' @param arg The argument label to surface in error messages.
#' @param call The call environment to attach to errors.
#'
#' @return A character vector (possibly empty).
#' @noRd
validate_safe_to_remove <- function(safe_to_remove,
                                    arg = "safe_to_remove",
                                    call = rlang::caller_env()) {
  if (is.null(safe_to_remove)) {
    return(character())
  }
  if (!is.character(safe_to_remove)) {
    rlang::abort(
      cli::format_error(c(
        "{.arg {arg}} must be a character vector.",
        x = "Got {.cls {class(safe_to_remove)[1]}}."
      )),
      call = call
    )
  }
  if (length(safe_to_remove) == 0L) {
    return(character())
  }
  if (anyNA(safe_to_remove)) {
    rlang::abort(
      cli::format_error("{.arg {arg}} must not contain {.val NA}."),
      call = call
    )
  }
  if (any(!nzchar(safe_to_remove))) {
    rlang::abort(
      cli::format_error("{.arg {arg}} must not contain empty strings."),
      call = call
    )
  }
  bad_ns <- grepl("::", safe_to_remove, fixed = TRUE)
  if (any(bad_ns)) {
    rlang::abort(
      cli::format_error(c(
        "{.arg {arg}} entries must be bare function names.",
        x = "Namespaced entries are not supported: {.val {safe_to_remove[bad_ns]}}.",
        i = "Matching is across {.code ::} forms; pass the bare name."
      )),
      call = call
    )
  }
  bad_name <- safe_to_remove != make.names(safe_to_remove)
  if (any(bad_name)) {
    rlang::abort(
      cli::format_error(c(
        "{.arg {arg}} entries must be valid R names.",
        x = "Invalid entries: {.val {safe_to_remove[bad_name]}}."
      )),
      call = call
    )
  }
  safe_to_remove
}

#' Prune Empty-Call Artifacts Left by Placeholder Substitution
#'
#' Single bottom-up walk that combines two jobs the old pipeline did
#' separately: (1) sweep `_NULL_PLACEHOLDER` sentinel symbols out of
#' argument slots, and (2) collapse zero-arg calls whose name is in
#' the curated/user remove set. Names not in the set (third-party
#' helpers like `aes_pcp()`, `pcp_arrange()`, `pcp_theme()`) are kept
#' regardless of how they got empty — being absent from the set is the
#' "we don't know if removal is safe" signal.
#'
#' Subtrees produced by an `expr` placeholder (where `orig` at this
#' path is a bare symbol — i.e. the placeholder name) are honoured
#' verbatim: substitution wins over the remove set, because typing
#' something into an `expr` input is an explicit "keep this" signal.
#' Mechanically this falls out of the `is.call(orig)` guard — when
#' `orig` is not a call (symbol or `NULL`), nothing inside the
#' substituted subtree is flagged.
#'
#' @param post The post-substitution expression.
#' @param orig The pre-substitution expression at the same path.
#' @param remove_set Character vector — curated default plus user
#'   `safe_to_remove`.
#' @param .depth Internal recursion depth.
#' @param max_depth Recursion safety cap.
#'
#' @return Either the cleaned expression, or the `_NULL_PLACEHOLDER`
#'   sentinel signalling the parent should drop this slot.
#' @noRd
prune_empty_substitution_artifacts <- function(post,
                                                orig,
                                                remove_set,
                                                .depth = 0L,
                                                max_depth = 100L) {
  if (.depth > max_depth) {
    rlang::abort("Expression nesting exceeds maximum depth.")
  }
  sentinel <- ptr_missing_expr_symbol()

  if (.depth == 0L) {
    if (is.symbol(post) && identical(post, sentinel)) return(NULL)
    if (!is.call(post)) return(post)
  }
  if (!is.call(post)) return(post)

  if (length(post) > 1L) {
    for (i in 2:length(post)) {
      child <- post[[i]]
      orig_child <- if (is.call(orig) && i <= length(orig)) orig[[i]] else NULL
      if (is.call(child)) {
        post[[i]] <- prune_empty_substitution_artifacts(
          child, orig_child, remove_set, .depth + 1L, max_depth
        )
      }
    }
  }

  if (length(post) > 1L) {
    for (i in length(post):2) {
      slot <- post[[i]]
      if (is.symbol(slot) && identical(slot, sentinel)) {
        post[[i]] <- NULL
      }
    }
  }

  if (.depth > 0L && length(post) == 1L) {
    nm <- rlang::call_name(post)
    if (!is.null(nm) && nm %in% remove_set && is.call(orig)) {
      return(sentinel)
    }
  }

  post
}

#' Remove Empty Non-Standalone Layers from an Expression List
#'
#' Top-level companion to `prune_empty_substitution_artifacts()`. Drop a
#' layer iff (a) it is a zero-arg call, (b) its name is in the curated /
#' user `remove_set`, and (c) it is not a `geom_*`/`stat_*` standalone
#' layer. Names not in `remove_set` (e.g. third-party helpers like
#' `pcp_theme()`) are kept by default — the user can opt specific names
#' in via `safe_to_remove`.
#'
#' Layers whose original entry was a bare symbol (e.g. `+ expr`) are
#' skipped — whatever the user supplied via the `expr` placeholder
#' replaces the entire layer and is honoured verbatim, even if its
#' name is in `remove_set`.
#'
#' @param expr_list A named list of post-substitution layer expressions.
#' @param original_expr_list The pre-substitution layer list.
#' @param remove_set Character vector of names safe to remove.
#'
#' @return The filtered expression list.
#' @noRd
ptr_remove_empty_nonstandalone_layers <- function(expr_list,
                                                  original_expr_list = NULL,
                                                  remove_set = default_safe_to_remove()) {
  for (nn in names(expr_list)) {
    expr <- expr_list[[nn]]
    if (is.null(expr) || nn == "ggplot") next
    # Layer originally a bare symbol (e.g. `+ expr`): substitution wins, keep
    # whatever the user supplied verbatim.
    if (!is.null(original_expr_list) && is.symbol(original_expr_list[[nn]])) next
    if (!(is.call(expr) && length(expr) == 1L)) next

    nm <- rlang::call_name(expr)
    if (is.null(nm)) next
    if (!nm %in% remove_set) next
    if (ptr_can_stand_alone(nm)) next

    if (ptr_verbose()) {
      cli::cli_inform(paste0("Layer ", nm, "() removed (no arguments provided)."))
    }
    expr_list[[nn]] <- NULL
  }
  expr_list
}

#' Drop `NULL` Elements from a List
#'
#' @param x A list or `NULL`.
#'
#' @return A list without `NULL` values, or `NULL`.
#' @noRd
check_remove_null <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- x[!vapply(x, is.null, logical(1))]
  if (length(x) == 0) {
    return(NULL)
  }

  x
}

#' Apply a Namespace Function to a Placeholder Id
#'
#' Resolve the namespaced Shiny input/output id for a placeholder.
#' Custom `bind_ui` callbacks should call this with the namespace function
#' supplied on the placeholder context, never assume `meta$id` is already
#' namespaced.
#'
#' Inside a custom `bind_ui(input, output, metas, context)` use:
#'
#' ```
#' input_id  <- ptr_ns_id(context$ns_fn    %||% shiny::NS(NULL), meta$id)
#' output_id <- ptr_ns_id(context$ui_ns_fn %||% shiny::NS(NULL), meta$id)
#' ```
#'
#' Under [ptr_app()] both `ns_fn` and `ui_ns_fn` default to `shiny::NS(NULL)`
#' so the namespaced id equals `meta$id`. When ggpaintr is embedded inside a
#' Shiny module (any wrapper that supplies a real namespace), they wrap a
#' module namespace and the two ids diverge. Always go through `ptr_ns_id()`
#' rather than building the id string yourself.
#'
#' @param ns_fn A namespace function with signature `character -> character`,
#'   typically `shiny::NS(id)` or `shiny::NS(NULL)`.
#' @param id A single id string (e.g., `meta$id` from a placeholder context).
#'
#' @return The namespaced id string.
#' @seealso [ptr_define_placeholder()], [ptr_resolve_layer_data()].
#' @examples
#' ns <- shiny::NS("mod1")
#' ptr_ns_id(ns, "ggplot_3_2")
#' ptr_ns_id(shiny::NS(NULL), "ggplot_3_2")
#' @export
ptr_ns_id <- function(ns_fn, id) {
  ns_fn(id)
}

#' Rewrite All Placeholder Ids in a `ptr_obj` Through a Namespace Function
#'
#' Applies `ns_fn` to every id stored in `ptr_obj$id_list` and
#' `ptr_obj$placeholder_map`. Normal `ptr_state` construction keeps parsed
#' placeholder ids raw and namespaces them at the UI/server boundary; this
#' helper remains available for focused tests and advanced internal rewrites.
#'
#' @param ptr_obj A `ptr_obj`.
#' @param ns_fn A namespace function `character -> character`.
#'
#' @return A `ptr_obj` with rewritten ids.
#' @noRd
ptr_ns_obj <- function(ptr_obj, ns_fn) {
  # Rewrite id_list: list[layer_name][[j]] = id_string
  ptr_obj$id_list <- lapply(ptr_obj$id_list, function(layer_ids) {
    lapply(layer_ids, function(id) ns_fn(id))
  })

  # Rewrite placeholder_map keys and $id field:
  # list[layer_name][[id]] = {id, keyword, layer_name, param, index_path}
  ptr_obj$placeholder_map <- lapply(ptr_obj$placeholder_map, function(layer_meta) {
    new_meta <- list()
    for (old_id in names(layer_meta)) {
      entry <- layer_meta[[old_id]]
      entry$id <- ns_fn(old_id)
      new_meta[[ns_fn(old_id)]] <- entry
    }
    new_meta
  })

  # Rewrite index_path_list names (they are named by id)
  ptr_obj$index_path_list <- lapply(ptr_obj$index_path_list, function(layer_paths) {
    old_names <- names(layer_paths)
    if (!is.null(old_names)) {
      names(layer_paths) <- vapply(old_names, ns_fn, character(1))
    }
    layer_paths
  })

  # Rewrite checkbox_id_list values through ns_fn; names (layer names) stay intact
  if (!is.null(ptr_obj$checkbox_id_list)) {
    old_names <- names(ptr_obj$checkbox_id_list)
    ptr_obj$checkbox_id_list <- stats::setNames(
      vapply(unname(ptr_obj$checkbox_id_list), ns_fn, character(1)),
      old_names
    )
  }

  ptr_obj
}

#' Check Whether ggpaintr Verbose Mode Is Active
#'
#' @return A single logical value.
#' @noRd
ptr_verbose <- function() {
  ptr_get_setting(ptr_settings$verbose)
}

# Default denylist for expr placeholder safety
unsafe_expr_denylist <- c(
  # system escape
  "system", "system2", "shell", "shell.exec", "pipe",
  # file I/O
  "file.create", "file.remove", "file.rename", "file.copy",
  "file.append", "unlink", "dir.create",
  "readLines", "writeLines", "readRDS", "saveRDS",
  "read.csv", "write.csv", "read.table", "write.table",
  "scan", "cat", "sink", "connection",
  "download.file", "url", "file",
  "writeBin", "readBin", "readChar", "writeChar",
  # deserialization / workspace I/O
  "serialize", "unserialize", "load", "save", "save.image",
  # meta-eval (denylist bypass vectors)
  "eval", "evalq", "parse", "deparse",
  "str2lang", "str2expression",
  "call", "as.call", "quote", "bquote", "as.symbol", "as.name",
  "do.call", "match.fun", "get", "mget", "getFromNamespace",
  "Recall", "sys.call", "match.call",
  # environment / global state mutation
  "<<-", "->>", "makeActiveBinding",
  "assign", "rm", "remove", "attach", "detach",
  "source", "sys.source",
  "library", "require", "loadNamespace",
  "Sys.setenv", "Sys.unsetenv", "options",
  "body<-", "formals<-", "environment<-",
  # dangerous base
  "on.exit", "q", "quit", "stop",
  ".Internal", ".Primitive", ".Call", ".External",
  # native code loading
  "dyn.load", "dyn.unload",
  # process / session blocking
  "Sys.sleep", "readline",
  # system mutation
  "Sys.chmod", "Sys.umask", "Sys.readlink", "Sys.setlocale", "Sys.setFileTime",
  # debugger hooks
  "debug", "debugonce", "undebug", "browser",
  # stack / environment introspection
  "sys.frame", "sys.function", "sys.calls",
  "parent.frame", "parent.env",
  "environment", "new.env", "as.environment",
  "baseenv", "globalenv", "emptyenv",
  "attr", "attributes", "slot",
  "attr<-", "attributes<-",
  # information disclosure
  "Sys.getenv", "Sys.getpid", "Sys.info", "Sys.time",
  "proc.time", "message", "warning", "getwd",
  "normalizePath", "Sys.glob", "list.files", "list.dirs",
  "getAnywhere", "exists", "find", "loadedNamespaces",
  "ls", "objects", "search", "searchpaths",
  "R.home", ".libPaths", ".packages",
  # meta-dispatch & method injection (string-arg bypass vectors)
  "exec", "getExportedValue", "getNativeSymbolInfo",
  "delayedAssign", "trace", "untrace",
  "setClass", "setMethod", "setGeneric", "registerS3method",
  "unlockBinding",
  # delayed / deferred code execution
  "reg.finalizer", "addTaskCallback", "taskCallbackManager",
  "setHook", "packageEvent"
)
lockBinding("unsafe_expr_denylist", environment())

#' Resolve the Effective Check List from an `expr_check` Value
#'
#' @param expr_check A logical or list with `deny_list` / `allow_list`.
#'
#' @return A list with `mode` (`"off"`, `"denylist"`, or `"allowlist"`)
#'   and `fns` (the character vector to check against).
#' @note Passing an empty \code{list()} silently falls back to the default
#'   denylist. Callers should pass \code{TRUE} explicitly for default behaviour.
#' @noRd
resolve_expr_check <- function(expr_check) {
  if (identical(expr_check, FALSE)) {
    return(list(mode = "off", fns = character()))
  }

  if (identical(expr_check, TRUE)) {
    return(list(mode = "denylist", fns = unsafe_expr_denylist))
  }

  if (!is.list(expr_check)) {
    rlang::abort(paste0(
      "expr_check must be TRUE, FALSE, or a list ",
      "with 'deny_list' and/or 'allow_list'."
    ))
  }

  has_allow <- !is.null(expr_check$allow_list)
  has_deny <- !is.null(expr_check$deny_list)

  if (!has_allow && !has_deny) {
    return(list(mode = "denylist", fns = unsafe_expr_denylist))
  }

  if (has_allow) {
    allow <- expr_check$allow_list
    if (has_deny) {
      allow <- setdiff(allow, expr_check$deny_list)
    }
    return(list(mode = "allowlist", fns = allow))
  }

  list(mode = "denylist", fns = expr_check$deny_list)
}

#' Extract a Function Name from an AST Node
#'
#' Returns both the bare name and the namespaced name (if applicable)
#' so callers can check either form.
#'
#' @param fn The first element of a call.
#'
#' @return A character vector of length 1 or 2.
#' @noRd
extract_fn_names <- function(fn) {
  if (is.symbol(fn)) {
    return(as.character(fn))
  }

  if (is.call(fn) && length(fn) == 3L &&
        as.character(fn[[1]]) %in% c("::", ":::")) {
    bare <- as.character(fn[[3]])
    op <- as.character(fn[[1]])
    qualified <- paste0(as.character(fn[[2]]), op, bare)
    return(c(bare, qualified))
  }

  deparse(fn, width.cutoff = 60L)[[1]]
}

#' Validate an Expression Against the Resolved Check List
#'
#' Walks the AST and aborts if any function call violates the
#' active check mode (denylist or allowlist).
#'
#' @param expr A parsed R expression.
#' @param expr_check A logical or list with `deny_list` / `allow_list`.
#'
#' @return Invisible `TRUE` if valid; aborts otherwise.
#' @noRd
validate_expr_safety <- function(expr, expr_check = TRUE) {
  resolved <- resolve_expr_check(expr_check)

  if (resolved$mode == "off") {
    return(invisible(TRUE))
  }

  max_depth <- 100L
  walk_expr <- function(x, .depth = 0L) {
    if (.depth > max_depth) {
      rlang::abort(
        paste0("Expression nesting exceeds maximum depth (", max_depth, "). ",
               "The expression may be too complex or maliciously crafted.")
      )
    }
    if (is.pairlist(x)) {
      for (i in seq_along(x)) {
        walk_expr(x[[i]], .depth = .depth + 1L)
      }
      return(invisible(NULL))
    }
    if (is.character(x) && length(x) == 1L) {
      if (resolved$mode == "denylist" && x %in% resolved$fns) {
        rlang::abort(paste0(
          "expr placeholder: `", x,
          "` is not allowed (found as string literal). ",
          "Set expr_check = FALSE to allow ",
          "arbitrary expressions."
        ))
      }
      if (resolved$mode == "allowlist" && x %in% unsafe_expr_denylist) {
        rlang::abort(paste0(
          "expr placeholder: `", x,
          "` is not allowed (found as string literal). ",
          "Set expr_check = FALSE to allow ",
          "arbitrary expressions."
        ))
      }
      return(invisible(NULL))
    }
    if (is.character(x) && length(x) > 1L) {
      for (el in x) {
        if (resolved$mode == "denylist" && el %in% resolved$fns) {
          rlang::abort(paste0(
            "expr placeholder: `", el,
            "` is not allowed (found in character vector). ",
            "Set expr_check = FALSE to allow ",
            "arbitrary expressions."
          ))
        }
        if (resolved$mode == "allowlist" && el %in% unsafe_expr_denylist) {
          rlang::abort(paste0(
            "expr placeholder: `", el,
            "` is not allowed (found in character vector). ",
            "Set expr_check = FALSE to allow ",
            "arbitrary expressions."
          ))
        }
      }
      return(invisible(NULL))
    }
    if (is.symbol(x)) {
      sym_name <- as.character(x)
      if (resolved$mode == "denylist" && sym_name %in% resolved$fns) {
        rlang::abort(paste0(
          "expr placeholder: `", sym_name,
          "` is not allowed (found as symbol reference). ",
          "Set expr_check = FALSE to allow ",
          "arbitrary expressions."
        ))
      }
      if (resolved$mode == "allowlist" && sym_name %in% unsafe_expr_denylist) {
        rlang::abort(paste0(
          "expr placeholder: `", sym_name,
          "` is not allowed (found as symbol reference). ",
          "Set expr_check = FALSE to allow ",
          "arbitrary expressions."
        ))
      }
      return(invisible(NULL))
    }
    if (is.call(x)) {
      # Compound-head strategy (e.g. `(system)("ls")`, `base::eval(expr)`):
      # Two-phase check — (1) recurse into the head sub-expression so the
      # walker catches denied symbols inside it (e.g. `system` inside `(system)`),
      # then (2) run extract_fn_names + denylist/allowlist on the outer head.
      # Phase 2 is belt-and-suspenders for denylist mode (deparsed compound
      # heads won't match real denylist entries, but the check is cheap).
      # In allowlist mode, phase 2 blocks compound heads whose deparsed name
      # isn't in the allowlist — this is intentionally restrictive.
      if (is.call(x[[1]])) {
        walk_expr(x[[1]], .depth = .depth + 1L)
      }

      fn_names <- extract_fn_names(x[[1]])

      if (resolved$mode == "denylist") {
        blocked <- fn_names[fn_names %in% resolved$fns]
        if (length(blocked) > 0) {
          rlang::abort(paste0(
            "expr placeholder: `", blocked[[1]],
            "` is not allowed. ",
            "Set expr_check = FALSE to allow ",
            "arbitrary expressions."
          ))
        }
      } else {
        if (length(fn_names) > 0 && !any(fn_names %in% resolved$fns)) {
          rlang::abort(paste0(
            "expr placeholder: `", fn_names[[1]],
            "` is not in the allowlist. ",
            "Set expr_check = FALSE to allow ",
            "arbitrary expressions."
          ))
        }
      }

      for (i in seq_along(x)[-1]) walk_expr(x[[i]], .depth = .depth + 1L)
    }
  }
  walk_expr(expr)
  invisible(TRUE)
}
