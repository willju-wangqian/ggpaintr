

# Recursively rewrite `lhs %>% rhs(args)` into `rhs(lhs, args)`. Used so that
# magrittr-style formulas are evaluated and inspected as ordinary calls without
# requiring magrittr at runtime. Native `|>` is already desugared by R's parser.

# Detect the chain of pipe operators feeding the top-level `ggplot(...)` call in
# `formula_text`. Returns a character vector of operators ("|>" or "%>%") in
# source order from the leftmost link to the operator immediately before
# `ggplot(`. Empty character vector if `ggplot(...)` is not pipe-fed. Used only
# for code-panel display so the rendered code mirrors the user's surface form
# rather than the AST after pipe desugaring.


# Back-compat wrapper: returns the operator immediately preceding `ggplot(`, or
# NULL if none.


# Render a `ggplot(...)` call back into a pipe chain matching the source form.
# `pipe_ops` is the chain returned by `detect_ggplot_pipe_chain` — operators in
# source order from leftmost link to the operator immediately before ggplot.
# Walks `length(pipe_ops)` levels deep into arg-1 nesting (each pipe inserted
# its LHS at position 1 during desugaring), drops that synthetic arg 1 at each
# level, and joins the deparsed pieces with the captured operators. Falls back
# to plain `expr_text` if the chain cannot be walked (e.g. pruning removed the
# nested call).


# Back-compat wrapper used by callers that still pass a single op.


# Walk up `index_path` from a placeholder slot in `layer_expr`, returning the
# function name of the nearest non-operator enclosing call. Used to label
# data-pipeline placeholder controls when the placeholder's direct parameter
# is unnamed (otherwise `{param}` resolves to the generic "this setting").
# Returns NULL if no such call exists.


# Resolve the bare function name from a call's head, handling both bare
# symbols (`filter`) and namespaced refs (`dplyr::filter`, `dplyr:::filter`).
# Returns NULL when the head is something else (anonymous function, etc.).


#' Validate the `shared` Bindings Argument to ptr_server()
#'
#' Accepts `NULL` or a (possibly empty) named list of Shiny reactives. Each
#' name corresponds to a `shared = "<id>"` annotation in the formula; each
#' value is the reactive that supplies that placeholder's input.
#'
#' @param shared User-supplied bindings.
#'
#' @return A named list of reactives (possibly empty).
#' @noRd
ptr_validate_shared_bindings <- function(shared) {
  if (is.null(shared)) return(list())
  if (!is.list(shared)) {
    rlang::abort("`shared` must be a named list of reactives (or NULL).")
  }
  if (length(shared) == 0L) return(list())
  nms <- names(shared)
  if (is.null(nms) || any(!nzchar(nms)) || any(duplicated(nms))) {
    rlang::abort(
      "`shared` must have unique non-empty names; each name must match a `shared = \"<id>\"` annotation in the formula."
    )
  }
  is_reactive <- vapply(shared, shiny::is.reactive, logical(1))
  if (!all(is_reactive)) {
    bad <- nms[!is_reactive]
    rlang::abort(paste0(
      "`shared` values must be Shiny reactives. Non-reactive entries: ",
      paste0("`", bad, "`", collapse = ", "), "."
    ))
  }
  shared
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


#' Curated Default `safe_to_remove` Set
#'
#' Stock ggplot2 names where a zero-argument call after placeholder
#' substitution is provably a no-op or an error and is therefore safe
#' to drop. Extends via the user-supplied `safe_to_remove` argument.
#'
#' @return A character vector of bare function names.
#' @noRd
default_drop_when_empty <- function() {
  c(
    "theme", "labs", "xlab", "ylab", "ggtitle",
    "facet_wrap", "facet_grid", "facet_null",
    "xlim", "ylim", "lims", "expand_limits",
    "guides", "annotate",
    "annotation_custom", "annotation_map", "annotation_raster",
    "aes", "aes_", "aes_q", "aes_string", "vars",
    "element_text", "element_line", "element_rect",
    "element_point", "element_polygon", "element_geom"
  )
}

default_safe_to_remove <- default_drop_when_empty

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
pruneable_operator_names <- c(
  "+", "-", "*", "/", "^", "%%", "%/%", "%*%",
  "<", ">", "<=", ">=", "==", "!=",
  "&", "|", "&&", "||",
  ":", "%in%", "~", "!"
)

is_pruneable_operator_call <- function(x) {
  if (!is.call(x)) return(FALSE)
  head <- x[[1L]]
  if (!is.symbol(head)) return(FALSE)
  as.character(head) %in% pruneable_operator_names
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
#' Under `ptr_app()` both `ns_fn` and `ui_ns_fn` default to `shiny::NS(NULL)`
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
#' @examples
#' ns <- shiny::NS("mod1")
#' ptr_ns_id(ns, "ggplot_3_2")
#' ptr_ns_id(shiny::NS(NULL), "ggplot_3_2")
#' @export
ptr_ns_id <- function(ns_fn, id) {
  ns_fn(id)
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
validate_expr_safety <- function(expr, expr_check = TRUE,
                                 placeholder_names = character()) {
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
      # Placeholder annotation calls (e.g. `num(shared = "x")`) are formula
      # DSL syntax, not user code — args are validated by parse_placeholder_token
      # at parse time, so skip the safety walk here.
      if (length(placeholder_names) > 0L && is.symbol(x[[1]]) &&
          as.character(x[[1]]) %in% placeholder_names) {
        return(invisible(NULL))
      }
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
