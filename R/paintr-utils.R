

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
#' Bare function names whose zero-argument call after placeholder
#' substitution is provably a no-op, returns a meaningless empty
#' tibble, or errors — and is therefore safe to drop. Curated from
#' ggplot2 plus eight tidyverse packages by empirically testing every
#' exported function with `f()` and `df %>% f()` on R 4.5.3 and keeping
#' only those whose zero-arg behaviour is unambiguously meaningless.
#' Names with a useful zero-arg API (e.g. `n()`, `today()`, `count()`,
#' `tally()`, `cur_*()`) are deliberately excluded. Extends via the
#' user-supplied `safe_to_remove` argument.
#'
#' @return A character vector of bare function names.
#' @noRd
default_drop_when_empty <- function() {
  c(
    ggplot2_drop_names(),
    dplyr_drop_names(),
    tidyr_drop_names(),
    tibble_drop_names(),
    pillar_drop_names(),
    purrr_drop_names(),
    stringr_drop_names(),
    forcats_drop_names(),
    lubridate_drop_names(),
    hms_drop_names()
  )
}

# ggplot2
ggplot2_drop_names <- function() {
  c(
    "ggplot2::aes", "ggplot2::aes_", "ggplot2::aes_q", "ggplot2::aes_string",
    "ggplot2::annotate", "ggplot2::annotation_custom", "ggplot2::annotation_map",
    "ggplot2::annotation_raster", "ggplot2::element_geom",
    "ggplot2::element_line", "ggplot2::element_point",
    "ggplot2::element_polygon", "ggplot2::element_rect", "ggplot2::element_text",
    "ggplot2::expand_limits", "ggplot2::facet_grid", "ggplot2::facet_null",
    "ggplot2::facet_wrap", "ggplot2::ggtitle", "ggplot2::guides",
    "ggplot2::labs", "ggplot2::lims", "ggplot2::theme", "ggplot2::xlab",
    "ggplot2::xlim", "ggplot2::ylab", "ggplot2::ylim"
  )
}

# dplyr
dplyr_drop_names <- function() {
  c(
    "dplyr::across", "dplyr::all_equal", "dplyr::anti_join", "dplyr::arrange",
    "dplyr::arrange_all", "dplyr::arrange_at", "dplyr::arrange_if",
    "dplyr::between", "dplyr::c_across", "dplyr::case_match", "dplyr::case_when",
    "dplyr::common_by", "dplyr::copy_to", "dplyr::cross_join", "dplyr::cumall",
    "dplyr::cumany", "dplyr::cummean", "dplyr::desc", "dplyr::distinct_at",
    "dplyr::distinct_if", "dplyr::distinct_prepare", "dplyr::explain",
    "dplyr::filter", "dplyr::filter_all", "dplyr::filter_at", "dplyr::filter_if",
    "dplyr::filter_out", "dplyr::full_join", "dplyr::group_by",
    "dplyr::group_by_all", "dplyr::group_by_at", "dplyr::group_by_if",
    "dplyr::if_all", "dplyr::if_any", "dplyr::if_else", "dplyr::inner_join",
    "dplyr::left_join", "dplyr::mutate", "dplyr::mutate_all", "dplyr::mutate_at",
    "dplyr::mutate_if", "dplyr::na_if", "dplyr::near", "dplyr::nest_join",
    "dplyr::nth", "dplyr::ntile", "dplyr::order_by", "dplyr::pick",
    "dplyr::reframe", "dplyr::relocate", "dplyr::rename", "dplyr::rename_all",
    "dplyr::rename_at", "dplyr::rename_if", "dplyr::rename_with",
    "dplyr::right_join", "dplyr::same_src", "dplyr::sample_n", "dplyr::select",
    "dplyr::select_at", "dplyr::select_if", "dplyr::semi_join",
    "dplyr::show_query", "dplyr::slice", "dplyr::slice_max", "dplyr::slice_min",
    "dplyr::summarise", "dplyr::summarise_all", "dplyr::summarise_at",
    "dplyr::summarise_if", "dplyr::summarize", "dplyr::summarize_all",
    "dplyr::summarize_at", "dplyr::summarize_if", "dplyr::top_frac",
    "dplyr::top_n", "dplyr::transmute", "dplyr::transmute_all",
    "dplyr::transmute_at", "dplyr::transmute_if", "dplyr::union_all",
    "dplyr::vars"
  )
}

# tidyr
tidyr_drop_names <- function() {
  c(
    "tidyr::build_longer_spec", "tidyr::build_wider_spec", "tidyr::chop",
    "tidyr::drop_na_", "tidyr::expand", "tidyr::expand_", "tidyr::extract",
    "tidyr::extract_", "tidyr::fill_", "tidyr::full_seq", "tidyr::gather_",
    "tidyr::hoist", "tidyr::nest_", "tidyr::pivot_longer",
    "tidyr::pivot_longer_spec", "tidyr::pivot_wider", "tidyr::pivot_wider_spec",
    "tidyr::separate", "tidyr::separate_", "tidyr::separate_longer_delim",
    "tidyr::separate_longer_position", "tidyr::separate_rows_",
    "tidyr::separate_wider_delim", "tidyr::separate_wider_position",
    "tidyr::separate_wider_regex", "tidyr::spread", "tidyr::spread_",
    "tidyr::tidyr_legacy", "tidyr::unchop", "tidyr::uncount", "tidyr::unite",
    "tidyr::unite_", "tidyr::unnest_auto", "tidyr::unnest_longer",
    "tidyr::unnest_wider", "tidyr::unpack"
  )
}

# tibble
tibble_drop_names <- function() {
  c(
    "tibble::column_to_rownames", "tibble::enframe"
  )
}


# pillar
pillar_drop_names <- function() {
  c(
    "pillar::char", "pillar::num"
  )
}

# purrr
purrr_drop_names <- function() {
  c(
    "purrr::accumulate", "purrr::accumulate2", "purrr::assign_in",
    "purrr::cross2", "purrr::cross3", "purrr::detect", "purrr::detect_index",
    "purrr::discard", "purrr::discard_at", "purrr::every", "purrr::flatten_chr",
    "purrr::flatten_dbl", "purrr::flatten_df", "purrr::flatten_dfr",
    "purrr::flatten_int", "purrr::flatten_lgl", "purrr::flatten_raw",
    "purrr::has_element", "purrr::head_while", "purrr::imap", "purrr::imap_chr",
    "purrr::imap_dbl", "purrr::imap_dfc", "purrr::imap_dfr", "purrr::imap_int",
    "purrr::imap_lgl", "purrr::imap_raw", "purrr::imap_vec", "purrr::imodify",
    "purrr::in_parallel", "purrr::insistently", "purrr::iwalk", "purrr::keep",
    "purrr::keep_at", "purrr::list_c", "purrr::list_cbind",
    "purrr::list_flatten", "purrr::list_rbind", "purrr::list_simplify",
    "purrr::list_transpose", "purrr::lmap", "purrr::lmap_at", "purrr::lmap_if",
    "purrr::map", "purrr::map_at", "purrr::map_chr", "purrr::map_dbl",
    "purrr::map_depth", "purrr::map_df", "purrr::map_dfc", "purrr::map_dfr",
    "purrr::map_if", "purrr::map_int", "purrr::map_lgl", "purrr::map_raw",
    "purrr::map_vec", "purrr::map2", "purrr::map2_chr", "purrr::map2_dbl",
    "purrr::map2_df", "purrr::map2_dfc", "purrr::map2_dfr", "purrr::map2_int",
    "purrr::map2_lgl", "purrr::map2_raw", "purrr::map2_vec", "purrr::modify",
    "purrr::modify_at", "purrr::modify_depth", "purrr::modify_if",
    "purrr::modify_in", "purrr::modify2", "purrr::negate", "purrr::none",
    "purrr::partial", "purrr::pmap", "purrr::pmap_chr", "purrr::pmap_dbl",
    "purrr::pmap_df", "purrr::pmap_dfc", "purrr::pmap_dfr", "purrr::pmap_int",
    "purrr::pmap_lgl", "purrr::pmap_raw", "purrr::pmap_vec", "purrr::possibly",
    "purrr::prepend", "purrr::pwalk", "purrr::quietly", "purrr::rate_reset",
    "purrr::rate_sleep", "purrr::rdunif", "purrr::reduce", "purrr::reduce2",
    "purrr::rerun", "purrr::safely", "purrr::slowly", "purrr::some",
    "purrr::tail_while", "purrr::walk", "purrr::walk2"
  )
}

# stringr
stringr_drop_names <- function() {
  c(
    "stringr::invert_match", "stringr::str_conv", "stringr::str_detect",
    "stringr::str_dup", "stringr::str_ends", "stringr::str_equal",
    "stringr::str_extract", "stringr::str_extract_all", "stringr::str_ilike",
    "stringr::str_interp", "stringr::str_like", "stringr::str_locate",
    "stringr::str_locate_all", "stringr::str_match", "stringr::str_match_all",
    "stringr::str_pad", "stringr::str_remove", "stringr::str_remove_all",
    "stringr::str_replace", "stringr::str_replace_all", "stringr::str_split",
    "stringr::str_split_1", "stringr::str_split_fixed", "stringr::str_split_i",
    "stringr::str_starts", "stringr::str_subset", "stringr::str_to_camel",
    "stringr::str_to_kebab", "stringr::str_to_snake", "stringr::str_trunc",
    "stringr::str_which"
  )
}

# forcats
forcats_drop_names <- function() {
  c(
    "forcats::fct_anon", "forcats::fct_collapse", "forcats::fct_count",
    "forcats::fct_drop", "forcats::fct_expand", "forcats::fct_explicit_na",
    "forcats::fct_infreq", "forcats::fct_inorder", "forcats::fct_inseq",
    "forcats::fct_lump", "forcats::fct_lump_lowfreq", "forcats::fct_lump_min",
    "forcats::fct_lump_n", "forcats::fct_lump_prop", "forcats::fct_match",
    "forcats::fct_na_level_to_value", "forcats::fct_na_value_to_level",
    "forcats::fct_other", "forcats::fct_recode", "forcats::fct_relabel",
    "forcats::fct_relevel", "forcats::fct_reorder", "forcats::fct_reorder2",
    "forcats::fct_rev", "forcats::fct_shift", "forcats::fct_shuffle",
    "forcats::fct_unify", "forcats::fct_unique", "forcats::first2",
    "forcats::last2", "forcats::lvls_expand", "forcats::lvls_reorder",
    "forcats::lvls_revalue", "forcats::lvls_union"
  )
}

# lubridate
lubridate_drop_names <- function() {
  c(
    "lubridate::%--%", "lubridate::add_with_rollback", "lubridate::am",
    "lubridate::ceiling_date", "lubridate::cyclic_encoding",
    "lubridate::date_decimal", "lubridate::day", "lubridate::days_in_month",
    "lubridate::decimal_date", "lubridate::dst", "lubridate::epiweek",
    "lubridate::epiyear", "lubridate::fast_strptime",
    "lubridate::fit_to_timeline", "lubridate::floor_date",
    "lubridate::force_tzs", "lubridate::hour", "lubridate::int_aligns",
    "lubridate::int_diff", "lubridate::int_end", "lubridate::int_flip",
    "lubridate::int_overlaps", "lubridate::int_shift",
    "lubridate::int_standardize", "lubridate::int_start", "lubridate::isoweek",
    "lubridate::isoyear", "lubridate::leap_year", "lubridate::mday",
    "lubridate::minute", "lubridate::month", "lubridate::period_to_seconds",
    "lubridate::pm", "lubridate::pretty_dates", "lubridate::qday",
    "lubridate::quarter", "lubridate::reclass_date", "lubridate::rollback",
    "lubridate::rollbackward", "lubridate::rollforward", "lubridate::round_date",
    "lubridate::second", "lubridate::seconds_to_period", "lubridate::semester",
    "lubridate::stamp", "lubridate::stamp_date", "lubridate::stamp_time",
    "lubridate::wday", "lubridate::week", "lubridate::yday", "lubridate::year"
  )
}

# hms
hms_drop_names <- function() {
  c(
    "hms::ceiling_hms", "hms::floor_hms", "hms::round_hms"
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
  parts <- strsplit(safe_to_remove, "::", fixed = TRUE)
  bad <- vapply(parts, function(p) {
    if (length(p) == 1L) {
      return(p != make.names(p) || !nzchar(p))
    }
    if (length(p) == 2L) {
      return(any(!nzchar(p)) || any(p != make.names(p)))
    }
    TRUE
  }, logical(1))
  if (any(bad)) {
    rlang::abort(
      cli::format_error(c(
        "{.arg {arg}} entries must be valid R names or {.code pkg::name} pairs.",
        x = "Invalid entries: {.val {safe_to_remove[bad]}}."
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
